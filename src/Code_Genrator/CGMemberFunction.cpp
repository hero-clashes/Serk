#include "CGMemberFunction.hpp"


void CGMemberFunction::writeVariable(llvm::BasicBlock *BB,
                                Decl *D, llvm::Value *Val, bool LoadVal) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == CGC.Class)
      writeLocalVariable(BB, D, Val, LoadVal);
    else if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      Builder.CreateStore(Val, CGM.getGlobal(D));
    } else
      llvm::report_fatal_error(
          "Nested procedures not yet supported");
  } else if (auto *FP =
                 llvm::dyn_cast<ParameterDeclaration>(
                     D)) {
    if (FP->IsPassedbyReference()) {
      Builder.CreateStore(Val, FormalParams[FP]);
    } else
      writeLocalVariable(BB, D, Val, LoadVal);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}
void CGMemberFunction::writeLocalVariable(llvm::BasicBlock *BB, Decl *Decl,
                                          llvm::Value *Val, bool LoadVal) {
  assert(BB && "Basic block is nullptr");
//   assert((llvm::isa<VariableDeclaration>(Decl) ||
//           llvm::isa<ParameterDeclaration>(Decl)) &&
//          "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullptr");
  if (Defs.find(Decl) == Defs.end()) {
    if (std::find(CGC.Members.begin(), CGC.Members.end(), (VariableDeclaration*)Decl) !=
        CGC.Members.end()) {
            auto it = std::find(CGC.Members.begin(), CGC.Members.end(), (VariableDeclaration*)Decl);
            int index = std::distance( CGC.Members.begin(), it );
     
            auto real_pointer = Builder.CreateLoad(Defs[CGC.Class]);
            auto pointer = Builder.CreateStructGEP(CGC.Type,real_pointer,index);
            Builder.CreateStore(Val, pointer);
            return;
    }  
    Defs[Decl] = Val;
  } else
    Builder.CreateStore(Val, Defs[Decl]);
}
llvm::Value *CGMemberFunction::readVariable(llvm::BasicBlock *BB,
                                       Decl *D,
                                       bool LoadVal) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    // if (V->getEnclosingDecl() == CGC.Class)
      
    if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      auto *Global = CGM.getGlobal(D);
      if (!LoadVal)
        return Global;
      return Builder.CreateLoad(mapType(D), Global);
    }
    //  if(LoadVal)
    //   {
        return readLocalVariable(BB, D,LoadVal);
      // }else 
      // return Defs[D];
      llvm::report_fatal_error(
          "Nested procedures not yet supported");
  } else if (auto *FP =
                 llvm::dyn_cast<ParameterDeclaration>(
                     D)) {
    if (FP->IsPassedbyReference()) {
      if (!LoadVal)
        return FormalParams[FP];
      return Builder.CreateLoad(
          mapType(FP)->getPointerElementType(),
          FormalParams[FP]);
    } else
      return readLocalVariable(BB, D,LoadVal);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}
llvm::Value *CGMemberFunction::readLocalVariable(llvm::BasicBlock *BB,
                                                 Decl *Decl,bool LoadVal = true) {
  assert(BB && "Basic block is nullptr");
  //   assert((llvm::isa<VariableDeclaration>(Decl) ||
  //           llvm::isa<ParameterDeclaration>(Decl)) &&
  //          "Declaration must be variable or formal parameter");
  auto Val = CGFunction::readLocalVariable(BB, Decl, LoadVal);
  if (!Val) {
    if (std::find(CGC.Members.begin(), CGC.Members.end(), (VariableDeclaration*)Decl) !=
        CGC.Members.end()) {
            auto it = std::find(CGC.Members.begin(), CGC.Members.end(), (VariableDeclaration*)Decl);
            int index = std::distance( CGC.Members.begin(), it );

            auto real_pointer = Builder.CreateLoad(Defs[CGC.Class]);
            auto pointer = Builder.CreateStructGEP(real_pointer,index);
            if(LoadVal)
            return Builder.CreateLoad(mapType(Decl),pointer);
            else
             return pointer;
    }
  }
  // return readLocalVariableRecursive(BB, Decl);
}

llvm::FunctionType *
CGMemberFunction::createFunctionType(FunctionDeclaration *Proc) {
  llvm::Type *ResultTy = nullptr;
  if (Proc->getRetType()) {
    ResultTy = mapType(Proc->getRetType());
  } else {
    ResultTy = Type::getVoidTy(CGM.getLLVMCtx());
  }
  auto FormalParams = Proc->getFormalParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  ParamTypes.push_back(CGC.Type->getPointerTo());
  for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }
  return llvm::FunctionType::get(ResultTy, ParamTypes,
                                 /* IsVarArgs */ false);
};

void CGMemberFunction::run(FunctionDeclaration *Proc) {
  this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  if (CGDebugInfo *Dbg = CGM.getDbgInfo())
    Dbg->emitFunction(Proc, Fn);

  llvm::BasicBlock *BB = createBasicBlock("entry");
  setCurr(BB);

  auto classPointer = Fn->getArg(0);
  llvm::Value *Alloca = Builder.CreateAlloca(classPointer->getType());
  auto ar = Builder.CreateStore(classPointer, Alloca);
  writeLocalVariable(Curr, CGC.Class, Alloca);

  size_t Idx = 0;
  for (auto I = Fn->arg_begin() + 1, E = Fn->arg_end(); I != E; ++I, ++Idx) {
    llvm::Argument *Arg = I;
    
    ParameterDeclaration *FP = Proc->getFormalParams()[Idx];
    // Create mapping FormalParameter -> llvm::Argument
    // for VAR parameters.
    FormalParams[FP] = Arg;
    llvm::Value *Alloca = Builder.CreateAlloca(Arg->getType());
    auto ar = Builder.CreateStore(Arg, Alloca);
    writeLocalVariable(Curr, FP, Alloca);
    if (CGDebugInfo *Dbg = CGM.getDbgInfo())
      
          Dbg->emitParameterVariable(FP, Idx + 1, Arg, BB);
  }

  InitDecls(Proc);

  auto Block = Proc->getStmts();
  if(Proc->getName() == "Create_Default"){
    emit(dyn_cast_or_null<ClassDeclaration>(Proc->getEnclosingDecl())->Stmts);
  }
  emit(Proc->getStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateRetVoid();
  }
  // // Validate the generated code, checking for consistency.
  // verifyFunction(*Fn);

  // Run the optimizer on the function.
  CGM.FPM->run(*Fn);

  // Fn->print(errs());
  if (CGDebugInfo *Dbg = CGM.getDbgInfo())
    Dbg->emitFunctionEnd(Proc, Fn);
}
llvm::Function *CGMemberFunction::createFunction(FunctionDeclaration *Proc,
                                           llvm::FunctionType *FTy){
  llvm::Function *Fn = llvm::Function::Create(
      Fty, llvm::GlobalValue::ExternalLinkage,
        CGC.Class->getName() + "_" + Proc->getName(), CGM.getModule());
      size_t Idx = 0;
  for (auto I = Fn->arg_begin() + 1, E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    if (FP->IsPassedbyReference()) {
      llvm::AttrBuilder Attr(CGM.getLLVMCtx());
      llvm::TypeSize Sz =
          CGM.getModule()->getDataLayout().getTypeStoreSize(
              CGM.convertType(FP->getType()));
      Attr.addDereferenceableAttr(Sz);
      Attr.addAttribute(llvm::Attribute::NoCapture);
      Arg->addAttrs(Attr);
    }
    Arg->setName(FP->getName());
  }
  return Fn;
};