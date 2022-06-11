#include "CGFunction.hpp"
#include "llvm/IR/Verifier.h"
#include <string>
#include "CGClass.hpp"

void CGFunction::run(FunctionDeclaration *Proc) {
  this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);

  llvm::BasicBlock *BB = createBasicBlock("entry");
  setCurr(BB);

    size_t Idx = 0;
  if(AggregateReturnType){
    for (auto I = Fn->arg_begin() + 1, E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    // Create mapping FormalParameter -> llvm::Argument
    // for VAR parameters.
    FormalParams[FP] = Arg;
    llvm::Value *Alloca = Builder.CreateAlloca(Arg->getType());
    auto ar = Builder.CreateStore(Arg, Alloca);
    writeLocalVariable(Curr, FP, Alloca);
  }
  } else
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    // Create mapping FormalParameter -> llvm::Argument
    // for VAR parameters.
    FormalParams[FP] = Arg;
    llvm::Value *Alloca = Builder.CreateAlloca(Arg->getType());
    auto ar = Builder.CreateStore(Arg, Alloca);
    writeLocalVariable(Curr, FP, Alloca);
  }

  InitDecls(Proc);

  if(Proc->getName() == "main"){
    Proc->getEnclosingDecl()->getName();
    emit(dyn_cast_or_null<CompileUnitDeclaration>(Proc->getEnclosingDecl())->getStmts());
  }
  auto Block = Proc->getStmts();
  emit(Proc->getStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateRetVoid();
  }
  // // Validate the generated code, checking for consistency.
  // verifyFunction(*Fn);

    // Run the optimizer on the function.
  CGM.FPM->run(*Fn);

  // Fn->print(errs());
}
llvm::FunctionType *CGFunction::createFunctionType(FunctionDeclaration *Proc) {
  llvm::Type *ResultTy = nullptr;
  if (Proc->getRetType()) {
    ResultTy = mapType(Proc->getRetType());
  }
  auto FormalParams = Proc->getFormalParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  if(ResultTy->isAggregateType()){
    AggregateReturnType = true;
    ParamTypes.push_back(ResultTy->getPointerTo());
    ResultTy = CGM.VoidTy;
  }
  for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }
  return llvm::FunctionType::get(ResultTy, ParamTypes,
                                 /* IsVarArgs */ false);
};
llvm::Function *CGFunction::createFunction(FunctionDeclaration *Proc,
                                           llvm::FunctionType *FTy){
  llvm::Function *Fn = llvm::Function::Create(
      Fty, llvm::GlobalValue::ExternalLinkage,
      Proc->getName(), CGM.getModule());
      size_t Idx = 0;
  if(AggregateReturnType){
      for (auto I = Fn->arg_begin() + 1, E = Fn->arg_end(); I != E;
        ++I, ++Idx) {
      llvm::Argument *Arg = I;
      ParameterDeclaration *FP =
          Proc->getFormalParams()[Idx];
      if (FP->IsPassedbyReference()) {
        llvm::AttrBuilder Attr;
        llvm::TypeSize Sz =
            CGM.getModule()->getDataLayout().getTypeStoreSize(
                CGM.convertType(FP->getType()));
        Attr.addDereferenceableAttr(Sz);
        Attr.addAttribute(llvm::Attribute::NoCapture);
        Arg->addAttrs(Attr);
      }
      Arg->setName(FP->getName());
    }
  } else
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    if (FP->IsPassedbyReference()) {
      llvm::AttrBuilder Attr;
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
llvm::Type *CGFunction::mapType(Decl *Decl) {
  if (auto *FP = llvm::dyn_cast<ParameterDeclaration>(Decl)) {
    llvm::Type *Ty = CGM.convertType(FP->getType());
    if (FP->IsPassedbyReference())
      Ty = Ty->getPointerTo();
    return Ty;
  }
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(Decl))
    return CGM.convertType(V->getType());
  
  return CGM.convertType(llvm::cast<TypeDeclaration>(Decl));
};
void CGFunction::emit(const StmtList &Stmts){
  for (auto *S : Stmts) {
    if (auto *Stmt = llvm::dyn_cast<AssignmentStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<FunctionCallStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<IfStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<WhileStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<ReturnStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<ForStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<MethodCallStatement>(S))
      emitStmt(Stmt);
    else
      llvm_unreachable("Unknown statement");
  }
};
void CGFunction::emitStmt(AssignmentStatement *Stmt){
  Current_Var_Decl = nullptr;
  Current_Var_Value = nullptr;
  Designator *Desig = Stmt->getVar();
  auto &Selectors = Desig->getSelectors();
  if (Selectors.empty())
    {
      Current_Var_Decl = Desig->getDecl();
      auto *Val = emitExpr(Stmt->getExpr());
      if (!Val->getType()->isVoidTy()) {
        writeVariable(Curr, Desig->getDecl(), Val);
      }  
    }
  else {
    llvm::SmallVector<llvm::Value *, 4> IdxList;
    // First index for GEP.
    IdxList.push_back(
        llvm::ConstantInt::get(CGM.Int32Ty, 0));
    auto *Base =
        readVariable(Curr, Desig->getDecl(), false);
    // errs() << Selectors.size();
    for (auto I = Selectors.begin(), E = Selectors.end();
         I != E; ++I) {
      if (auto *IdxSel =
              llvm::dyn_cast<IndexSelector>(*I)) {
        IdxList.push_back(emitExpr(IdxSel->getIndex()));
      } else if (auto *FieldSel =
                     llvm::dyn_cast<FieldSelector>(*I)) {
        llvm::Value *V = llvm::ConstantInt::get(
            CGM.Int32Ty, FieldSel->getIndex());
        IdxList.push_back(V);
      } else {
        llvm::report_fatal_error("not implemented");
      }
    }
    if (!IdxList.empty()) {
      if (Base->getType()) {
        // Base->dump();
        // for(auto val:IdxList) val->dump();
        Base = Builder.CreateInBoundsGEP(Base, IdxList);
        Current_Var_Value = Base;
        auto *Val = emitExpr(Stmt->getExpr());
        if(Val->getType()->isVoidTy()) return;
        Builder.CreateStore(Val, Base);
      }
      else {
        llvm::report_fatal_error("should not happen");
      }
    }
  }
};
llvm::Value *CGFunction::emitExpr(Expr *E){
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix =
                 llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Var = llvm::dyn_cast<Designator>(E)) {
    auto *Decl = Var->getDecl();
    llvm::Value *Val = readVariable(Curr, Decl, false);
    // With more languages features in place, here you
    // need to add array and record support.
    auto &Selectors = Var->getSelectors();
    if (Selectors.empty()) {
      return Val;
    } else {
      llvm::SmallVector<llvm::Value *, 4> IdxList;
      // First index for GEP.
      IdxList.push_back(llvm::ConstantInt::get(CGM.Int32Ty, 0));
      
      for (auto I = Selectors.begin(), E = Selectors.end(); I != E; ++I) {
        if (auto *IdxSel = llvm::dyn_cast<IndexSelector>(*I)) {
          IdxList.push_back(emitExpr(IdxSel->getIndex()));
        } else if (auto *FieldSel = llvm::dyn_cast<FieldSelector>(*I)) {
          llvm::Value *V =
              llvm::ConstantInt::get(CGM.Int32Ty, FieldSel->getIndex());
          IdxList.push_back(V);
        } else {
          llvm::report_fatal_error("not implemented");
        }
      }
      Val = Builder.CreateInBoundsGEP(Val, IdxList);
      Val = Builder.CreateLoad(Val);
      return Val;
    }
  } else if (auto *IntLit = llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int32Ty,
                                  IntLit->getValue());
  } else if (auto *BoolLit = llvm::dyn_cast<BooleanLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int1Ty,
                                  BoolLit->getValue());
  } else if (auto *FuncCall = llvm::dyn_cast<FunctionCallExpr>(E)) {
    return emitFunccall(FuncCall);
  } else if (auto *MethCall = llvm::dyn_cast<MethodCallExpr>(E)) {
    return emitMethcall(MethCall);
  } else if (auto *Str = llvm::dyn_cast<String_Literal>(E)) {
    Str->Value.consume_back("\"");
    Str->Value.consume_front("\"");
    return Builder.CreateGlobalStringPtr(Str->Value);
  } else if (auto *Const = llvm::dyn_cast<ConstantAccess>(E)) {
    return emitExpr(Const->getDecl()->getExpr());
  }
  llvm::report_fatal_error("Unsupported expression");

};
llvm::Value *CGFunction::emitFunccall(FunctionCallExpr *E){
   auto *F = CGM.getModule()->getFunction(E->geDecl()->getName());
  std::vector<Value *> ArgsV;
  if(E->getParams().empty() && !F->empty()){
    Value* v = Current_Var_Decl ? readVariable(Curr, Current_Var_Decl,false): Current_Var_Value;
    ArgsV.push_back(v);
  } else
  for(auto expr:E->getParams()){
    ArgsV.push_back(emitExpr(expr));
  };
  return Builder.CreateCall(F, ArgsV, F->getReturnType()->isVoidTy()? "" :"calltmp");
  // llvm::report_fatal_error("not implemented");
};
llvm::Value *CGFunction::emitMethcall(MethodCallExpr *E){
   std::string Method_Name = E->Var->getType()->getName().str() + "_" + E->Function_Name.str();
  auto *F = CGM.getModule()->getFunction(Method_Name);
  auto o = F->arg_size();
  std::vector<Value *> ArgsV;
  ArgsV.push_back(Defs[E->Var]);
  for(auto expr:E->getParams()){
    ArgsV.push_back(emitExpr(expr));
  };
  return Builder.CreateCall(F, ArgsV, "calltmp");
};
void CGFunction::writeLocalVariable(llvm::BasicBlock *BB,
                                     Decl *Decl,
                                     llvm::Value *Val) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<ParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullptr");
  if(Defs.find(Decl) == Defs.end()){
    Defs[Decl] = Val;
  } else
  Builder.CreateStore(Val, Defs[Decl]);
}

llvm::Value *
CGFunction::readLocalVariable(llvm::BasicBlock *BB,
                               Decl *Decl,bool LoadVal = true) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<ParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  auto Val = Defs.find(Decl);
  if (Val != Defs.end())
    return Builder.CreateLoad(mapType(Decl), Val->second);
  // return readLocalVariableRecursive(BB, Decl);
}


void CGFunction::writeVariable(llvm::BasicBlock *BB,
                                Decl *D, llvm::Value *Val) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == Proc)
      writeLocalVariable(BB, D, Val);
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
      writeLocalVariable(BB, D, Val);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}

llvm::Value *CGFunction::readVariable(llvm::BasicBlock *BB,
                                       Decl *D,
                                       bool LoadVal) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == Proc)
      if(LoadVal)
      {
        return readLocalVariable(BB, D);
      }else 
      return Defs[D];
    else if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      auto *Global = CGM.getGlobal(D);
      if (!LoadVal)
        return Global;
      return Builder.CreateLoad(mapType(D), Global);
    } else
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
      return readLocalVariable(BB, D);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}
void CGFunction::emitStmt(FunctionCallStatement *Stmt) {
  auto *F = CGM.getModule()->getFunction(Stmt->getProc()->getName());

  std::vector<Value *> ArgsV;
  int index  =0;
  for(auto expr:Stmt->getParams()){
    if (!Stmt->getProc()->getFormalParams().empty() && Stmt->getProc()->getFormalParams()[index]->IsPassedbyReference()) {
      Value* val;
     auto a =dyn_cast_or_null<Designator>(expr);
     val = Defs[a->getDecl()];
     ArgsV.push_back(val);
    //  val->dump();
    }else
    ArgsV.push_back(emitExpr(expr));
    index++;
  };
   Builder.CreateCall(F, ArgsV);
  // llvm::report_fatal_error("not implemented");
}
void CGFunction::emitStmt(MethodCallStatement *Stmt){
  std::string Method_Name = Stmt->Var->getType()->getName().str() + "_" + Stmt->Function_Name.str();
  auto *F = CGM.getModule()->getFunction(Method_Name);
  auto o = F->arg_size();
  std::vector<Value *> ArgsV;
  ArgsV.push_back(emitExpr(Stmt->Var));
  for(auto expr:Stmt->getParams()){
    ArgsV.push_back(emitExpr(expr));
  };
  Builder.CreateCall(F, ArgsV, F->getReturnType()->isVoidTy()? "" : "calltmp");
  // llvm::report_fatal_error("not implemented");
};
llvm::Value *
CGFunction::emitInfixExpr(InfixExpression *E) {
  llvm::Value *Left = emitExpr(E->getLeft());
  llvm::Value *Right = emitExpr(E->getRight());
  llvm::Value *Result = nullptr;
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    Result = Builder.CreateNSWAdd(Left, Right);
    break;
  case tok::minus:
    Result = Builder.CreateNSWSub(Left, Right);
    break;
  case tok::star:
    Result = Builder.CreateNSWMul(Left, Right);
    break;
  // case tok::kw_DIV:
  //   Result = Builder.CreateSDiv(Left, Right);
  //   break;
  // case tok::kw_MOD:
  //   Result = Builder.CreateSRem(Left, Right);
  //   break;
  case tok::equal_equal:
    Result = Builder.CreateICmpEQ(Left, Right);
    break;
  case tok::not_equal:
    Result = Builder.CreateICmpNE(Left, Right);
    break;
  case tok::less:
    Result = Builder.CreateICmpSLT(Left, Right);
    break;
  case tok::lessequal:
    Result = Builder.CreateICmpSLE(Left, Right);
    break;
  case tok::greater:
    Result = Builder.CreateICmpSGT(Left, Right);
    break;
  case tok::greaterequal:
    Result = Builder.CreateICmpSGE(Left, Right);
    break;
  case tok::And:
    Result = Builder.CreateAnd(Left, Right);
    break;
  case tok::Or:
    Result = Builder.CreateOr(Left, Right);
    break;
  case tok::slash:
    // Divide by real numbers not supported.
    LLVM_FALLTHROUGH;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *
CGFunction::emitPrefixExpr(PrefixExpression *E) {
  llvm::Value *Result = emitExpr(E->getExpr());
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    // Identity - nothing to do.
    break;
  case tok::minus:
    Result = Builder.CreateNeg(Result);
    break;
  case tok::Not:
    Result = Builder.CreateNot(Result);
    break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}
void CGFunction::emitStmt(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    if(AggregateReturnType){
      auto AggregateTypePointer = Fn->getArg(0);
      auto Val_not_Pointer = Builder.CreateLoad(RetVal);
      Builder.CreateStore(Val_not_Pointer, AggregateTypePointer);
      Builder.CreateRetVoid();  
    } else 
      Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}
  void CGFunction::emitStmt(IfStatement *Stmt){
    bool HasElse = Stmt->getElseStmts().size() > 0;

  // Create the required basic blocks.
  llvm::BasicBlock *IfBB = createBasicBlock("if.body");
  llvm::BasicBlock *ElseBB =
      HasElse ? createBasicBlock("else.body") : nullptr;
  llvm::BasicBlock *AfterIfBB =
      createBasicBlock("after.if");

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, IfBB,
                       HasElse ? ElseBB : AfterIfBB);

  setCurr(IfBB);
  emit(Stmt->getIfStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateBr(AfterIfBB);
  }
  

  if (HasElse) {
    setCurr(ElseBB);
    emit(Stmt->getElseStmts());
    if (!Curr->getTerminator()) {
      Builder.CreateBr(AfterIfBB);
    }
    
  }
  setCurr(AfterIfBB);
  };
  void CGFunction::emitStmt(WhileStatement *Stmt) {
    // The basic block for the condition.
    llvm::BasicBlock *WhileCondBB;
    // The basic block for the while body.
    llvm::BasicBlock *WhileBodyBB = createBasicBlock("while.body");
    // The basic block after the while statement.
    llvm::BasicBlock *AfterWhileBB = createBasicBlock("after.while");

    WhileCondBB = createBasicBlock("while.cond");
    Builder.CreateBr(WhileCondBB);
    setCurr(WhileCondBB);


    llvm::Value *Cond = emitExpr(Stmt->getCond());
    Builder.CreateCondBr(Cond, WhileBodyBB, AfterWhileBB);


    setCurr(WhileBodyBB);
    emit(Stmt->getWhileStmts());
    Builder.CreateBr(WhileCondBB);


    setCurr(AfterWhileBB);
  };
  void CGFunction::emitStmt(ForStatement *Stmt) {
    llvm::BasicBlock *ForCondBB;
    llvm::BasicBlock *ForIntailBB = createBasicBlock("For.initail");
    // The basic block for the For body.
    llvm::BasicBlock *ForBodyBB = createBasicBlock("For.body");
    // The basic block after the For statement.
    llvm::BasicBlock *AfterForBB = createBasicBlock("after.For");

    llvm::BasicBlock *StepFor = createBasicBlock("For.Step");

    ForCondBB = createBasicBlock("For.Cond");

    Builder.CreateBr(ForIntailBB);
    setCurr(ForIntailBB);
    emit(Stmt->Start_Val);// emit each statment as the starting point
    Builder.CreateBr(ForCondBB);

    setCurr(ForCondBB);
    llvm::Value *Cond = emitExpr(Stmt->getCond());
    Builder.CreateCondBr(Cond, ForBodyBB, AfterForBB);


    setCurr(ForBodyBB);
    emit(Stmt->Body);
    Builder.CreateBr(StepFor);
  
    setCurr(StepFor);
    emit(Stmt->Step);
    Builder.CreateBr(ForCondBB);

    setCurr(AfterForBB);
  }
  void CGFunction::InitDecls(FunctionDeclaration *Proc){
    for (auto *D : Proc->getDecls()) {
      if (auto *Var = llvm::dyn_cast<VariableDeclaration>(D)) {
        llvm::Type *Ty = mapType(Var);
        // if (Ty->isAggregateType()) {
        llvm::Value *Val = Builder.CreateAlloca(Ty, nullptr, Var->getName());
        // Defs[D] = Val;
        writeLocalVariable(Curr, Var, Val);
        if (auto C = dyn_cast_or_null<ClassDeclaration>(Var->getType())) {
          if(!Var->is_initlezed){
            auto a = C->getName().str() + "_" + "Create_Default";
            auto F = CGM.getModule()->getFunction(a);
            Builder.CreateCall(F, {Val});
          }
        }
        // }
      }
      if (auto *classs = llvm::dyn_cast<ClassDeclaration>(D)) {
        if (classs->is_genric)
          continue;
        CGClass CGC(CGM);
        auto Ty = CGC.run(classs);
        CGM.TypeCache[dyn_cast_or_null<TypeDeclaration>(classs)] = Ty;
      }
    }
  }