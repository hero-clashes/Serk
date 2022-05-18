#include "CGFunction.hpp"

void CGFunction::run(FunctionDeclaration *Proc) {
  this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);

  llvm::BasicBlock *BB = createBasicBlock("entry");
  setCurr(BB);

    size_t Idx = 0;
  auto &Defs = CurrentDef[BB];
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    // Create mapping FormalParameter -> llvm::Argument
    // for VAR parameters.
    FormalParams[FP] = Arg;
    writeLocalVariable(Curr, FP, Arg);
  }

  for (auto *D : Proc->getDecls()) {
    if (auto *Var =
            llvm::dyn_cast<VariableDeclaration>(D)) {
      llvm::Type *Ty = mapType(Var);
      if (Ty->isAggregateType()) {
        llvm::Value *Val = Builder.CreateAlloca(Ty);
        writeLocalVariable(Curr, Var, Val);
      }
    }
  }

  auto Block = Proc->getStmts();
  emit(Proc->getStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateRetVoid();
  }
  sealBlock(Curr);
  Fn->print(errs());
}
llvm::FunctionType *CGFunction::createFunctionType(FunctionDeclaration *Proc) {
  llvm::Type *ResultTy = nullptr;
  if (Proc->getRetType()) {
    ResultTy = mapType(Proc->getRetType());
  }
  auto FormalParams = Proc->getFormalParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
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
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    ParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    if (FP->isVar()) {
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
    if (FP->isVar())
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
    // else if (auto *Stmt = llvm::dyn_cast<IfStatement>(S))
    //   emitStmt(Stmt);
    // else if (auto *Stmt = llvm::dyn_cast<WhileStatement>(S))
    //   emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<ReturnStatement>(S))
      emitStmt(Stmt);
    else
      llvm_unreachable("Unknown statement");
  }
};
void CGFunction::emitStmt(AssignmentStatement *Stmt){
  auto *Val = emitExpr(Stmt->getExpr());
  Designator *Desig = Stmt->getVar();
  auto &Selectors = Desig->getSelectors();
  if (Selectors.empty())
    writeVariable(Curr, Desig->getDecl(), Val);
  // else {
    // llvm::SmallVector<llvm::Value *, 4> IdxList;
    // // First index for GEP.
    // IdxList.push_back(
    //     llvm::ConstantInt::get(CGM.Int32Ty, 0));
    // auto *Base =
    //     readVariable(Curr, Desig->getDecl(), false);
    // for (auto I = Selectors.begin(), E = Selectors.end();
    //      I != E; ++I) {
    //   if (auto *IdxSel =
    //           llvm::dyn_cast<IndexSelector>(*I)) {
    //     IdxList.push_back(emitExpr(IdxSel->getIndex()));
    //   } else if (auto *FieldSel =
    //                  llvm::dyn_cast<FieldSelector>(*I)) {
    //     llvm::Value *V = llvm::ConstantInt::get(
    //         CGM.Int32Ty, FieldSel->getIndex());
    //     IdxList.push_back(V);
    //   } else {
    //     llvm::report_fatal_error("not implemented");
    //   }
    // }
  //   if (!IdxList.empty()) {
  //     if (Base->getType()->isPointerTy()) {
  //       Base = Builder.CreateInBoundsGEP(Base, IdxList);
  //       Builder.CreateStore(Val, Base);
  //     }
  //     else {
  //       llvm::report_fatal_error("should not happen");
  //     }
  //   }
  // }
};
llvm::Value *CGFunction::emitExpr(Expr *E){
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix =
                 llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Var = llvm::dyn_cast<Designator>(E)) {
    auto *Decl = Var->getDecl();
    llvm::Value *Val = readVariable(Curr, Decl);
    // With more languages features in place, here you
    // need to add array and record support.
    // auto &Selectors = Var->getSelectors();
    // for (auto I = Selectors.begin(), E = Selectors.end();
    //      I != E;
    //      /* no increment */) {
    //   if (auto *IdxSel =
    //           llvm::dyn_cast<IndexSelector>(*I)) {
    //     llvm::SmallVector<llvm::Value *, 4> IdxList;
    //     while (I != E) {
    //       if (auto *Sel =
    //               llvm::dyn_cast<IndexSelector>(*I)) {
    //         IdxList.push_back(emitExpr(Sel->getIndex()));
    //         ++I;
    //       } else
    //         break;
    //     }
    //     Val = Builder.CreateInBoundsGEP(Val, IdxList);
    //     Val = Builder.CreateLoad(
    //         Val->getType()->getPointerElementType(), Val);
    //   } else if (auto *FieldSel =
    //                  llvm::dyn_cast<FieldSelector>(*I)) {
    //     llvm::SmallVector<llvm::Value *, 4> IdxList;
    //     while (I != E) {
    //       if (auto *Sel =
    //               llvm::dyn_cast<FieldSelector>(*I)) {
    //         llvm::Value *V = llvm::ConstantInt::get(
    //             CGM.Int64Ty, Sel->getIndex());
    //         IdxList.push_back(V);
    //         ++I;
    //       } else
    //         break;
    //     }
    //     Val = Builder.CreateInBoundsGEP(Val, IdxList);
    //     Val = Builder.CreateLoad(
    //         Val->getType()->getPointerElementType(), Val);
    //   } else if (auto *DerefSel =
    //                  llvm::dyn_cast<DereferenceSelector>(
    //                      *I)) {
    //     Val = Builder.CreateLoad(
    //         Val->getType()->getPointerElementType(), Val);
    //     ++I;
    return Val;
      } else if (auto *IntLit =
                 llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int64Ty,
                                  IntLit->getValue());
  } else if (auto *BoolLit =
                 llvm::dyn_cast<BooleanLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int1Ty,
                                  BoolLit->getValue());}

  llvm::report_fatal_error("Unsupported expression");

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
  CurrentDef[BB].Defs[Decl] = Val;
}

llvm::Value *
CGFunction::readLocalVariable(llvm::BasicBlock *BB,
                               Decl *Decl) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<ParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  auto Val = CurrentDef[BB].Defs.find(Decl);
  if (Val != CurrentDef[BB].Defs.end())
    return Val->second;
  // return readLocalVariableRecursive(BB, Decl);
}

void CGFunction::sealBlock(llvm::BasicBlock *BB) {
  assert(!CurrentDef[BB].Sealed &&
         "Attempt to seal already sealed block");
  // for (auto PhiDecl : CurrentDef[BB].IncompletePhis) {
  //   addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  // }
  CurrentDef[BB].IncompletePhis.clear();
  CurrentDef[BB].Sealed = true;
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
    if (FP->isVar()) {
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
      return readLocalVariable(BB, D);
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
    if (FP->isVar()) {
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
  llvm::report_fatal_error("not implemented");
}
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
  case tok::equal:
    Result = Builder.CreateICmpEQ(Left, Right);
    break;
  // case tok::hash:
  //   Result = Builder.CreateICmpNE(Left, Right);
  //   break;
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
  // case tok::kw_AND:
  //   Result = Builder.CreateAnd(Left, Right);
  //   break;
  // case tok::kw_OR:
  //   Result = Builder.CreateOr(Left, Right);
  //   break;
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
  // case tok::kw_NOT:
  //   Result = Builder.CreateNot(Result);
  //   break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}
void CGFunction::emitStmt(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}