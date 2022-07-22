#include "CGGenratorFunction.hpp"


llvm::FunctionType *
CGGenratorFunction::createFunctionType(FunctionDeclaration *Proc){
    llvm::Type *ResultTy = nullptr;
    ResultTy = CGM.Int8PtrTy;
    auto FormalParams = Proc->getFormalParams();
    llvm::SmallVector<llvm::Type *, 8> ParamTypes;
    for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }
  return llvm::FunctionType::get(ResultTy, ParamTypes,
                                 /* IsVarArgs */ false);
};
void CGGenratorFunction::run(FunctionDeclaration *Proc){
     this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);
  if (CGDebugInfo *Dbg = CGM.getDbgInfo())
    Dbg->emitFunction(Proc, Fn);
  Fn->addFnAttr("coroutine.presplit","0");
  llvm::BasicBlock *BB = createBasicBlock("entry");
  c  = createBasicBlock("cleanup");
  s = createBasicBlock("suspend");
  f_s = createBasicBlock("final_suspend");
  setCurr(BB);
  
  Return_Promise = Constant::getNullValue(CGM.Int8PtrTy);
  auto R_type =mapType(Proc->getRetType());
  if(!R_type->isVoidTy()){
    auto alloc = Builder.CreateAlloca(R_type);
    Return_Promise = Builder.CreatePointerCast(alloc, CGM.Int8PtrTy);

  }
   //creating courntine stuff
  auto ID = Builder.CreateIntrinsic(
      Intrinsic::coro_id,
      {},
      // {CGM.Int32Ty, CGM.Int8PtrTy,
      //  CGM.Int8PtrTy, CGM.Int8PtrTy},
      {llvm::ConstantInt::get(CGM.Int32Ty, 0, true), Return_Promise,
       Constant::getNullValue(CGM.Int8PtrTy),
       Constant::getNullValue(CGM.Int8PtrTy)});
  auto size = Builder.CreateIntrinsic(Intrinsic::coro_size,{CGM.Int64Ty},{});
  auto alloc = Builder.CreateCall(CGM.M->getFunction("malloc"),{size});
  auto hdl = Builder.CreateIntrinsic(
      Intrinsic::coro_begin,
      {}, {ID, alloc});
  setCurr(c);
  auto mem = Builder.CreateIntrinsic(Intrinsic::coro_free,{},{ID,hdl});
  Builder.CreateCall(CGM.M->getFunction("free"),{mem});
  Builder.CreateBr(s);
  setCurr(s);
  Builder.CreateIntrinsic(Intrinsic::coro_end,{},{hdl,ConstantInt::getFalse(CGM.getLLVMCtx())});
  Builder.CreateRet(hdl);

  setCurr(f_s);
  auto v = Builder.CreateIntrinsic(Intrinsic::coro_suspend,{},{ConstantTokenNone::get(CGM.getLLVMCtx()),ConstantInt::getTrue(CGM.getLLVMCtx())});
  auto sw = Builder.CreateSwitch(v,s);
  // auto bb = createBasicBlock("after_yield");
  // sw->addCase(llvm::ConstantInt::get(dyn_cast<IntegerType>(CGM.Int8Ty), 0, true), bb);
  sw->addCase(llvm::ConstantInt::get(dyn_cast<IntegerType>(CGM.Int8Ty), 1, true), c);
  // Builder.CreateBr(s);



  setCurr(BB);
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E; ++I, ++Idx) {
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
  emit(Proc->getStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateBr(f_s);
  }
  // // Validate the generated code, checking for consistency.
  // verifyFunction(*Fn);

  // Run the optimizer on the function.
  CGM.FPM->run(*Fn);

  // Fn->print(errs());
  if (CGDebugInfo *Dbg = CGM.getDbgInfo())
    Dbg->emitFunctionEnd(Proc, Fn);
};
bool CGGenratorFunction::emitSpecialStmt(Stmt *S) { 
  if(auto R = dyn_cast_or_null<ReturnStatement>(S)){
    auto V = emitExpr(R->getRetVal());
    // V->dump();
    auto casted = Builder.CreatePointerCast(Return_Promise,mapType(Proc->getRetType())->getPointerTo());
    Builder.CreateStore(V, casted);
    auto v = Builder.CreateIntrinsic(Intrinsic::coro_suspend,{},{ConstantTokenNone::get(CGM.getLLVMCtx()),ConstantInt::getFalse(CGM.getLLVMCtx())});
    auto sw = Builder.CreateSwitch(v,s);
    auto bb = createBasicBlock("after_yield");
    sw->addCase(llvm::ConstantInt::get(dyn_cast<IntegerType>(CGM.Int8Ty), 0, true), bb);
    sw->addCase(llvm::ConstantInt::get(dyn_cast<IntegerType>(CGM.Int8Ty), 1, true), c);
    if(auto dbg = CGM.getDbgInfo())
            dbg->SetLoc(&Curr->back(),stmt_loc);
    setCurr(bb);
    return true;
  }
  
  return false; }