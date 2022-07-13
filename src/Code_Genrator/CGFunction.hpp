#pragma once
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "AST/AST.hpp"
#include "CGCompileUnit.hpp"
class CGFunction{
  public:
  CGCompileUnit &CGM;
  llvm::IRBuilder<> Builder;

  llvm::BasicBlock *Curr;

  FunctionDeclaration *Proc;
  llvm::FunctionType *Fty;
  llvm::Function *Fn;

  bool AggregateReturnType = false;
  Decl *Current_Var_Decl = nullptr;
  llvm::Value *Current_Var_Value = nullptr;
  SMLoc stmt_loc;

  llvm::DenseMap<Decl *, llvm::TrackingVH<llvm::Value>>
        Defs;

  llvm::DenseMap<Decl *,
                 llvm::Argument *>
      FormalParams;
    llvm::FunctionType *
  createFunctionType(FunctionDeclaration *Proc);
  llvm::Function *createFunction(FunctionDeclaration *Proc,
                                 llvm::FunctionType *FTy);
  llvm::Type *mapType(Decl *Decl);


  virtual void writeVariable(llvm::BasicBlock *BB, Decl *Decl,
                     llvm::Value *Val,bool LoadVal = false);
  virtual llvm::Value *readVariable(llvm::BasicBlock *BB,
                            Decl *Decl, bool LoadVal = true);
  virtual void writeLocalVariable(llvm::BasicBlock *BB, Decl *Decl,
                          llvm::Value *Val, bool LoadVal = false);
  virtual llvm::Value *readLocalVariable(llvm::BasicBlock *BB,
                                 Decl *Decl,bool LoadVal);
  llvm::Value *
  readLocalVariableRecursive(llvm::BasicBlock *BB,
                             Decl *Decl);


  llvm::Value *emitInfixExpr(InfixExpression *E);
  llvm::Value *emitPrefixExpr(PrefixExpression *E);
  llvm::Value *emitExpr(Expr *E,bool want_value = true);
  virtual llvm::Value *emitFunccall(FunctionCallExpr *E);
  llvm::Value *emitMethcall(MethodCallExpr *E);
  llvm::Value *emitCast(CastExpr *E);
  void emitStmt(AssignmentStatement *Stmt);
  virtual void emitStmt(FunctionCallStatement *Stmt);
  void emitStmt(MethodCallStatement *Stmt);
  void emitStmt(IfStatement *Stmt);
  void emitStmt(WhileStatement *Stmt);
  void emitStmt(ReturnStatement *Stmt);
  void emitStmt(ForStatement *Stmt);
  void emit(const StmtList &Stmts);

  void InitDecls(FunctionDeclaration *Proc);
  
public:

  void setCurr(llvm::BasicBlock *BB) {
    Curr = BB;
    Builder.SetInsertPoint(Curr);
  }

  llvm::BasicBlock *createBasicBlock(
      const llvm::Twine &Name,
      llvm::BasicBlock *InsertBefore = nullptr) {
    return llvm::BasicBlock::Create(CGM.getLLVMCtx(), Name,
                                    Fn, InsertBefore);
  }


  CGFunction(CGCompileUnit &CGM)
      : CGM(CGM), Builder(CGM.getLLVMCtx()),
        Curr(nullptr){};

  void run(FunctionDeclaration *Proc);
  void run_imported(FunctionDeclaration *Proc);
  void run() {};
};