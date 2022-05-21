#pragma once
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "AST/AST.hpp"
#include "CGCompileUnit.hpp"
class CGFunction{
  CGCompileUnit &CGM;
  llvm::IRBuilder<> Builder;

  llvm::BasicBlock *Curr;

  FunctionDeclaration *Proc;
  llvm::FunctionType *Fty;
  llvm::Function *Fn;


  llvm::DenseMap<Decl *, llvm::TrackingVH<llvm::Value>>
        Defs;

  llvm::DenseMap<ParameterDeclaration *,
                 llvm::Argument *>
      FormalParams;
    llvm::FunctionType *
  createFunctionType(FunctionDeclaration *Proc);
  llvm::Function *createFunction(FunctionDeclaration *Proc,
                                 llvm::FunctionType *FTy);
  llvm::Type *mapType(Decl *Decl);


  void writeVariable(llvm::BasicBlock *BB, Decl *Decl,
                     llvm::Value *Val);
  llvm::Value *readVariable(llvm::BasicBlock *BB,
                            Decl *Decl, bool LoadVal = true);
  void writeLocalVariable(llvm::BasicBlock *BB, Decl *Decl,
                          llvm::Value *Val);
  llvm::Value *readLocalVariable(llvm::BasicBlock *BB,
                                 Decl *Decl);
  llvm::Value *
  readLocalVariableRecursive(llvm::BasicBlock *BB,
                             Decl *Decl);


  llvm::Value *emitInfixExpr(InfixExpression *E);
  llvm::Value *emitPrefixExpr(PrefixExpression *E);
  llvm::Value *emitExpr(Expr *E);
  llvm::Value *emitFunccall(FunctionCallExpr *E);
  void emitStmt(AssignmentStatement *Stmt);
  void emitStmt(FunctionCallStatement *Stmt);
  void emitStmt(IfStatement *Stmt);
  void emitStmt(WhileStatement *Stmt);
  void emitStmt(ReturnStatement *Stmt);
  void emit(const StmtList &Stmts);
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
  void run() {};
};