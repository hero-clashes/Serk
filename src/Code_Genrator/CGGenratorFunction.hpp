#pragma once
#include "CGFunction.hpp"

class CGGenratorFunction : public CGFunction {

  void writeVariable(llvm::BasicBlock *BB, Decl *Decl, llvm::Value *Val);
  llvm::Value *readVariable(llvm::BasicBlock *BB, Decl *Decl,
                            bool LoadVal = true);
  void writeLocalVariable(llvm::BasicBlock *BB, Decl *Decl, llvm::Value *Val);
  llvm::Value *readLocalVariable(llvm::BasicBlock *BB, Decl *Decl, bool);
  llvm::Value *readLocalVariableRecursive(llvm::BasicBlock *BB, Decl *Decl);

  llvm::Function *createFunction(FunctionDeclaration *Proc,
                                 llvm::FunctionType *FTy);

  llvm::FunctionType *createFunctionType(FunctionDeclaration *Proc);

  void run(FunctionDeclaration *Proc);

  llvm::Type *Create_Context_type();
};