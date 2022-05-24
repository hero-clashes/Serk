#pragma once
#include "CGFunction.hpp"
#include "CGClass.hpp"
class CGMemberFunction: public CGFunction{
    public:

CGClass &CGC;
CGMemberFunction(CGCompileUnit &CGM,CGClass &CGC):CGFunction(CGM),CGC(CGC){};
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


    llvm::Function *createFunction(FunctionDeclaration *Proc,
                                 llvm::FunctionType *FTy);

llvm::FunctionType *
  createFunctionType(FunctionDeclaration *Proc);

void run(FunctionDeclaration *Proc);
};