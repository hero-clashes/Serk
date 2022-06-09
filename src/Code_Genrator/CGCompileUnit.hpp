#pragma once
#include "AST/AST.hpp"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
class CGCompileUnit{
  public:
llvm::Module *M;

CompileUnitDeclaration *Mod;

llvm::DenseMap<TypeDeclaration *, llvm::Type *> TypeCache;

public:
  llvm::Type *VoidTy;
  llvm::Type *Int1Ty;
  llvm::Type *Int8PtrTy;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Constant *Int32Zero;

  // Repository of global objects.
  llvm::DenseMap<Decl *, llvm::GlobalObject *> Globals;
public:
  CGCompileUnit( llvm::Module *M);
  void initialize();

  llvm::LLVMContext &getLLVMCtx() {
    return M->getContext();
  }
  llvm::Module *getModule() { return M; }
  CompileUnitDeclaration *getModuleDeclaration() { return Mod; }

  llvm::Type *convertType(TypeDeclaration *Ty);
  std::string mangleName(Decl *D);

  llvm::GlobalObject *getGlobal(Decl *);
  std::unique_ptr<llvm::legacy::FunctionPassManager> FPM;
  void run(CompileUnitDeclaration *Mod);
};