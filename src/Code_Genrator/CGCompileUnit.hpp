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
#include "CGDebugInfo.hpp"
class CGCompileUnit{
  public:
llvm::Module *M;

SourceMgr& mgr;
bool Debug;
std::unique_ptr<CGDebugInfo> DebugInfo;
CompileUnitDeclaration *Mod;

llvm::DenseMap<TypeDeclaration *, llvm::Type *> TypeCache;

public:
  llvm::Type *VoidTy;
  llvm::Type *Int1Ty;
  llvm::Type *Int8Ty;
  llvm::Type *Int8PtrTy;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Constant *Int32Zero;

  // Repository of global objects.
  llvm::DenseMap<Decl *, llvm::GlobalObject *> Globals;
public:
  CGCompileUnit( llvm::Module *M,SourceMgr& mgr,bool Debug);
  void initialize();

  llvm::LLVMContext &getLLVMCtx() {
    return M->getContext();
  }
  CGDebugInfo *getDbgInfo() {
    return DebugInfo.get();
  }
  llvm::Module *getModule() { return M; }
  CompileUnitDeclaration *getModuleDeclaration() { return Mod; }

  llvm::Type *convertType(TypeDeclaration *Ty);
  std::string mangleName(Decl *D);

  llvm::GlobalObject *getGlobal(Decl *);
  std::unique_ptr<llvm::legacy::FunctionPassManager> FPM;
  void run(CompileUnitDeclaration *Mod);
};