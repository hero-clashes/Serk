#pragma once

#include "AST/AST.hpp"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/GlobalVariable.h"
#include <unordered_map>

class CGCompileUnit;

class CGDebugInfo {
  CGCompileUnit &CGM;
  llvm::DIBuilder DBuilder;
  llvm::DICompileUnit *CU;

  llvm::DenseMap<TypeDeclaration *, llvm::DIType *>
      TypeCache;

  llvm::SmallVector<llvm::DIScope *, 4> ScopeStack;
  std::unordered_map<unsigned, DIFile*> Files;
  DIFile* Get_File(SMLoc Loc);
  llvm::DIScope *getScope();
  void openScope(llvm::DIScope *);
  unsigned getLineNumber(SMLoc Loc);

  llvm::DIType *getPervasiveType(Integer_TypeDeclaration *Ty);
  llvm::DIType *getAliasType(Alias_TypeDeclaration *Ty);
  llvm::DIType *getArrayType(ArrayTypeDeclaration *Ty);
  llvm::DIType *getClassType(ClassDeclaration *Ty);
  llvm::DIType *getPointerType(PointerTypeDeclaration *Ty);

  llvm::DIType *getType(TypeDeclaration *Type);
  llvm::DISubroutineType *getType(FunctionDeclaration *P);

public:
  CGDebugInfo(CGCompileUnit &CGM);

  void closeScope();

  void emitGlobalVariable(VariableDeclaration *Decl,
                          llvm::GlobalVariable *V);
  void emitFunction(FunctionDeclaration *Decl,
                     llvm::Function *Fn);
  void emitFunctionEnd(FunctionDeclaration *Decl,
                        llvm::Function *Fn);
  llvm::DILocalVariable *
  emitParameterVariable(ParameterDeclaration *FP,
                        size_t Idx, llvm::Value *Val,
                        llvm::BasicBlock *BB);
  void emitValue(llvm::Value *Val,
                 VariableDeclaration *Var, SMLoc Loc,
                 llvm::BasicBlock *BB);

  llvm::DebugLoc getDebugLoc(SMLoc Loc);
  void SetLoc(llvm::Instruction *Inst,SMLoc Loc);
  void finalize();
  void set_file(CompileUnitDeclaration* M);
};