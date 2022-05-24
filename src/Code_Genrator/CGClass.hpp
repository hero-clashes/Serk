#pragma once
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include <vector>
#include "AST/AST.hpp"
#include "CGCompileUnit.hpp"
class CGClass{
    public:
  CGCompileUnit &CGM;
  ClassDeclaration *Class;
  std::vector<VariableDeclaration*> Members;
  StructType *Type;
    CGClass(CGCompileUnit &CGM)
      : CGM(CGM){};
  StructType *run(ClassDeclaration *Class);
  void run(){}    
};  