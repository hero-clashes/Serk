#pragma once
class Decl;

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
using namespace llvm;
class Scope {
  Scope *Parent;
  StringMap<Decl *> Symbols;
  unsigned Depth;
public:
  Decl* P_Decl;
  Scope(Decl* P_Decl,Scope *Parent = nullptr) : Parent(Parent),Depth(0),P_Decl(P_Decl) {
    if(Parent){
      Depth = Parent->Depth + 1;
    }
  }
  bool insert(Decl *Declaration);
  Decl *lookup(StringRef Name);

  Scope *getParent() { return Parent; }
  Scope *getScopeAtDepth(unsigned D){
    auto *Ret = this;
    while (Ret->Depth != D) {
      Ret = Ret->getParent();
    }
    return Ret;
  }
};