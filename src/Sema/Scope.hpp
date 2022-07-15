#pragma once
#include <vector>
class Decl;
class Stmt;
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
using namespace llvm;
class Scope {
  public:
  Scope *Parent;
  StringMap<Decl *> Symbols;
  unsigned Depth;
  std::vector<Stmt *> &stmts; 
  Decl* P_Decl;
  Scope(Decl* P_Decl,std::vector<Stmt *> &stmts,Scope *Parent = nullptr) : Parent(Parent),Depth(0),P_Decl(P_Decl),stmts(stmts) {
    if(Parent){
      Depth = Parent->Depth + 1;
    }
  }
  bool insert(Decl *Declaration);
  Decl *lookup(StringRef Name);
  std::vector<std::pair<Decl *,int>> fuzzy_search(StringRef Name);
  Scope *getParent() { return Parent; }
  Scope *getScopeAtDepth(unsigned D){
    auto *Ret = this;
    while (Ret->Depth != D) {
      Ret = Ret->getParent();
    }
    return Ret;
  }
};