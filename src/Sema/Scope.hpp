#pragma once
class Decl;

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
using namespace llvm;
class Scope {
  Scope *Parent;
  StringMap<Decl *> Symbols;

public:
  Scope(Scope *Parent = nullptr) : Parent(Parent) {}

  bool insert(Decl *Declaration);
  Decl *lookup(StringRef Name);

  Scope *getParent() { return Parent; }
};