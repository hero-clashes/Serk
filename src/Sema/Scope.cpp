#include "Scope.hpp"
#include "AST/AST.hpp"
#include <tuple>
#include <utility>
#include <vector>
#include "fuzzer.hpp"
bool Scope::insert(Decl *Declaration) {
  return Symbols
      .insert(std::pair<StringRef, Decl *>(
          Declaration->getName(), Declaration))
      .second;
}

Decl *Scope::lookup(StringRef Name) {
  Scope *S = this;
  while (S) {
    StringMap<Decl *>::const_iterator I =
        S->Symbols.find(Name);
    if (I != S->Symbols.end())
      return I->second;
    S = S->getParent();
  }
  return nullptr;
}
std::vector<std::pair<Decl *,int>> Scope::fuzzy_search(StringRef Name){
  std::vector<std::pair<Decl *,int>> results;
  Scope *S = this;
  rapidfuzz::fuzz::CachedRatio<char> scorer(Name);
  while(S){
    for(auto Key:S->Symbols.keys()){
      double score = scorer.similarity(Key);
      score*= 100;
      results.push_back({S->Symbols[Key],score});
    }
    S = S->getParent();
  }
  return results;
};
