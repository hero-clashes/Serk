#pragma once
#include "Token.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include <unordered_map>

#include "Diag/Diagnostic.hpp"

class KeywordFilter {
  llvm::StringMap<tok::TokenKind> HashTable;

  void addKeyword(StringRef Keyword,
                  tok::TokenKind TokenCode);

public:
  void addKeywords();

  tok::TokenKind getKeyword(
      StringRef Name,
      tok::TokenKind DefaultTokenCode = tok::unknown) {
    auto Result = HashTable.find(Name);
    if (Result != HashTable.end())
      return Result->second;
    return DefaultTokenCode;
  }
};

class Lexer {
  SourceMgr &SrcMgr;
  DiagnosticsEngine &Diags;

  const char *CurPtr;
  StringRef CurBuf;

  /// CurBuffer - This is the current buffer index we're
  /// lexing from as managed by the SourceMgr object.
  unsigned CurBuffer = 0;

  KeywordFilter Keywords;

public:
  Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), Diags(Diags) {
    CurBuffer = SrcMgr.getMainFileID();
    CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
    CurPtr = CurBuf.begin();
    Keywords.addKeywords();
  }

  DiagnosticsEngine &getDiagnostics() const {
    return Diags;
  }

  Lexer includefile(StringRef file,SMLoc loc){
    std::string IncludedFile;
    auto buf = SrcMgr.AddIncludeFile(file.str(), loc, IncludedFile);
    Lexer copy = *this;
    if(buf == 0){
      //TODO error out
    } else {
      
      auto buffer = SrcMgr.getMemoryBuffer(buf);
      copy.CurBuf = buffer->getBuffer();
      copy.CurPtr = copy.CurBuf.begin();
    }
    return copy;
  };
  /// Returns the next token from the input.
  void next(Token &Result);

  /// Gets source code buffer.
  StringRef getBuffer() const { return CurBuf; }
  Token peak(int n);
  unsigned getCurBuffer() {return CurBuffer;};
private:
  void identifier(Token &Result);
  void number(Token &Result);
  void string(Token &Result);
  void comment();

  SMLoc getLoc() { return SMLoc::getFromPointer(CurPtr); }

  void formToken(Token &Result, const char *TokEnd,
                 tok::TokenKind Kind);

};