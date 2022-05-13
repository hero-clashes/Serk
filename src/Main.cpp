#include <fstream>
#include <utility>
#include "Lexer/Lexer.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
int main() {

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
 FileOrErr = llvm::MemoryBuffer::getFile("main.serk");

  
  SourceMgr mgr;

  mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  DiagnosticsEngine dia{mgr};
  Lexer lex{mgr,dia};
  Token tok;
  lex.next(tok);
  while (tok.isNot(tok::eof)){
  fmt::print("{} {} \n",tok.is(tok::identifier) ? tok.getIdentifier().str(): "",magic_enum::enum_name(tok.getKind()));

  lex.next(tok);
  }

  return 0;

}