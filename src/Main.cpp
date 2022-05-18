#include <fstream>
#include <utility>
#include "Lexer/Lexer.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
#include "Parser/Parser.hpp"
#include "Code_Genrator/Code_Genrator.hpp"
#include "KaleidoscopeJIT.h"
int main() {

  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();


  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
 FileOrErr = llvm::MemoryBuffer::getFile("main.serk");

  
  SourceMgr mgr;

  mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  DiagnosticsEngine dia{mgr};
  Lexer lex{mgr,dia};

  Sema sema{dia};
  Parser parser{lex,sema};

  auto outputast = parser.parse();
  auto TheContext = std::make_unique<LLVMContext>();
  auto JIT = orc::KaleidoscopeJIT::Create();
  auto Genrator = CodeGenerator::create(*TheContext,*JIT->get());
  auto M = Genrator->run(outputast, "main.serk");

  auto RT = JIT->get()->getMainJITDylib().createResourceTracker();

  auto TSM = llvm::orc::ThreadSafeModule(std::move(M), std::move(TheContext));
  auto e = JIT->get()->addModule(std::move(TSM), RT);

  auto ExprSymbol = JIT->get()->lookup("main");
  int (*FP)() = (int (*)())(intptr_t)ExprSymbol->getAddress();
  fmt::print("{}",FP());
  return 0;

}