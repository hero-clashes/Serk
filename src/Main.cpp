#include <fstream>
#include <utility>
#include "Lexer/Lexer.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
#include "Parser/Parser.hpp"
#include "Code_Genrator/Code_Genrator.hpp"
#include "KaleidoscopeJIT.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/CodeGen/CommandFlags.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::SmallVector<const char *, 256> argv(argv_ + 1,
     argv_ + argc_);
  LLVMInitializeX86TargetInfo();                                         
  LLVMInitializeNativeTarget();
  LLVMInitializeX86TargetMCA();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm::cl::ParseCommandLineOptions(argc_, argv_);

  llvm::codegen::RegisterCodeGenFlags()	;

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
 FileOrErr = llvm::MemoryBuffer::getFile("main.serk");

  
  SourceMgr mgr;

  mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  

  DiagnosticsEngine dia{mgr};
  Lexer lex{mgr,dia};

  Sema sema{dia};
  Parser parser{lex,sema};

  auto outputast = parser.parse();
  if(!outputast || dia.nunErrors() > 0){
    return 0;
  }
  auto TheContext = std::make_unique<LLVMContext>();
  auto JIT = orc::KaleidoscopeJIT::Create();
  auto Genrator = CodeGenerator::create(*TheContext,*JIT->get());
  auto M = Genrator->run(outputast, "main.serk",mgr);

llvm::Triple TargetTriple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
  
  std::string Error;
  auto Targeet = llvm::TargetRegistry::lookupTarget(TargetTriple.getTriple(), Error);
  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Targeet) {
    errs() << Error;
    return 1;
  }
  llvm::TargetOptions Options =
 llvm::codegen::InitTargetOptionsFromCodeGenFlags(TargetTriple);
 std::string CPUStr = llvm::codegen::getCPUStr();
 std::string FeatureStr = llvm::codegen::getFeaturesStr();

  llvm::TargetMachine *TM = Targeet->
 createTargetMachine(
 TargetTriple.getTriple(), CPUStr, FeatureStr,
 Options,
 llvm::Optional<llvm::Reloc::Model>(
 codegen::getRelocModel()));
 legacy::PassManager PM;
 auto Filename = "output.o";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
   auto FileType = CGFT_ObjectFile;

  if (TM->addPassesToEmitFile(PM, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }
  // M->setTargetTriple(TargetTriple.getTriple());


  PM.run(*M);
  // dest.flush();
  // auto RT = JIT->get()->getMainJITDylib().createResourceTracker();

  // auto TSM = llvm::orc::ThreadSafeModule(std::move(M), std::move(TheContext));
  // auto e = JIT->get()->addModule(std::move(TSM), RT);

  // auto ExprSymbol = JIT->get()->lookup("main");
  // int (*FP)() = (int (*)())(intptr_t)ExprSymbol->getAddress();
  // fmt::print("{}",FP());
  dest.close();
  // auto a = "\"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.32.31326\\bin\\Hostx64\\x64\\Link.exe\" output.o /ENTRY:main /DEBUG:FULL";
  std::string a(R"(""C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\bin\Hostx64\x64\link.exe" "/DEBUG:FULL" /LARGEADDRESSAWARE:NO "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\lib\x64" "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\atlmfc\lib\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\ucrt\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\um\x64" "-libpath:C:\Program Files\LLVM\lib\clang\13.0.0\lib\windows" "-nologo" "output.o" A.lib winmm.lib opengl32.lib gdi32.lib user32.lib shell32.lib ole32.lib oleaut32.lib msvcrt.lib")");
 system(a.c_str());
  return 0;

}