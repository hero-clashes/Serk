#include <algorithm>
#include <fstream>
#include <string>
#include <utility>
#include <vector>
#include "Lexer/Lexer.hpp"
#include "fmt/format.h"
#include "magic_enum.hpp"
#include "Parser/Parser.hpp"
#include "Code_Genrator/Code_Genrator.hpp"
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
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

#include "llvm/Passes/PassBuilder.h" // New
#include "llvm/Passes/PassPlugin.h" // New
#include "llvm/Analysis/AliasAnalysis.h" // New
#include "llvm/Analysis/TargetTransformInfo.h" // New

TargetMachine *Create_TM() {
  llvm::Triple TargetTriple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);

  std::string Error;
  auto Targeet =
      llvm::TargetRegistry::lookupTarget(TargetTriple.getTriple(), Error);
  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Targeet) {
    errs() << Error;
    return nullptr;
  }
  llvm::TargetOptions Options =
      llvm::codegen::InitTargetOptionsFromCodeGenFlags(TargetTriple);
  std::string CPUStr = llvm::codegen::getCPUStr();
  std::string FeatureStr = llvm::codegen::getFeaturesStr();

  llvm::TargetMachine *TM = Targeet->createTargetMachine(
      TargetTriple.getTriple(), CPUStr, FeatureStr, Options,
      llvm::Optional<llvm::Reloc::Model>(codegen::getRelocModel()));
  return TM;
}

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::SmallVector<const char *, 256> argv(argv_ + 1,
     argv_ + argc_);
  LLVMInitializeX86TargetInfo();                                         
  LLVMInitializeNativeTarget();
  LLVMInitializeX86TargetMCA();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm::cl::ParseCommandLineOptions(argc_, argv_, "Serk Compiler");

  llvm::codegen::RegisterCodeGenFlags()	;

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
    FileOrErr = llvm::MemoryBuffer::getFile("main.serk");


  SourceMgr mgr;

  mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());



  DiagnosticsEngine dia{mgr};
  Lexer lex{mgr,dia};

  Sema sema{dia};
  Parser parser{lex,sema};

  parser.parse("main.serk");
  if(dia.nunErrors() > 0){
    return 0;
  }
  auto TM = Create_TM();
  auto TheContext = std::make_unique<LLVMContext>();
  auto Genrator = CodeGenerator::create(*TheContext,TM);
  std::vector<std::string> objs;
  for (auto key : imported.keys()) {
    auto val = imported[key];
    if (!val)
      return 0;
    auto M = Genrator->run(val, key.str(), mgr);
    legacy::PassManager PM;
    std::error_code EC;
    key.consume_back(".serk");
    auto filename = (key + ".obj").str();
    raw_fd_ostream dest(filename, EC, sys::fs::OF_None);

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
    objs.push_back(filename);
    PM.run(*M);

    dest.close();
  }
  // auto M = Genrator->run(outputast, "main.serk",mgr);


  // auto a = "\"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.32.31326\\bin\\Hostx64\\x64\\Link.exe\" output.o /ENTRY:main /DEBUG:FULL";
  std::string a(R"(""C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\bin\Hostx64\x64\link.exe" "/DEBUG:FULL" /LARGEADDRESSAWARE:NO "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\lib\x64" "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\atlmfc\lib\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\ucrt\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\um\x64" "-libpath:C:\Program Files\LLVM\lib\clang\13.0.0\lib\windows" "-nologo" A.lib winmm.lib opengl32.lib gdi32.lib user32.lib shell32.lib ole32.lib oleaut32.lib msvcrt.lib")");
  std::reverse(objs.begin(),objs.end());
  for(auto ob:objs){
    a.append(" " + ob);
  }
  system(a.c_str());
  return 0;

}