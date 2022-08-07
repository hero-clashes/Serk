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
#include "llvm/Support/FileSystem.h"

#include "llvm/Passes/PassBuilder.h" // New
#include "llvm/Passes/PassPlugin.h" // New
#include "llvm/Analysis/AliasAnalysis.h" // New
#include "llvm/Analysis/TargetTransformInfo.h" // New
#include "Testing.hpp"
#include "llvm/Transforms/Coroutines.h"
#include "llvm/IR/Verifier.h"
static llvm::cl::list<std::string>
    InputFiles(llvm::cl::Positional,
               llvm::cl::desc("<input-files>"));

static cl::opt<bool>
    Debug("g",
             cl::desc("emit Debug info"),
             cl::init(true));


static cl::opt<signed char> OptLevel(
    cl::desc("Setting the optimization level:"),
    cl::ZeroOrMore,
    cl::values(
        clEnumValN(3, "O", "Equivalent to -O3"),
        clEnumValN(0, "O0", "Optimization level 0"),
        clEnumValN(1, "O1", "Optimization level 1"),
        clEnumValN(2, "O2", "Optimization level 2"),
        clEnumValN(3, "O3", "Optimization level 3"),
        clEnumValN(-1, "Os",
                   "Like -O2 with extra optimizations "
                   "for size"),
        clEnumValN(
            -2, "Oz",
            "Like -Os but reduces code size further")),
    cl::init(0));

static cl::opt<bool>
    EmitLLVM("emit-llvm",
             cl::desc("Emit IR code instead of assembler"),
             cl::init(false));
static cl::opt<std::string>
    OutputFile("output",
            cl::desc("file out name"));
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
  LLVMInitializeX86TargetInfo();                                         
  LLVMInitializeNativeTarget();
  LLVMInitializeX86TargetMCA();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  llvm::cl::ParseCommandLineOptions(argc_, argv_, "Serk Compiler");

  llvm::codegen::RegisterCodeGenFlags()	;
  if(InputFiles.empty() && Files_Test.empty()){ 
    InputFiles.push_back("main.serk");}
  else if(InputFiles.empty()){
    for (auto Test:Files_Test) InputFiles.push_back(Test);
  }
  SourceMgr mgr;
  std::vector<std::string> Dirs;
  for (auto File : InputFiles) {
    auto base_filename = new std::string(File.substr(File.find_last_of("/\\") + 1));
    auto new_f = new std::string(File);
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(File);
    Dirs.push_back(File.substr(0,File.size() - base_filename->size()));
    mgr.setIncludeDirs(Dirs);
    mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

    DiagnosticsEngine dia{mgr};
    Sema sema{dia};

    Lexer lex{mgr, dia};

    Parser parser{lex, sema};
    parser.parse(*new_f);
    if (dia.nunErrors() > 0) {
      return 1;
    }
  }
  auto TM = Create_TM();
  auto TheContext = std::make_unique<LLVMContext>();
  auto Genrator = CodeGenerator::create(*TheContext,TM);
  std::vector<std::string> objs;
  for (auto key : imported.keys()) {
    auto val = imported[key];
    if (!val)
      return 1;
    auto M = Genrator->run(val, key.str(), mgr, Debug);
    
    PassBuilder PB(TM);
    LoopAnalysisManager LAM;
    FunctionAnalysisManager FAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;

    // Register the AA manager first so that our version
  // is the one used.
  FAM.registerPass(
      [&] { return PB.buildDefaultAAPipeline(); });

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  PB.registerPipelineStartEPCallback(
      [&PB](ModulePassManager &PM, OptimizationLevel Op){
        // if (auto Err = PB.parsePassPipeline(
        //         PM, "")) {
        //   WithColor::error(errs())
        //       << "Could not parse pipeline "
        //       << "PipelineStartEPPipeline.ArgStr" << ": "
        //       << toString(std::move(Err)) << "\n";
        // }
      });
  
  ModulePassManager MPM;

  StringRef DefaultPass;
    switch (OptLevel) {
    case 0: DefaultPass = "default<O0>"; break;
    case 1: DefaultPass = "default<O1>"; break;
    case 2: DefaultPass = "default<O2>"; break;
    case 3: DefaultPass = "default<O3>"; break;
    case -1: DefaultPass = "default<Os>"; break;
    case -2: DefaultPass = "default<Oz>"; break;
    }
  if (auto Err = PB.parsePassPipeline(
          MPM, DefaultPass)) {
    WithColor::error(errs())
        << toString(std::move(Err)) << "\n";
    return 1;
  }
    legacy::PassManager PM;
    PM.add(createTargetTransformInfoWrapperPass(
      TM->getTargetIRAnalysis()));
    PM.add(createCoroEarlyLegacyPass());
    // PM.add(createCoroSplitLegacyPass());
    // PM.add(createCoroElideLegacyPass());
    PM.add(createCoroCleanupLegacyPass());
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
    objs.push_back(filename);
    auto R = verifyModule(*M,&errs());
    MPM.run(*M, MAM);
    PM.run(*M);

    dest.close();
  }


  std::string a(R"(""C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\bin\Hostx64\x64\link.exe" "/DEBUG:FULL" /LARGEADDRESSAWARE:NO "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\lib\x64" "-libpath:C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\atlmfc\lib\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\ucrt\x64" "-libpath:C:\Program Files (x86)\Windows Kits\10\Lib\10.0.19041.0\um\x64" "-libpath:C:\Program Files\LLVM\lib\clang\13.0.0\lib\windows" "-nologo" A.lib winmm.lib opengl32.lib gdi32.lib user32.lib shell32.lib ole32.lib oleaut32.lib msvcrt.lib")");
  // std::reverse(objs.begin(),objs.end());
  for(auto ob:objs){
    a.append(" " + ob);
  }
  a.append(" /OUT:");
  if(OutputFile.empty())
  {
    auto N = StringRef(InputFiles[0]);
    N.consume_back(".serk");
    a.append((N + ".exe").str());
  }
  else
    a.append(OutputFile);
  system(a.c_str());
  return 0;

}