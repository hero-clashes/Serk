#include "Code_Genrator.hpp"
#include "CGCompileUnit.hpp"
#include "llvm/CodeGen/CommandFlags.h"
static cl::opt<bool>
    Print_IR("print-ir",
             cl::desc("Print IR"),
             cl::init(true)); 
CodeGenerator* CodeGenerator::create(llvm::LLVMContext& Ctx, llvm::TargetMachine* TM)
{
    return new CodeGenerator(Ctx,TM);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(CompileUnitDeclaration* Mod, std::string FileName,SourceMgr& mgr ,bool Debug) {
    std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>(FileName, Ctx);
    M->addModuleFlag(llvm::Module::Append,"CodeView", 1);
    M->addModuleFlag(llvm::Module::Append,"uwtable", 1);
    M->addModuleFlag(llvm::Module::Append,"Debug Info Version", 3);
    M->setDataLayout(TM->createDataLayout());
    M->setTargetTriple(LLVM_DEFAULT_TARGET_TRIPLE);
    CGCompileUnit CGM(M.get(),mgr,Debug);
    CGM.run(Mod);
    if(Print_IR) M->dump();
    return M;
}