#include "Code_Genrator.hpp"
#include "CGCompileUnit.hpp"
CodeGenerator* CodeGenerator::create(llvm::LLVMContext& Ctx,orc::KaleidoscopeJIT &JIT)
{
    return new CodeGenerator(Ctx,JIT);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(CompileUnitDeclaration* Mod, std::string FileName,SourceMgr& mgr) {
    std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>("my cool jit", Ctx);
    M->addModuleFlag(llvm::Module::Append,"CodeView", 1);
    M->addModuleFlag(llvm::Module::Append,"uwtable", 1);
    M->addModuleFlag(llvm::Module::Append,"Debug Info Version", 3);
    M->setDataLayout(JIT.getDataLayout());
    M->setTargetTriple(LLVM_DEFAULT_TARGET_TRIPLE);
    CGCompileUnit CGM(M.get(),mgr);
    CGM.run(Mod);
    M->dump();
    return M;
}