#include "Code_Genrator.hpp"
#include "CGCompileUnit.hpp"
CodeGenerator* CodeGenerator::create(llvm::LLVMContext& Ctx,orc::KaleidoscopeJIT &JIT)
{
    return new CodeGenerator(Ctx,JIT);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(CompileUnitDeclaration* Mod, std::string FileName) {
    std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>("my cool jit", Ctx);
    M->setDataLayout(JIT.getDataLayout());
    CGCompileUnit CGM(M.get());
    CGM.run(Mod);
    return M;
}