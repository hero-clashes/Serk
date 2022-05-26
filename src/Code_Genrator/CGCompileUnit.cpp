#include "CGCompileUnit.hpp"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "CGFunction.hpp"
#include "CGClass.hpp"
CGCompileUnit::CGCompileUnit(llvm::Module *M): M(M) {
  initialize();
    
}

void CGCompileUnit::initialize()
{
    VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  Int32Zero =
      llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
      // Create a new pass manager attached to it.
  FPM =
      std::make_unique<llvm::legacy::FunctionPassManager>(M);

  // Promote allocas to registers.
  FPM->add(createPromoteMemoryToRegisterPass());
  // // Do simple "peephole" optimizations and bit-twiddling optzns.
  // FPM->add(createInstructionCombiningPass());
  // // Reassociate expressions.
  // FPM->add(createReassociatePass());
  // // Eliminate Common SubExpressions.
  // FPM->add(createGVNPass());
  // // Simplify the control flow graph (deleting unreachable blocks, etc).
  // FPM->add(createCFGSimplificationPass());

  FPM->doInitialization();
}

llvm::Type* CGCompileUnit::convertType(TypeDeclaration *Ty)
{
  if (llvm::Type *T = TypeCache[Ty])
    return T;

  if (llvm::isa<Base_TypeDeclaration>(Ty)) {
    if (Ty->getName() == "int")
      return Int64Ty;
    if (Ty->getName() == "bool")
      return Int1Ty;
    if(Ty->getName() == "void")
      return VoidTy;
  } else if(llvm::isa<ClassDeclaration>(Ty)){

  }
  // else if (auto *AliasTy =
  //                llvm::dyn_cast<AliasTypeDeclaration>(Ty)) {
  //   llvm::Type *T = convertType(AliasTy->getType());
  //   return TypeCache[Ty] = T;
  // } else if (auto *ArrayTy =
  //                llvm::dyn_cast<ArrayTypeDeclaration>(Ty)) {
  //   llvm::Type *Component = convertType(ArrayTy->getType());
  //   Expr *Nums = ArrayTy->getNums();
  //   uint64_t NumElements = 5; // TODO Eval Nums
  //   llvm::Type *T =
  //       llvm::ArrayType::get(Component, NumElements);
  //   return TypeCache[Ty] = T;
  // } else if (auto *RecordTy =
  //                llvm ::dyn_cast<RecordTypeDeclaration>(
  //                    Ty)) {
  //   llvm::SmallVector<llvm::Type *, 4> Elements;
  //   for (const auto &F : RecordTy->getFields()) {
  //     Elements.push_back(convertType(F.getType()));
  //   }
  //   llvm::Type *T = llvm::StructType::create(
  //       Elements, RecordTy->getName(), false);
  //   return TypeCache[Ty] = T;
  // }
  llvm::report_fatal_error("Unsupported type");
}

std::string CGCompileUnit::mangleName(Decl *D)
{
    std::string Mangled;
  llvm::SmallString<16> Tmp;
  while (D) {
    llvm::StringRef Name = D->getName();
    Tmp.clear();
    Tmp.append(llvm::itostr(Name.size()));
    Tmp.append(Name);
    Mangled.insert(0, Tmp.c_str());
    D = D->getEnclosingDecl();
  }
  Mangled.insert(0, "_t");
  return Mangled;

}

llvm::GlobalObject* CGCompileUnit::getGlobal(Decl *D)
{
  return Globals[D];
}

void CGCompileUnit::run(CompileUnitDeclaration *Mod)
{
  this->Mod = Mod;
  for (auto *Decl : Mod->getDecls()) {
    if (auto *Var =
            llvm::dyn_cast<VariableDeclaration>(Decl)) {
      // Create global variables
      llvm::GlobalVariable *V = new llvm::GlobalVariable(
          *M, convertType(Var->getType()),
          /*isConstant=*/false,
          llvm::GlobalValue::PrivateLinkage, nullptr,
          mangleName(Var));
      Globals[Var] = V;
    } else if (auto *Proc =
                   llvm::dyn_cast<FunctionDeclaration>(
                       Decl)) {
      CGFunction CGP(*this);
      CGP.run(Proc);
    } else if(auto *Proc =
                   llvm::dyn_cast<ClassDeclaration>(
                       Decl)){
      CGClass CGC(*this);
      auto Ty = CGC.run(Proc);
      TypeCache[dyn_cast_or_null<TypeDeclaration>(Proc)] = Ty;
    }
  }
}
