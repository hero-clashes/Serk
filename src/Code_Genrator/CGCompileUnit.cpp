#include "CGCompileUnit.hpp"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "CGFunction.hpp"
#include "CGClass.hpp"
CGCompileUnit::CGCompileUnit(llvm::Module *M,SourceMgr& mgr): M(M),mgr(mgr) {
  initialize();
    
}

void CGCompileUnit::initialize()
{
    VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMCtx());
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  Int32Zero =
      llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
      // Create a new pass manager attached to it.
  FPM =
      std::make_unique<llvm::legacy::FunctionPassManager>(M);

  // Promote allocas to registers.
  // FPM->add(createPromoteMemoryToRegisterPass());
  // // Do simple "peephole" optimizations and bit-twiddling optzns.
  // FPM->add(createInstructionCombiningPass());
  // // Reassociate expressions.
  // FPM->add(createReassociatePass());
  // // Eliminate Common SubExpressions.
  // FPM->add(createGVNPass());
  // // Simplify the control flow graph (deleting unreachable blocks, etc).
  // FPM->add(createCFGSimplificationPass());

  FPM->doInitialization();
  M->getOrInsertFunction(
  "printf",
  FunctionType::get(
    IntegerType::getInt32Ty(getLLVMCtx()),
    Type::getInt8Ty(getLLVMCtx())->getPointerTo(),
    true /* this is variadic func */
  )
  );
  M->getOrInsertFunction(
    "malloc",
    FunctionType::get(
      Int8PtrTy,
      IntegerType::getInt64Ty(getLLVMCtx()),
      /* has variadic args */ false
    )
  );
   M->getOrInsertFunction(
    "free",
    FunctionType::get(
      VoidTy,
      Int8PtrTy,
      /* has variadic args */ false
    )
  );
}

llvm::Type* CGCompileUnit::convertType(TypeDeclaration *Ty)
{
  if (llvm::Type *T = TypeCache[Ty])
    return T;
  if(Ty->getName() == "void")
      return VoidTy;
  if (auto inttype = dyn_cast_or_null<Integer_TypeDeclaration>(Ty)) {
    if(inttype->getName() == "double")
      return TypeCache[Ty] = llvm::Type::getDoubleTy(getLLVMCtx());
    if(inttype->getName() == "float")
      return TypeCache[Ty] = llvm::Type::getFloatTy(getLLVMCtx());  
    return TypeCache[Ty] = llvm::Type::getIntNTy(getLLVMCtx(),inttype->Size);
  } else if(llvm::isa<ClassDeclaration>(Ty)){

  }
  else if (auto *AliasTy =
                 llvm::dyn_cast<Alias_TypeDeclaration>(Ty)) {
    llvm::Type *T = convertType(AliasTy->Realone);
    return TypeCache[Ty] = T;
  } else if (auto *ArrayTy =
                  llvm::dyn_cast<ArrayTypeDeclaration>(Ty)) {
     llvm::Type *Component = convertType(ArrayTy->getType());
     Expr *Nums = ArrayTy->getNums();
     //auto val = 
     uint64_t NumElements;
     if(auto Const = dyn_cast_or_null<IntegerLiteral>(Nums)){
       NumElements = Const->getValue().getExtValue();
     } else if(auto dec = dyn_cast_or_null<ConstantAccess>(Nums)){
      if(auto ints = dyn_cast_or_null<IntegerLiteral>(dec->getDecl()->E)){
       NumElements = ints->getValue().getExtValue();
      }
     }
     llvm::Type *T =
         llvm::ArrayType::get(Component, NumElements);
     return TypeCache[Ty] = T;
   }  else if (auto *Pointer =
                 llvm::dyn_cast<PointerTypeDeclaration>(Ty)) {
    return convertType(Pointer->getType())->getPointerTo();
   }
   if (auto s =StructType::getTypeByName(getLLVMCtx(),Ty->getName()))
    return s;
    // else if (auto *RecordTy =
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
  if (Debug)
    DebugInfo.reset(new CGDebugInfo(*this));
  for (auto *Inc_M: Mod->Imported_Module) {
    for (auto *Decl : Inc_M->getDecls()) {
      if (auto *Var = llvm::dyn_cast<VariableDeclaration>(Decl)) {
        llvm::GlobalVariable *V = new llvm::GlobalVariable(
            *M, convertType(Var->getType()),
            /*isConstant=*/false, llvm::GlobalValue::ExternalLinkage, nullptr,
            mangleName(Var));
        Globals[Var] = V;
      } else if (auto *Proc = llvm::dyn_cast<FunctionDeclaration>(Decl)) {
        CGFunction CGP(*this);
        CGP.run_imported(Proc);
      } else if (auto *Proc = llvm::dyn_cast<ClassDeclaration>(Decl)) {
        if (Proc->is_genric)
          continue;
        CGClass CGC(*this);
        auto Ty = CGC.run_imported(Proc);
        TypeCache[dyn_cast_or_null<TypeDeclaration>(Proc)] = Ty;
      }
    }
  }
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
      if (CGDebugInfo *Dbg = getDbgInfo())
        Dbg->emitGlobalVariable(Var, V);
    } else if (auto *Proc =
                   llvm::dyn_cast<FunctionDeclaration>(
                       Decl)) {
      CGFunction CGP(*this);
      CGP.run(Proc);
    } else if(auto *Proc =
                   llvm::dyn_cast<ClassDeclaration>(
                       Decl)){
      if(Proc->is_genric) continue;
      CGClass CGC(*this);
      auto Ty = CGC.run(Proc);
      TypeCache[dyn_cast_or_null<TypeDeclaration>(Proc)] = Ty;
    }
  }
  if(auto Dbg = getDbgInfo())
    Dbg->finalize();
}
