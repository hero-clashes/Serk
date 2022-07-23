#include "CGClass.hpp"
#include "CGMemberFunction.hpp"
#include "CGGenratorMemberFunction.hpp"
StructType *CGClass::run_imported(ClassDeclaration *Class) {
  this->Class = Class;
  Type = StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name);
  for (auto *Decl : Class->Decls) {
    if (auto *Proc = llvm::dyn_cast<FunctionDeclaration>(Decl)) {
      CGMemberFunction CGP(CGM, *this);
      CGP.run_imported(Proc);
    }
  }
  return StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name);  
}
StructType *CGClass::run(ClassDeclaration *Class) {
  this->Class = Class;
  if(StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name)) return StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name);
  for (auto *Decl : Class->Decls) {
    if (auto *Var = llvm::dyn_cast<VariableDeclaration>(Decl)) {
        Members.push_back(Var);
    }
  }
  Type = StructType::create(CGM.getLLVMCtx(), Class->getName());
  CGM.TypeCache[dyn_cast<TypeDeclaration>(Class)] = Type;
    std::vector<llvm::Type *> bodyTypes;
    for (auto *Member : Members) {
        bodyTypes.push_back(CGM.convertType(Member->getType()));
    }
  Type->setBody(bodyTypes);

  for (auto *Decl : Class->Decls) {
    if (auto *Proc = llvm::dyn_cast<FunctionDeclaration>(Decl)) {
      if(Proc->Type == FunctionDeclaration::Genrator){
        CGGenratorMemberFunction CGP(CGM, *this);
        CGP.run(Proc);
      }else {
        CGMemberFunction CGP(CGM, *this);
        CGP.run(Proc);
      }
    }
  }
  if(!CGM.getModule()->getFunction(Class->getName().str() + "_" + "Create") && !Class->Stmts.empty()){
    auto n = new std::string(Class->getName().str() + "_" + "Create");
    ParamList a;
    DeclList b;
    StmtList s;
    auto F = new FunctionDeclaration(Class,Class->getLocation(), *n ,a,nullptr,b,Class->Stmts);
    CGMemberFunction CGP(CGM, *this);
    CGP.run(F);
  }
  return Type;
};