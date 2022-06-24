#include "CGClass.hpp"
#include "CGMemberFunction.hpp"
StructType *CGClass::run(ClassDeclaration *Class) {
  this->Class = Class;
  if(StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name)) return StructType::getTypeByName(CGM.getLLVMCtx(),Class->Name);
  for (auto *Decl : Class->Decls) {
    if (auto *Var = llvm::dyn_cast<VariableDeclaration>(Decl)) {
        Members.push_back(Var);
    }
  }
  Type = StructType::create(CGM.getLLVMCtx(), Class->getName());

    std::vector<llvm::Type *> bodyTypes;
    for (auto *Member : Members) {
        bodyTypes.push_back(CGM.convertType(Member->getType()));
    }
  Type->setBody(bodyTypes);

  for (auto *Decl : Class->Decls) {
    if (auto *Proc = llvm::dyn_cast<FunctionDeclaration>(Decl)) {
      CGMemberFunction CGP(CGM, *this);
      CGP.run(Proc);
    }
  }
  if(!CGM.getModule()->getFunction(Class->getName().str() + "_" + "Create_Default") && !Class->Stmts.empty()){
    ParamList a;
    DeclList b;
    StmtList s;
    auto F = new FunctionDeclaration(Class,Class->getLocation(), "Create_Default" ,a,nullptr,b,s);
    CGMemberFunction CGP(CGM, *this);
    CGP.run(F);
  }
  return Type;
};