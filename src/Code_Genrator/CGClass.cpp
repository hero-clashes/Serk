#include "CGClass.hpp"
#include "CGMemberFunction.hpp"
StructType *CGClass::run(ClassDeclaration *Class) {
  this->Class = Class;
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
  if(!CGM.getModule()->getFunction(Class->getName().str() + "_" + "Create_Default")){
    ParamList a;
    DeclList b;
    auto F = new FunctionDeclaration(Class,SMLoc(), "Create_Default" ,a,nullptr,b,Class->Stmts);
    CGMemberFunction CGP(CGM, *this);
    CGP.run(F);
  }
  return Type;
};