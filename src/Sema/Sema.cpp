#include "Sema.hpp"
#include "AST/AST.hpp"

void Sema::enterScope(Decl *D) {
  CurrentScope = new Scope(CurrentScope);
  CurrentDecl = D;
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentDecl->getEnclosingDecl();
}

void Sema::initialize(){
    CurrentScope = new Scope();
    CurrentDecl = nullptr;
    IntegerType = new Base_TypeDeclaration(CurrentDecl, SMLoc(), "int");
    BoolType = new Base_TypeDeclaration(CurrentDecl, SMLoc(), "bool");
    CurrentScope->insert(IntegerType);
    CurrentScope->insert(BoolType);

};


FunctionDeclaration *Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name){
FunctionDeclaration *P =
      new FunctionDeclaration(CurrentDecl, Loc, Name);
  if (!CurrentScope->insert(P))
    Diags.report(Loc, diag::err_symbold_declared, Name);
  return P;
};


void Sema::actOnProcedureHeading(
    FunctionDeclaration* ProcDecl, ParamList& Params,
    Decl* RetType) {
    ProcDecl->setFormalParams(Params);
    auto RetTypeDecl =
        dyn_cast_or_null<TypeDeclaration>(RetType);
    if (!RetTypeDecl && RetType)
        Diags.report(RetType->getLocation(),
            diag::err_returntype_must_be_type);
    else
        ProcDecl->setRetType(RetTypeDecl);
}

void Sema::actOnProcedureDeclaration(FunctionDeclaration* ProcDecl, SMLoc Loc, StringRef Name, DeclList& Decls, StmtList& Stmts)
{
    if (Name != ProcDecl->getName()) {
       // Diags.report(Loc, diag::err_proc_identifier_not_equal);
        //Diags.report(ProcDecl->getLocation(),
          //  diag::note_proc_identifier_declaration);
        llvm::errs() << "sad";
    }
    ProcDecl->setDecls(Decls);
    ProcDecl->setStmts(Stmts);
}

TypeDeclaration*   Sema::actOnTypeRefernce(SMLoc Loc, StringRef Name) {
    if (auto D = dyn_cast_or_null<TypeDeclaration>(CurrentScope->lookup(Name))) {
        return D;
    }
    else {
        Diags.report(Loc,diag::err_returntype_must_be_type);
    };
}
ParameterDeclaration* Sema::actOnParmaDecl(SMLoc Loc, StringRef Name, Decl *Type)
{
    auto Type_as = dyn_cast_or_null<TypeDeclaration>(Type);
    auto D = new ParameterDeclaration(CurrentDecl, Loc, Name, Type_as, true);
    if (CurrentScope->insert(D)) {
        //TODO error it out if it didn't get inserted
    }
    return D;
}
VariableDeclaration* Sema::actOnVarDeceleration(SMLoc Loc, StringRef Name, Decl* Type)
{

    assert(CurrentScope && "CurrentScope not set");
    if (TypeDeclaration* Ty = dyn_cast<TypeDeclaration>(Type)) {
        VariableDeclaration* Decl = new VariableDeclaration(
            CurrentDecl, Loc, Name, Ty);
        if (CurrentScope->insert(Decl))
            return Decl;
        else
            Diags.report(Loc, diag::err_symbold_declared, Name);
    }
    return nullptr;
}
void Sema::actOnReturnStatement(StmtList& Stmts, SMLoc Loc, Expr* RetVal)
{
    auto* Proc = dyn_cast<FunctionDeclaration>(CurrentDecl);
    if (Proc->getRetType() && RetVal) {
        if (Proc->getRetType() != RetVal->getType())
            Diags.report(Loc, diag::err_function_and_return_type);
    }

    Stmts.push_back(new ReturnStatement(RetVal));
}
VariableDeclaration* Sema::actOnVarRefernce(SMLoc Loc, StringRef Name)
{
    if (auto D = dyn_cast_or_null<VariableDeclaration>(CurrentScope->lookup(Name))) {
        return D;
    };
    return nullptr;
}
Expr* Sema::actOnDesignator(Decl* D)
{
    if (!D)
        return nullptr;
    if (auto* V = dyn_cast<VariableDeclaration>(D))
        return new Designator(V);
    return nullptr;
}
Expr* Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal)
{
    uint8_t Radix = 10;
    if (Literal.endswith("H")) {
        Literal = Literal.drop_back();
        Radix = 16;
    }
    llvm::APInt Value(64, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(Value, false),
        IntegerType);
}
void Sema::actOnAssignment(StmtList& Stmts, SMLoc Loc, Expr* D, Expr* E)
{
    if (auto Var = dyn_cast<Designator>(D)) {
        if (Var->getType() != E->getType()) {
            //Diags.report(
              //  Loc, diag::err_types_for_operator_not_compatible,
                //tok::getPunctuatorSpelling(tok::equal));
            errs() << "error";
        }
        Stmts.push_back(new AssignmentStatement(Var, E));
    }
    else if (D) {
        // TODO Emit error
    }
}
;


CompileUnitDeclaration *
Sema::actOnCompileUnitDeclaration(SMLoc Loc, StringRef Name) {
  return new CompileUnitDeclaration(CurrentDecl, Loc, Name);
}

void Sema::actOnCompileUnitDeclaration(
    CompileUnitDeclaration *ModDecl, SMLoc Loc, StringRef Name,
    DeclList &Decls, StmtList &Stmts) {
  if (Name != ModDecl->getName()) {
    // Diags.report(Loc,
    //              diag::err_module_identifier_not_equal);
    // Diags.report(ModDecl->getLocation(),
    //              diag::note_module_identifier_declaration);
  }
  ModDecl->setDecls(Decls);
  ModDecl->setStmts(Stmts);
}