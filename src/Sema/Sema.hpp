#pragma once
#include "Diag/Diagnostic.hpp"
#include "Scope.hpp"
#include "AST/AST.hpp"

class Sema {
  public:

  void enterScope(Decl *);
  void leaveScope();
  Scope *CurrentScope;
  Decl *CurrentDecl;
  DiagnosticsEngine &Diags;

  TypeDeclaration *IntegerType;
  TypeDeclaration *BoolType;
  Sema(DiagnosticsEngine &Diags)
      : CurrentScope(nullptr), CurrentDecl(nullptr), Diags(Diags) {
    initialize();
  }

  void initialize();

  FunctionDeclaration *actOnFunctionDeclaration(SMLoc Loc, StringRef Name);
  void actOnProcedureHeading(
    FunctionDeclaration *ProcDecl, ParamList &Params,
    Decl *RetType);
  void actOnProcedureDeclaration(
      FunctionDeclaration* ProcDecl, SMLoc Loc,
      StringRef Name, DeclList& Decls, StmtList& Stmts);


  TypeDeclaration* actOnTypeRefernce(SMLoc Loc, StringRef Name);
  ParameterDeclaration* actOnParmaDecl(SMLoc Loc, StringRef Name, Decl*Type);
  VariableDeclaration* actOnVarDeceleration(SMLoc Loc, StringRef Name, Decl* Type);
  void actOnReturnStatement(StmtList& Stmts, SMLoc Loc,
      Expr* RetVal);
  VariableDeclaration* actOnVarRefernce(SMLoc Loc, StringRef Name);
  Expr* actOnDesignator(Decl* D);
  Expr* actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
  void actOnAssignment(StmtList& Stmts, SMLoc Loc, Expr* D,
      Expr* E);

  CompileUnitDeclaration *actOnCompileUnitDeclaration(SMLoc Loc,
                                            StringRef Name);
  void actOnCompileUnitDeclaration(CompileUnitDeclaration *ModDecl,
                              SMLoc Loc, StringRef Name,
                              DeclList &Decls,
                              StmtList &Stmts);
};


class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Decl *D)
      : Semantics(Semantics) {
    Semantics.enterScope(D);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};