#pragma once
#include "AST/AST.hpp"
#include "Diag/Diagnostic.hpp"
#include "Scope.hpp"
#include <variant>


class Sema {
public:
  void enterScope(Decl *);
  void leaveScope();
  Scope *CurrentScope;
  Decl *CurrentDecl;
  DiagnosticsEngine &Diags;

  TypeDeclaration *IntegerType;
  TypeDeclaration *BoolType;
  TypeDeclaration *ByteType;
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  ConstantDeclaration *TrueConst;
  ConstantDeclaration *FalseConst;
  Sema(DiagnosticsEngine &Diags)
      : CurrentScope(nullptr), CurrentDecl(nullptr), Diags(Diags) {
    initialize();
  }

  void initialize();

  FunctionDeclaration *actOnFunctionDeclaration(SMLoc Loc, StringRef Name);
  void actOnFunctionHeading(FunctionDeclaration *ProcDecl, ParamList &Params,
                            Decl *RetType);
  void actOnFunctionDeclaration(FunctionDeclaration *ProcDecl, SMLoc Loc,
                                StringRef Name, DeclList &Decls,
                                StmtList &Stmts);

  TypeDeclaration *actOnTypeRefernce(SMLoc Loc, StringRef Name);
  ParameterDeclaration *actOnParmaDecl(SMLoc Loc, StringRef Name, Decl *Type,bool by_ref);
  VariableDeclaration *actOnVarDeceleration(SMLoc Loc, StringRef Name,
                                            Decl *Type, bool is_initlezed);
  void actOnReturnStatement(StmtList &Stmts, SMLoc Loc, Expr *RetVal);
  Decl *actOnVarRefernce(SMLoc Loc, StringRef Name);

  void actOnAssignment(StmtList &Stmts, SMLoc Loc, Expr *D, Expr *E);

  CompileUnitDeclaration *actOnCompileUnitDeclaration(SMLoc Loc,
                                                      StringRef Name);
  void actOnCompileUnitDeclaration(CompileUnitDeclaration *ModDecl, SMLoc Loc,
                                   StringRef Name, DeclList &Decls,
                                   StmtList &Stmts);

  Expr *actOnDesignator(Decl *D);
  Expr *actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
  Expr *actOnIntegerLiteral(SMLoc Loc, int Literal);
  Expr *actOnExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnSimpleExpression(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnTerm(Expr *Left, Expr *Right, const OperatorInfo &Op);
  Expr *actOnPrefixExpression(Expr *E, const OperatorInfo &Op);
  bool isOperatorForType(tok::TokenKind Op,
                             TypeDeclaration *Ty) ;
  FunctionCallStatement *actOnFunctionCallStatemnt(SMLoc Loc, Decl *D,
                     ExprList &Params);
  Expr *actOnFunctionCallExpr(SMLoc Loc, Decl *D,
                     ExprList &Params);
  void actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &IfStmts,
                        StmtList &ElseStmts);
  void actOnWhileStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &WhileStmts);
  void actOnForStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &Start_Val,StmtList &ForStepStmts, StmtList &ForBodyStmts);
  ClassDeclaration *actOnClassDeclaration(SMLoc Loc, StringRef Name,bool Is_Genric);
  void actOnClassBody(Decl* D,DeclList &Decls,StmtList &Start);
  Expr *actOnStringLiteral(SMLoc Loc, StringRef Literal);
  void actOnConstantDeclaration(DeclList &Decls,
                                    SMLoc Loc,
                                    StringRef Name,
                                    Expr *E);
  void actOnAliasTypeDeclaration(DeclList &Decls, SMLoc Loc,
                                 StringRef Name, Decl *D);
  void actOnIndexSelector(Expr *Desig, SMLoc Loc, Expr *E);
  void actOnFieldSelector(Expr *Desig, SMLoc Loc, StringRef Name);
  ArrayTypeDeclaration *actOnArrayTypeDeclaration(DeclList &Decls, SMLoc Loc,Expr *E,Decl *D);
  void Create_Genric_type(StringRef Name,SMLoc loc);
  void Create_Genric_Var(DeclList Decls,StringRef Name,SMLoc loc, TypeDeclaration* Ty);     
  ClassDeclaration *init_genric_class(DeclList &Decls,Decl *T,std::vector<std::variant<TypeDeclaration*,Expr *>> Args);   
  TypeDeclaration *Get_type(TypeDeclaration* Type);    
  void checkFormalAndActualParameters(
    SMLoc Loc, const ParamList &Formals,
    const ExprList &Actuals);                
  TypeDeclaration *Get_Pointer_Type(TypeDeclaration *Ty);
  Expr *Get_Refernce(SMLoc loc,Expr *E);
  Expr *DeRefernce(SMLoc loc,Expr *E);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Decl *D) : Semantics(Semantics) {
    Semantics.enterScope(D);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};