#pragma once
#include "llvm/IR/Value.h"
#include "llvm/Support/SMLoc.h"
#include "Lexer/TokenKinds.hpp"
#include "llvm/ADT/APSInt.h"
using namespace llvm;




class Decl;
class ParameterDeclaration;
class Expr;
class Selector;
class Stmt;
class TypeDeclaration;
class Code_Genrator;

using DeclList = std::vector<Decl *>;
using ParamList =
    std::vector<ParameterDeclaration *>;
using ExprList = std::vector<Expr *>;
using StmtList = std::vector<Stmt *>;
using SelectorList = std::vector<Selector*>;

class Decl {
public:
  enum DeclKind {
    DK_CompileUnit,
    DK_Alias,
    DK_Base_Type,
    DK_Var,
    DK_Function,
    DK_Param
  };

private:
  const DeclKind Kind;

protected:
  Decl *EnclosingDecL;
  SMLoc Loc;
  StringRef Name;

public:
  Decl(DeclKind Kind, Decl *EnclosingDecL, SMLoc Loc,
       StringRef Name)
      : Kind(Kind), EnclosingDecL(EnclosingDecL), Loc(Loc),
        Name(Name) {}

  DeclKind getKind() const { return Kind; }
  SMLoc getLocation() { return Loc; }
  StringRef getName() { return Name; }
  Decl *getEnclosingDecl() { return EnclosingDecL; }
};

class CompileUnitDeclaration : public Decl {
  DeclList Decls;
  StmtList Stmts;

public:
  CompileUnitDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                    StringRef Name)
      : Decl(DK_CompileUnit, EnclosingDecL, Loc, Name) {}

  CompileUnitDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                    StringRef Name, DeclList &Decls,
                    StmtList &Stmts)
      : Decl(DK_CompileUnit, EnclosingDecL, Loc, Name),
        Decls(Decls), Stmts(Stmts) {}

  const DeclList &getDecls() { return Decls; }
  void setDecls(DeclList &D) { Decls = D; }
  const StmtList &getStmts() { return Stmts; }
  void setStmts(StmtList &L) { Stmts = L; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_CompileUnit;
  }
};

class TypeDeclaration : public Decl {
protected:
  TypeDeclaration(DeclKind Kind, Decl *EnclosingDecL,
                  SMLoc Loc, StringRef Name)
      : Decl(Kind, EnclosingDecL, Loc, Name) {}

public:
  static bool classof(const Decl *D) {
    return D->getKind() >= DK_Alias &&
           D->getKind() <= DK_Base_Type;
  }
};


class Base_TypeDeclaration : public TypeDeclaration {
public:
  Base_TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                           StringRef Name)
      : TypeDeclaration(DK_Base_Type, EnclosingDecL,
                        Loc, Name) {}

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Base_Type;
  }
};

class VariableDeclaration : public Decl {
  TypeDeclaration *Ty;

public:
  VariableDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                      StringRef Name, TypeDeclaration *Ty)
      : Decl(DK_Var, EnclosingDecL, Loc, Name), Ty(Ty) {}

  TypeDeclaration *getType() { return Ty; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Var;
  };
};


class FunctionDeclaration : public Decl {
  ParamList Params;
  TypeDeclaration *RetType;
  DeclList Decls;
  StmtList Stmts;

public:
  FunctionDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                       StringRef Name)
      : Decl(DK_Function, EnclosingDecL, Loc, Name) {}

  FunctionDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                       StringRef Name,
                       ParamList &Params,
                       TypeDeclaration *RetType,
                       DeclList &Decls, StmtList &Stmts)
      : Decl(DK_Function, EnclosingDecL, Loc, Name),
        Params(Params), RetType(RetType), Decls(Decls),
        Stmts(Stmts) {}

  const ParamList &getFormalParams() {
    return Params;
  }
  void setFormalParams(ParamList &FP) { Params = FP; }
  TypeDeclaration *getRetType() { return RetType; }
  void setRetType(TypeDeclaration *Ty) { RetType = Ty; }

  const DeclList &getDecls() { return Decls; }
  void setDecls(DeclList &D) { Decls = D; }
  const StmtList &getStmts() { return Stmts; }
  void setStmts(StmtList &L) { Stmts = L; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Function;
  }
};

class ParameterDeclaration : public Decl {
  TypeDeclaration *Ty;
  bool IsVar;

public:
  ParameterDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                             StringRef Name,
                             TypeDeclaration *Ty,
                             bool IsVar)
      : Decl(DK_Param, EnclosingDecL, Loc, Name), Ty(Ty),
        IsVar(IsVar) {}

  TypeDeclaration *getType() const { return Ty; }
  bool IsPassedbyReference() const { return IsVar; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Param;
  }
};

class OperatorInfo {
  SMLoc Loc;
  uint32_t Kind : 16;
  uint32_t IsUnspecified : 1;

public:
  OperatorInfo()
      : Loc(), Kind(tok::unknown), IsUnspecified(true) {}
  OperatorInfo(SMLoc Loc, tok::TokenKind Kind,
               bool IsUnspecified = false)
      : Loc(Loc), Kind(Kind), IsUnspecified(IsUnspecified) {
  }

  SMLoc getLocation() const { return Loc; }
  tok::TokenKind getKind() const {
    return static_cast<tok::TokenKind>(Kind);
  }
  bool isUnspecified() const { return IsUnspecified; }
};

class Expr {
public:
  enum ExprKind {
    EK_Infix,
    EK_Prefix,
    EK_Int,
    EK_Bool,
    EK_Designator,
    EK_Const,
    EK_Func,
  };

private:
  const ExprKind Kind;
  TypeDeclaration *Ty;
  bool IsConstant;

protected:
  Expr(ExprKind Kind, TypeDeclaration *Ty, bool IsConst)
      : Kind(Kind), Ty(Ty), IsConstant(IsConst) {}

public:
  ExprKind getKind() const { return Kind; }
  TypeDeclaration *getType() { return Ty; }
  void setType(TypeDeclaration *T) { Ty = T; }
  bool isConst() { return IsConstant; }
};
class Selector {
public:
    enum SelectorKind {
        SK_Index,
        SK_Field,
        SK_Dereference,
    };

private:
    const SelectorKind Kind;

    // The type decribes the base type.
    // E.g. the component type of an index selector
    TypeDeclaration* Type;

protected:
    Selector(SelectorKind Kind, TypeDeclaration* Type)
        : Kind(Kind), Type(Type) {}

public:
    SelectorKind getKind() const { return Kind; }
    TypeDeclaration* getType() const { return Type; }
};

class Designator : public Expr {
    Decl* Var;
    SelectorList Selectors;

public:
    Designator(VariableDeclaration* Var)
        : Expr(EK_Designator, Var->getType(), false),
        Var(Var) {}
    Designator(ParameterDeclaration* Param)
        : Expr(EK_Designator, Param->getType(), false),
        Var(Param) {}

    void addSelector(Selector* Sel) {
        Selectors.push_back(Sel);
        setType(Sel->getType());
    }

    Decl* getDecl() { return Var; }
    const SelectorList& getSelectors() const {
        return Selectors;
    }

    static bool classof(const Expr* E) {
        return E->getKind() == EK_Designator;
    }
};

class InfixExpression : public Expr {
  Expr *Left;
  Expr *Right;
  const OperatorInfo Op;

public:
  InfixExpression(Expr *Left, Expr *Right, OperatorInfo Op,
                  TypeDeclaration *Ty, bool IsConst)
      : Expr(EK_Infix, Ty, IsConst), Left(Left),
        Right(Right), Op(Op) {}

  Expr *getLeft() { return Left; }
  Expr *getRight() { return Right; }
  const OperatorInfo &getOperatorInfo() { return Op; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Infix;
  }
};

class PrefixExpression : public Expr {
  Expr *E;
  const OperatorInfo Op;

public:
  PrefixExpression(Expr *E, OperatorInfo Op,
                   TypeDeclaration *Ty, bool IsConst)
      : Expr(EK_Prefix, Ty, IsConst), E(E), Op(Op) {}

  Expr *getExpr() { return E; }
  const OperatorInfo &getOperatorInfo() { return Op; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Prefix;
  }
};

class IntegerLiteral : public Expr {
  SMLoc Loc;
  llvm::APSInt Value;

public:
  IntegerLiteral(SMLoc Loc, const llvm::APSInt &Value,
                 TypeDeclaration *Ty)
      : Expr(EK_Int, Ty, true), Loc(Loc), Value(Value) {}
  llvm::APSInt &getValue() { return Value; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Int;
  }
};

class BooleanLiteral : public Expr {
  bool Value;

public:
  BooleanLiteral(bool Value, TypeDeclaration *Ty)
      : Expr(EK_Bool, Ty, true), Value(Value) {}
  bool getValue() { return Value; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Bool;
  }
};

class FunctionCallExpr : public Expr {
  FunctionDeclaration *Proc;
  ExprList Params;

public:
  FunctionCallExpr(FunctionDeclaration *Proc,
                   ExprList Params)
      : Expr(EK_Func, Proc->getRetType(), false),
        Proc(Proc), Params(Params) {}

  FunctionDeclaration *geDecl() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Func;
  }
};

class Stmt {
public:
    enum StmtKind {
        SK_Assign,
        SK_ProcCall,
        SK_If,
        SK_While,
        SK_Return,
        SK_For,
    };

private:
    const StmtKind Kind;

protected:
    Stmt(StmtKind Kind) : Kind(Kind) {}

public:
    StmtKind getKind() const { return Kind; }
};

class AssignmentStatement : public Stmt {
    Designator* Var;
    Expr* E;

public:
    AssignmentStatement(Designator* Var, Expr* E)
        : Stmt(SK_Assign), Var(Var), E(E) {}

    Designator* getVar() { return Var; }
    Expr* getExpr() { return E; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_Assign;
    }
};

class FunctionCallStatement : public Stmt {
    FunctionDeclaration* Proc;
    ExprList Params;

public:
    FunctionCallStatement(FunctionDeclaration* Proc,
        ExprList& Params)
        : Stmt(SK_ProcCall), Proc(Proc), Params(Params) {}

    FunctionDeclaration* getProc() { return Proc; }
    const ExprList& getParams() { return Params; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_ProcCall;
    }
};

class IfStatement : public Stmt {
    Expr* Cond;
    StmtList IfStmts;
    StmtList ElseStmts;

public:
    IfStatement(Expr* Cond, StmtList& IfStmts,
        StmtList& ElseStmts)
        : Stmt(SK_If), Cond(Cond), IfStmts(IfStmts),
        ElseStmts(ElseStmts) {}

    Expr* getCond() { return Cond; }
    const StmtList& getIfStmts() { return IfStmts; }
    const StmtList& getElseStmts() { return ElseStmts; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_If;
    }
};

class WhileStatement : public Stmt {
    Expr* Cond;
    StmtList Stmts;

public:
    WhileStatement(Expr* Cond, StmtList& Stmts)
        : Stmt(SK_While), Cond(Cond), Stmts(Stmts) {}

    Expr* getCond() { return Cond; }
    const StmtList& getWhileStmts() { return Stmts; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_While;
    }
};

class ReturnStatement : public Stmt {
    Expr* RetVal;

public:
    ReturnStatement(Expr* RetVal)
        : Stmt(SK_Return), RetVal(RetVal) {}

    Expr* getRetVal() { return RetVal; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_Return;
    }
};

class ForStatement : public Stmt {
public:
    StmtList Start_Val;
    Expr* Cond;
    StmtList Step;

    StmtList Body;

    ForStatement(Expr *Cond, StmtList &Start_Val,StmtList &Step, StmtList &Body)
        : Stmt(SK_For), Cond(Cond), Start_Val(Start_Val),Step(Step),Body(Body) {}

    Expr* getCond() { return Cond; }
    // const StmtList& getWhileStmts() { return Stmts; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_For;
    }
};
