#pragma once
#include "llvm/IR/Value.h"
#include "llvm/Support/SMLoc.h"
#include "Lexer/TokenKinds.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/APFloat.h"
#include <optional>
#include <vector>
#include "Lexer/Lexer.hpp"
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
    DK_Const,
    DK_Alias,
    DK_Class,
    DK_Array,
    DK_Pointer,
    DK_Integer_Type,
    DK_Float_Type,
    DK_Base_Type,
    DK_Var,
    DK_Function,
    DK_Param
  };

private:
  const DeclKind Kind;

public:
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

class ModuleDeclaration  : public Decl {
  DeclList Decls;
  StmtList Stmts;

public:
  std::vector<ModuleDeclaration *> Imported_Module;
  ModuleDeclaration (Decl *EnclosingDecL, SMLoc Loc,
                    StringRef Name)
      : Decl(DK_CompileUnit, EnclosingDecL, Loc, Name) {}

  ModuleDeclaration (Decl *EnclosingDecL, SMLoc Loc,
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
class PointerTypeDeclaration : public TypeDeclaration {
  TypeDeclaration *Type;

public:
  PointerTypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                         StringRef Name,
                         TypeDeclaration *Type)
      : TypeDeclaration(DK_Pointer, EnclosingDecL, Loc,
                        Name),
        Type(Type) {}

  TypeDeclaration *getType() const { return Type; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Pointer;
  }
};
class ConstantDeclaration : public Decl {
public:

  Expr *E;

public:
  ConstantDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                      StringRef Name, Expr *E)
      : Decl(DK_Const, EnclosingDecL, Loc, Name), E(E) {}

  Expr *getExpr() { return E; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Const;
  }
};
class Integer_TypeDeclaration : public TypeDeclaration {
public:
  bool Is_Signed;
  int Size;
  Integer_TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                           StringRef Name,int Size,bool Is_Signed = true)
      : TypeDeclaration(DK_Integer_Type, EnclosingDecL,
                        Loc, Name),Size(Size),Is_Signed(Is_Signed) {}

  static bool classof(const Decl *D) {
    return DK_Float_Type == D->getKind() || D->getKind() == DK_Integer_Type;
  }
};
class Float_TypeDeclaration : public TypeDeclaration {
public:
  bool Is_Signed;
  int Size;
  Float_TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                           StringRef Name,int Size,bool Is_Signed = true)
      : TypeDeclaration(DK_Float_Type, EnclosingDecL,
                        Loc, Name),Size(Size),Is_Signed(Is_Signed) {}

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Float_Type;
  }
};
class Alias_TypeDeclaration : public Decl {
public:
  TypeDeclaration *Realone;
  Alias_TypeDeclaration(Decl *EnclosingDecL, SMLoc Loc, StringRef Name,
                        TypeDeclaration *Realone)
      : Decl(DK_Alias, EnclosingDecL, Loc, Name), Realone(Realone) {}

  TypeDeclaration *getType() { return Realone; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Alias; };
};

class ArrayTypeDeclaration : public TypeDeclaration {
  Expr *Nums;
  TypeDeclaration *Type;

public:
  ArrayTypeDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                       StringRef Name, Expr *Nums,
                       TypeDeclaration *Type)
      : TypeDeclaration(DK_Array, EnclosingDecL, Loc,
                        Name),
        Nums(Nums), Type(Type) {}

  Expr *getNums() const { return Nums; }
  TypeDeclaration *getType() const { return Type; }

  static bool classof(const Decl *D) {
    return D->getKind() == DK_Array;
  }
};

class VariableDeclaration : public Decl {
public:  
  TypeDeclaration *Ty;
  bool is_initlezed;
public:
  VariableDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                      StringRef Name, TypeDeclaration *Ty, bool is_initlezed)
      : Decl(DK_Var, EnclosingDecL, Loc, Name), Ty(Ty), is_initlezed(is_initlezed) {}

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
  enum FType{
    Normal,
    Method,
    Extern,
    Virtual,
    Genrator,
  };
  FType Type = Normal;
  bool is_varg = false;
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

class ClassDeclaration: public Decl{
  public:
  std::vector<Decl*> Decls;
  StmtList Stmts;
  bool is_genric;
  DeclList TempleteArg;
  bool has_constructor = false;
  ClassDeclaration(Decl *EnclosingDecL, SMLoc Loc,
                             StringRef Name,bool is_genric):Decl(DK_Class, EnclosingDecL, Loc, Name),is_genric(is_genric) {

  };
static bool classof(const Decl *D) {
    return D->getKind() == DK_Class;
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
    EK_Float,
    EK_Bool,
    EK_Designator,
    EK_Const,
    EK_Func,
    EK_Meth,
    EK_Constructor,
    EK_String,
    EK_Cast,
    EK_Impl,
    EK_Sizeof
  };

private:
  const ExprKind Kind;
  TypeDeclaration *Ty;
  bool IsConstant;
public:
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
  TypeDeclaration *Type;

protected:
  Selector(SelectorKind Kind, TypeDeclaration *Type)
      : Kind(Kind), Type(Type) {}

public:
  SelectorKind getKind() const { return Kind; }
  TypeDeclaration *getType() const { return Type; }
};

class IndexSelector : public Selector {
  Expr *Index;

public:
  IndexSelector(Expr *Index, TypeDeclaration *Type)
      : Selector(SK_Index, Type), Index(Index) {}

  Expr *getIndex() const { return Index; }

  static bool classof(const Selector *Sel) {
    return Sel->getKind() == SK_Index;
  }
};

class FieldSelector : public Selector {
  uint32_t Index;
  StringRef Name;

public:
  FieldSelector(uint32_t Index, StringRef Name,
                TypeDeclaration *Type)
      : Selector(SK_Field, Type), Index(Index), Name(Name) {
  }

  uint32_t getIndex() const { return Index; }
  const StringRef &getname() const { return Name; }

  static bool classof(const Selector *Sel) {
    return Sel->getKind() == SK_Field;
  }
};

class Designator : public Expr {
    Decl* Var;
    SelectorList Selectors;

public:
    bool Derfernce = false;
    bool Get_Adress = false;
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
    void Derfernced(){
      Derfernce = true;
      assert(isa<PointerTypeDeclaration>(getType()));
      setType(dyn_cast<PointerTypeDeclaration>(getType())->getType());
    }
    void Get_Pointer(){
      Get_Adress = true;
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
class FloatLiteral : public Expr {
  SMLoc Loc;
  llvm::APFloat Value;

public:
  FloatLiteral(SMLoc Loc, const llvm::APFloat &Value,
                 TypeDeclaration *Ty)
      : Expr(EK_Float, Ty, true), Loc(Loc), Value(Value) {}
  llvm::APFloat &getValue() { return Value; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Float;
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

class String_Literal : public Expr {
  public:
  SMLoc Loc;
  StringRef Value;
  
  String_Literal(SMLoc Loc,StringRef Value, TypeDeclaration *Ty)
      : Expr(EK_String, Ty, true), Value(Value),Loc(Loc) {}
  StringRef getValue() { return Value; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_String;
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
class MethodCallExpr :public Expr{
   public:
  VariableDeclaration *Var;
  StringRef Function_Name;
  ExprList Params;

  MethodCallExpr(VariableDeclaration *Var, StringRef Function_Name,
                   ExprList Params, TypeDeclaration* Ty)
      : Expr(EK_Meth, Ty , false),Var(Var),
        Function_Name(Function_Name), Params(Params) {}

  // FunctionDeclaration *geDecl() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Meth;
  }
};
class ConstructorCallExpr :public Expr{
   public:
  FunctionDeclaration* Func;
  ExprList Params;

  ConstructorCallExpr(FunctionDeclaration* Func,
                   ExprList Params)
      : Expr(EK_Constructor, Func->getRetType() , false),Func(Func),
        Params(Params) {}

  // FunctionDeclaration *geDecl() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Constructor;
  }
};
class CastExpr :public Expr{
   public:
  Expr *E;
  TypeDeclaration* Type_to_cast_for;

  CastExpr(Expr *E, TypeDeclaration* Type_to_cast_for)
      : Expr(EK_Cast, Type_to_cast_for , E->isConst()),E(E),Type_to_cast_for(Type_to_cast_for)//TODO fix returntype {}
      {};

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Cast;
  }
};
class ImpExpr : public Expr{
  using Expr::Expr;
 static bool classof(const Expr *E) {
    return E->getKind() == EK_Impl;
  }
};
class SizeofExpr : public Expr{
  public:
  TypeDeclaration *TypeTogetsize;
  SizeofExpr(TypeDeclaration *Ty,TypeDeclaration *Int_ty):Expr(Expr::EK_Sizeof, Int_ty , true),TypeTogetsize(Ty){
    
  }
 static bool classof(const Expr *E) {
    return E->getKind() == EK_Sizeof;
  }
};
class Stmt {
public:
    enum StmtKind {
        SK_Assign,
        SK_ProcCall,
        Sk_MethodCall,
        SK_If,
        SK_While,
        SK_Return,
        SK_For,
    };

private:
    const StmtKind Kind;
    SMLoc Loc;
protected:
    Stmt(StmtKind Kind,SMLoc Loc) : Kind(Kind),Loc(Loc) {}
public:
    StmtKind getKind() const { return Kind; }
    SMLoc getLoc() const {return Loc;}
};

class AssignmentStatement : public Stmt {
    Designator* Var;
    Expr* E;

public:
    AssignmentStatement(Designator* Var, Expr* E,SMLoc Loc)
        : Stmt(SK_Assign,Loc), Var(Var), E(E) {}

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
        ExprList& Params,SMLoc Loc)
        : Stmt(SK_ProcCall,Loc), Proc(Proc), Params(Params) {}

    FunctionDeclaration* getProc() { return Proc; }
    const ExprList& getParams() { return Params; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_ProcCall;
    }
};
class MethodCallStatement :public Stmt{
  public:
  Expr *Var;
  StringRef Function_Name;
  ExprList Params;


  MethodCallStatement(Expr *Var, StringRef Function_Name,
                   ExprList &Params,SMLoc Loc)
      : Stmt(Sk_MethodCall,Loc),Var(Var),
        Function_Name(Function_Name), Params(Params) {}

  // FunctionDeclaration *geDecl() { return Proc; }
  const ExprList &getParams() { return Params; }

  static bool classof(const Stmt *E) {
    return E->getKind() == Sk_MethodCall;
  }
};
class IfStatement : public Stmt {
    Expr* Cond;
    StmtList IfStmts;
    StmtList ElseStmts;

public:
    IfStatement(Expr* Cond, StmtList& IfStmts,
        StmtList& ElseStmts,SMLoc Loc)
        : Stmt(SK_If,Loc), Cond(Cond), IfStmts(IfStmts),
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
    WhileStatement(Expr* Cond, StmtList& Stmts,SMLoc Loc)
        : Stmt(SK_While,Loc), Cond(Cond), Stmts(Stmts) {}

    Expr* getCond() { return Cond; }
    const StmtList& getWhileStmts() { return Stmts; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_While;
    }
};

class ReturnStatement : public Stmt {
    Expr* RetVal;

public:
    ReturnStatement(Expr* RetVal,SMLoc Loc)
        : Stmt(SK_Return,Loc), RetVal(RetVal) {}

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

    ForStatement(Expr *Cond, StmtList &Start_Val,StmtList &Step, StmtList &Body,SMLoc Loc)
        : Stmt(SK_For,Loc), Cond(Cond), Start_Val(Start_Val),Step(Step),Body(Body) {}

    Expr* getCond() { return Cond; }
    // const StmtList& getWhileStmts() { return Stmts; }

    static bool classof(const Stmt* S) {
        return S->getKind() == SK_For;
    }
};
class ConstantAccess : public Expr {
  ConstantDeclaration *Const;

public:
  ConstantAccess(ConstantDeclaration *Const)
      : Expr(EK_Const, Const->getExpr()->getType(), true),
        Const(Const) {}

  ConstantDeclaration *getDecl() { return Const; }

  static bool classof(const Expr *E) {
    return E->getKind() == EK_Const;
  }
};
