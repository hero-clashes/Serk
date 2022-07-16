#include "Sema.hpp"
#include "AST/AST.hpp"
#include <array>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <vcruntime.h>
#include <vector>
#include <variant>

bool Sema::isOperatorForType(tok::TokenKind Op,
                             TypeDeclaration *Ty) {
  switch (Op) {
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::slash:
  case tok::Reminder:
    return isa<Integer_TypeDeclaration>(Ty);
  case tok::And:
  case tok::Or:
  case tok::Not:
    return Ty == BoolType;
  default:
    llvm_unreachable("Unknown operator");
  }
}

void Sema::enterScope(Decl *D,StmtList &stmt) {
  CurrentScope = new Scope(D,stmt,CurrentScope);
  CurrentDecl = D;
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  for (auto Val:CurrentScope->Symbols.keys()) {
    auto Decl = CurrentScope->Symbols[Val];
    if(auto Var = dyn_cast_or_null<VariableDeclaration>(Decl)){
      if (auto Class = dyn_cast_or_null<ClassDeclaration>(Var->getType())) {
        auto s = (Class->getName() + "_Delete").str();
        auto F = (FunctionDeclaration*)CurrentScope->lookup(s);
        if(F){
          ExprList a{actOnDesignator(Var)};
          ((Designator*)a[0])->Get_Adress = true;
          CurrentScope->stmts.push_back(actOnFunctionCallStatemnt(CurrentScope->stmts.empty()? SMLoc(): CurrentScope->stmts.back()->getLoc(), F, a));
        
        }
      }
    }
  }
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentScope->P_Decl;
}

void Sema::initialize() {
  std::vector<Stmt *> s;
  CurrentScope = new Scope(nullptr,s,nullptr);
  CurrentDecl = nullptr;
  std::tuple<std::array<const char *,14>,std::array<int,14>,std::array<bool,14>> Int_types = {{"int","float","double","long", "byte", "int64", "int32","int16", "int8","uint64", "uint32","uint16", "uint8" ,"bool"},
                                                                                              {32,    32,        64,     64,     8,      64,     32,     16,      8     ,64,        32,      16,      8 ,       1},
                                                                                              {{true, true,     true , true ,   true, true,   true,     true,   true,   false,      false,   false,    false , false}}};
  auto size = std::get<0>(Int_types).size();
  auto &intnames = std::get<0>(Int_types);
  auto &intsizes = std::get<1>(Int_types);
  auto &intsigns = std::get<2>(Int_types);
  for(int i = 0;i<size;i++){
    if (i == 1 || i == 2) {
    Insert_Decl(new Float_TypeDeclaration(CurrentDecl,SMLoc(),intnames[i],intsizes[i],intsigns[i]));
    } else
    Insert_Decl(new Integer_TypeDeclaration(CurrentDecl,SMLoc(),intnames[i],intsizes[i],intsigns[i]));
  }
  IntegerType = (TypeDeclaration *)CurrentScope->lookup("int");
  BoolType = (Integer_TypeDeclaration *)CurrentScope->lookup("bool");
  DoubleType = (Integer_TypeDeclaration *)CurrentScope->lookup("double");
  FloatType = (Integer_TypeDeclaration *)CurrentScope->lookup("float");
  auto int64 = (Integer_TypeDeclaration *)CurrentScope->lookup("uint64");

  auto Void =
      Insert_Decl(new Base_TypeDeclaration(CurrentDecl, SMLoc(), "void"));
  ByteType = (Integer_TypeDeclaration *)CurrentScope->lookup("int8");

  NullPtr = new ConstantDeclaration(CurrentDecl,SMLoc(),"nullptr", new Expr(Expr::EK_Impl,Get_Pointer_Type(ByteType),true));
  auto printf = (FunctionDeclaration *)Insert_Decl(new FunctionDeclaration(CurrentDecl, SMLoc(), "printf"));
  ParamList c{ new ParameterDeclaration(CurrentDecl,SMLoc(),"str", Get_Pointer_Type(ByteType), false)};
  printf->setFormalParams(c);
  printf->setRetType(IntegerType);
  printf->is_varg = true;
  FunctionDeclaration *Malloc = (FunctionDeclaration *)Insert_Decl(
      new FunctionDeclaration(CurrentDecl, SMLoc(), "malloc"));
  ParamList a{
      new ParameterDeclaration(CurrentDecl, SMLoc(), "size", int64, false)};
  Malloc->setFormalParams(a);
  Malloc->setRetType(Get_Pointer_Type(ByteType));
  FunctionDeclaration *realloc = (FunctionDeclaration *)Insert_Decl(
      new FunctionDeclaration(CurrentDecl, SMLoc(), "realloc"));
  ParamList b{ new ParameterDeclaration(CurrentDecl,SMLoc(),"old_ptr", Get_Pointer_Type(ByteType), false),
      new ParameterDeclaration(CurrentDecl, SMLoc(), "size", int64, false)};
  realloc->setFormalParams(b);
  realloc->setRetType(Get_Pointer_Type(ByteType));
  FunctionDeclaration *free = (FunctionDeclaration *)Insert_Decl(
      new FunctionDeclaration(CurrentDecl, SMLoc(), "free"));
  ParamList d{ new ParameterDeclaration(CurrentDecl,SMLoc(),"ptr", Get_Pointer_Type(ByteType), false)};
  free->setFormalParams(d);
  free->setRetType((TypeDeclaration*)Void);
  TrueLiteral = new BooleanLiteral(true, BoolType);
  FalseLiteral = new BooleanLiteral(false, BoolType);
  TrueConst =
      new ConstantDeclaration(CurrentDecl, SMLoc(), "true", TrueLiteral);
  FalseConst =
      new ConstantDeclaration(CurrentDecl, SMLoc(), "false", FalseLiteral);
};

FunctionDeclaration *Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name){
FunctionDeclaration *P =
      new FunctionDeclaration(CurrentDecl, Loc, Name);
  if(isa<ClassDeclaration>(CurrentDecl)){
    P->Name = *(new std::string((CurrentDecl->Name + "_" + Name).str()));
    if (!CurrentScope->getParent()->insert(P))
    Diags.report(Loc, diag::err_symbold_declared, Name);
  } else
  if (!CurrentScope->insert(P))
    Diags.report(Loc, diag::err_symbold_declared, Name);
  return P;
};


void Sema::actOnFunctionHeading(
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

void Sema::actOnFunctionDeclaration(FunctionDeclaration* ProcDecl, SMLoc Loc, StringRef Name, DeclList& Decls, StmtList& Stmts)
{
    ProcDecl->setDecls(Decls);
    ProcDecl->setStmts(Stmts);
}

TypeDeclaration*   Sema::actOnTypeRefernce(SMLoc Loc, StringRef Name) {
    if (auto D = dyn_cast_or_null<TypeDeclaration>(CurrentScope->lookup(Name))) {
        return D;
    }
    else {
        Diags.report(Loc,diag::err_type_isnt_found,Name);
    };
}
ParameterDeclaration* Sema::actOnParmaDecl(SMLoc Loc, StringRef Name, Decl *Type,bool by_ref)
{
    auto Type_as = dyn_cast_or_null<TypeDeclaration>(Type);
    auto D = new ParameterDeclaration(CurrentDecl, Loc, Name, Type_as, by_ref);
    if (!CurrentScope->insert(D)) {
      Diags.report(Loc, diag::err_symbold_declared, Name);
    }
    return D;
}
VariableDeclaration* Sema::actOnVarDeceleration(SMLoc Loc, StringRef Name, Decl* Type, bool is_initlezed = false)
{

    assert(CurrentScope && "CurrentScope not set");
    if (TypeDeclaration* Ty = dyn_cast<TypeDeclaration>(Type)) {
        VariableDeclaration* Decl = new VariableDeclaration(
            CurrentDecl, Loc, Name, Ty, is_initlezed);
        bool shadowing = false;
        if(CurrentScope->lookup(Name)){
          shadowing = true;
        }
        if (CurrentScope->insert(Decl))
            {
              if(shadowing) Diags.report(Loc, diag::war_var_shadowing, Name);
              return Decl;  
            }
        else
            Diags.report(Loc, diag::err_symbold_declared, Name);
    } else
      Diags.report(Loc, diag::err_type_not_defined, Name);
    return nullptr;
}
void Sema::actOnReturnStatement(StmtList& Stmts, SMLoc Loc, Expr* RetVal)
{
    auto* Proc = dyn_cast<FunctionDeclaration>(CurrentDecl);
    if (Proc->getRetType() && RetVal) {
      if (Get_type(Proc->getRetType()) != Get_type(RetVal->getType())) {
        if (Can_Be_Casted(RetVal, Proc->getRetType())) {
          RetVal = Create_Cast(RetVal, Proc->getRetType());
        } else
          Diags.report(Loc, diag::err_function_and_return_type);
      }
    }

    Stmts.push_back(new ReturnStatement(RetVal,Loc));
}
Decl* Sema::actOnVarRefernce(SMLoc Loc, StringRef Name)
{
    if (auto D = dyn_cast_or_null<VariableDeclaration>(CurrentScope->lookup(Name))) {
        return D;
    } else  if (auto D = dyn_cast_or_null<ParameterDeclaration>(CurrentScope->lookup(Name))) {
        return D;
    } else if (auto D =dyn_cast_or_null<FunctionDeclaration>(CurrentScope->lookup(Name))){
      return D;
    } else if (auto D =dyn_cast_or_null<ConstantDeclaration>(CurrentScope->lookup(Name))){
      return D;
    }else if (Name == "true") {
      return TrueConst;
    }else if (Name == "false") {
      return FalseConst;
    }else if (Name == "nullptr") {
      return NullPtr;
    }else if (auto D = dyn_cast_or_null<ClassDeclaration>(CurrentScope->lookup(Name))){
      auto s = new std::string((Name + "_Create").str());
      if(auto F = dyn_cast_or_null<FunctionDeclaration>(CurrentScope->lookup(*s))){
        return F;
      }
    } else if (auto D = dyn_cast_or_null<ClassDeclaration>(CurrentDecl->getEnclosingDecl())) {
      auto s = new std::string((D->Name + "_" + Name).str());
      if(auto F = dyn_cast_or_null<FunctionDeclaration>(CurrentScope->lookup(*s))){
        return F;
      }
    }
    Diags.report(Loc, diag::err_var_isnt_found, Name);
    auto Results = CurrentScope->fuzzy_search(Name);
    auto max = *std::max_element(Results.begin(),
                             Results.end(),
                             [](const std::pair<Decl *, int>& a,const std::pair<Decl *, int>& b) { return a.second < b.second; });
    Diags.report(max.first->Loc, diag::note_did_you_mean, max.first->Name);
    return nullptr;
}
Expr *Sema::actOnDesignator(Decl *D) {
  if (!D)
    return nullptr;
  if (auto *V = dyn_cast<VariableDeclaration>(D))
    return new Designator(V);
  if (auto *V = dyn_cast<ParameterDeclaration>(D))
    return new Designator(V);
  else if (auto *C = dyn_cast<ConstantDeclaration>(D)) {
    if (C == TrueConst)
      return TrueLiteral;
    if (C == FalseConst) {
      return FalseLiteral;
    }
    return new ConstantAccess(C);
  }
  return nullptr;
}
void Sema::actOnConstantDeclaration(DeclList &Decls,
                                    SMLoc Loc,
                                    StringRef Name,
                                    Expr *E) {
  assert(CurrentScope && "CurrentScope not set");
  ConstantDeclaration *Decl =
      new ConstantDeclaration(CurrentDecl, Loc, Name, E);
  if (CurrentScope->insert(Decl))
    Decls.push_back(Decl);
  else
    Diags.report(Loc, diag::err_symbold_declared, Name);
}
Expr* Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal)
{
    uint8_t Radix = 10;
    if (Literal.endswith("H")) {
        Literal = Literal.drop_back();
        Radix = 16;
    }
    llvm::APInt Value(32, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(Value, false),
        IntegerType);
}
Expr* Sema::actOnIntegerLiteral(SMLoc Loc, int Literal)
{
    uint8_t Radix = 10;
    llvm::APInt Value(32, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(Value, false),
        IntegerType);
}
Expr* Sema::actOnFloatLiteral(SMLoc Loc, StringRef Literal)
{
  if(Literal.endswith_insensitive("f")){
    Literal = Literal.drop_back();
    return new FloatLiteral(Loc, llvm::APFloat(std::stof(Literal.str())), FloatType);
  }

  return new FloatLiteral(Loc, llvm::APFloat(std::stod(Literal.str())), DoubleType);
  
}
void Sema::actOnAssignment(StmtList& Stmts, SMLoc Loc, Expr* D, Expr* E)
{
    if (auto Var = dyn_cast<Designator>(D)) {
        if (Get_type(Var->getType()) != Get_type(E->getType())) {
            if(Can_Be_Casted(E, D->getType())){
              E = Create_Cast(E, D->getType());
            } else
            Diags.report(
               Loc, diag::err_cant_type,
                Get_type(Var->getType())->Name,Get_type(E->getType())->Name);
        }
        Stmts.push_back(new AssignmentStatement(Var, E,Loc));
    }
    else if (D) {
        Diags.report(
               Loc, diag::err_cant_assgin_to_empty_expr);
    }
}
;


ModuleDeclaration  *
Sema::actOnCompileUnitDeclaration(SMLoc Loc, StringRef Name) {
  return new ModuleDeclaration (CurrentDecl, Loc, Name);
}

void Sema::actOnCompileUnitDeclaration(
    ModuleDeclaration  *ModDecl, SMLoc Loc, StringRef Name,
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

Expr *Sema::actOnExpression(Expr *Left, Expr *Right,
                            const OperatorInfo &Op) {
  // Relation
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Get_type(Left->getType()) != Get_type(Right->getType())) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  bool IsConst = Left->isConst() && Right->isConst();
  return new InfixExpression(Left, Right, Op, BoolType,
                             IsConst);
}

Expr *Sema::actOnSimpleExpression(Expr *Left, Expr *Right,
                                  const OperatorInfo &Op) {
  // Addition
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Get_type(Left->getType()) != Get_type(Right->getType())) {//TODO add casting based on bit width
    if (Can_Be_Casted(Left, Right->getType())) {
      Left = Create_Cast(Left, Right->getType());
    } else
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Get_type(Left->getType());
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::Or) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() || R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}
Expr *Sema::actOnTerm(Expr *Left, Expr *Right,
                      const OperatorInfo &Op) {
  // Multiplication
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Get_type(Left->getType()) != Get_type(Right->getType()) ||
      !isOperatorForType(Op.getKind(), Get_type(Left->getType()))) {
    if (Can_Be_Casted(Left, Right->getType())) {
      Left = Create_Cast(Left, Right->getType());
    } else
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Get_type(Left->getType());
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::And) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() && R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}

Expr *Sema::actOnPrefixExpression(Expr *E,
                                  const OperatorInfo &Op) {
  if (!E)
    return nullptr;

  if (!isOperatorForType(Op.getKind(), Get_type(E->getType()))) {
    if(Op.getKind() == tok::Not && isa<PointerTypeDeclaration>(Get_type(E->getType()))){
      E = Create_Cast(E, BoolType);
    } else
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }

  if (E->isConst() && Op.getKind() == tok::Not) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(E);
    return L->getValue() ? FalseLiteral : TrueLiteral;
  }

  if (Op.getKind() == tok::minus) {
    bool Ambiguous = true;
    if (isa<IntegerLiteral>(E) || isa<Designator>(E))
      Ambiguous = false;
    else if (auto *Infix = dyn_cast<InfixExpression>(E)) {
      tok::TokenKind Kind =
          Infix->getOperatorInfo().getKind();
      if (Kind == tok::star || Kind == tok::slash)
        Ambiguous = false;
    }
    if (Ambiguous) {
      Diags.report(Op.getLocation(),
                   diag::warn_ambigous_negation);
    }
  }

  return new PrefixExpression(E, Op, Get_type(E->getType()),
                              E->isConst());
}

FunctionCallStatement *Sema::actOnFunctionCallStatemnt(SMLoc Loc, Decl *D,
                     ExprList &Params){

   if (!D)
    return nullptr;
  if (auto *P = dyn_cast<FunctionDeclaration>(D)) {
    if (!P->Name.endswith("Delete")) {
    checkFormalAndActualParameters(P,
        D->getLocation(), P->getFormalParams(), Params);
    if (!P->getRetType())
      Diags.report(D->getLocation(),
                   diag::err_function_call_on_nonfunction);
    }
    
    return new FunctionCallStatement(P, Params,Loc);
  }
  Diags.report(D->getLocation(),
               diag::err_function_call_on_nonfunction);
  return nullptr;
};
Expr *Sema::actOnFunctionCallExpr(SMLoc Loc, Decl *D,
                     ExprList &Params){

   if (!D)
    return nullptr;
  if (auto *P = dyn_cast<FunctionDeclaration>(D)) {
    checkFormalAndActualParameters(P,
        D->getLocation(), P->getFormalParams(), Params);
    if (!P->getRetType())
      Diags.report(D->getLocation(),
                   diag::err_function_call_on_nonfunction);
    return new FunctionCallExpr(P, Params);
  }
  Diags.report(D->getLocation(),
               diag::err_function_call_on_nonfunction);
  return nullptr;
};
Expr *Sema::actOnConstructorCallExpr(SMLoc Loc, Decl *D,
                     ExprList &Params){
  if (!D)
    return nullptr;
  if (auto *P = dyn_cast<FunctionDeclaration>(D)) {
    checkFormalAndActualParameters(P,
        D->getLocation(), P->getFormalParams(), Params);
    if (!P->getRetType())
      Diags.report(D->getLocation(),
                   diag::err_function_call_on_nonfunction);
    return new ConstructorCallExpr(P, Params);
  }
  Diags.report(D->getLocation(),
               diag::err_function_call_on_nonfunction);
  return nullptr;
};
Expr *Sema::actOnMethodCallExpr(SMLoc Loc, Decl *D, StringRef Method_Name,
                                ExprList &Params){
  auto name = (((VariableDeclaration*)D)->getType()->getName() + "_" + Method_Name).str();
  auto M = CurrentScope->lookup(name);
  if(!M ||!isa<FunctionDeclaration>(M)){
    Diags.report(D->getLocation(),
                   diag::err_function_call_on_nonfunction);
  } else {
    auto P = (FunctionDeclaration*)M;
    checkFormalAndActualParameters(P,
        Loc, P->getFormalParams(), Params);
  }
  return new MethodCallExpr((VariableDeclaration*)D,Method_Name,Params,((FunctionDeclaration*)M)->getRetType());                 
};
void Sema::actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &IfStmts,
                        StmtList &ElseStmts){
// if (!Cond)
//     Cond = FalseLiteral;

  if (Cond->getType() != BoolType) {
    if(Can_Be_Casted(Cond, BoolType)){
      Cond = Create_Cast(Cond, BoolType);
    } else
    Diags.report(Loc, diag::err_if_expr_must_be_bool);
  }
  Stmts.push_back(
      new IfStatement(Cond, IfStmts, ElseStmts,Loc));
};
void Sema::actOnWhileStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &WhileStmts){


    Stmts.push_back(new WhileStatement(Cond,WhileStmts,Loc));
                        };
void Sema::actOnForStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &Start_Val,StmtList &ForStepStmts, StmtList &ForBodyStmts){
      Stmts.push_back(new ForStatement(Cond,Start_Val,ForStepStmts,ForBodyStmts,Loc));
                        };
ClassDeclaration *Sema::actOnClassDeclaration(SMLoc Loc, StringRef Name,bool Is_Genric){
  ClassDeclaration *P =
      new ClassDeclaration(CurrentDecl, Loc, Name,Is_Genric);
  if (!CurrentScope->getScopeAtDepth(0)->insert(P))
    Diags.report(Loc, diag::err_symbold_declared, Name);
  return P;
};

void Sema::actOnClassBody(Decl* D,DeclList &Decls,StmtList &Start){
  auto classd = dyn_cast_or_null<ClassDeclaration>(D);
  classd->Decls = Decls;
  classd->Stmts = Start;
};
Expr *Sema::actOnStringLiteral(SMLoc Loc, StringRef Literal){
  return new String_Literal(Loc, Literal,
            Get_Pointer_Type(ByteType)
);
};
void Sema::actOnAliasTypeDeclaration(DeclList &Decls, SMLoc Loc,
                                 StringRef Name, Decl *D){
  assert(CurrentScope && "CurrentScope not set");
  if (TypeDeclaration *Ty = dyn_cast<TypeDeclaration>(D)) {
    Alias_TypeDeclaration *Decl = new Alias_TypeDeclaration(
        CurrentDecl, Loc, Name, Ty);
    if (CurrentScope->insert(Decl))
      Decls.push_back(Decl);
    else
      Diags.report(Loc, diag::err_symbold_declared, Name);
  } else {
    Diags.report(Loc,
                diag::err_vardecl_requires_type);
  }

  };
void Sema::actOnIndexSelector(Expr *Desig, SMLoc Loc, Expr *E) {
  if (auto *D = dyn_cast<Designator>(Desig)) {
    if (auto *Ty = dyn_cast<ArrayTypeDeclaration>(D->getType()) ) {
      D->addSelector(new IndexSelector(E, Ty->getType()));
    } else if (auto *Ty = dyn_cast<PointerTypeDeclaration>(D->getType()) ) {
      D->addSelector(new IndexSelector(E, Ty->getType()));
    } else {
      Diags.report(Loc, diag::err_indexing_non_array);
    }
    // TODO Error message
  }
}

void Sema::actOnFieldSelector(Expr *Desig, SMLoc Loc,
                              StringRef Name) {
  if (auto *D = dyn_cast<Designator>(Desig)) {
    if (auto *R =
            dyn_cast<ClassDeclaration>(Get_type(D->getType()))) {
      uint32_t Index = 0;
      for (const auto &F : R->Decls) {
        auto F_V = dyn_cast_or_null<VariableDeclaration>(F);
        if (F->getName() == Name) {
          D->addSelector(
              new FieldSelector(Index, Name, Get_type(F_V->getType())));
          return;
        }
        ++Index;
      }
      Diags.report(Loc, diag::err_member_not_found,Name, R->getName());
    } else
    Diags.report(Loc, diag::err_accessing_member_non_class);
  }
  // TODO Error message
}
void Sema::Create_Genric_type(StringRef Name,SMLoc loc){
  auto Genric = new Alias_TypeDeclaration(CurrentDecl,loc,Name,nullptr);
  dyn_cast_or_null<ClassDeclaration>(CurrentDecl)->TempleteArg.push_back(Genric);
  CurrentScope->insert(Genric);
};
void Sema::Create_Genric_Var(DeclList Decls,StringRef Name,SMLoc loc,TypeDeclaration* Ty){
  auto Genric = new ConstantDeclaration(CurrentDecl,loc,Name,new Expr(Expr::ExprKind::EK_Const,Ty,true));
  dyn_cast_or_null<ClassDeclaration>(CurrentDecl)->TempleteArg.push_back(Genric);
  CurrentScope->insert(Genric);
  Decls.push_back(Genric);
};
ClassDeclaration *Sema::init_genric_class(DeclList &Decls,Decl *T,std::vector<std::variant<TypeDeclaration*,Expr *>> Args,SMLoc Loc){
  auto Class = dyn_cast_or_null<ClassDeclaration>(T);
  if(!Class){
    Diags.report(Loc, diag::err_not_class);
    return nullptr;
  }
  if(!Class->is_genric){
    Diags.report(Loc, diag::err_class_not_genric);
    return nullptr;
  }
  auto Class_Copy = new ClassDeclaration(*Class);
  Class_Copy->is_genric = false;
  auto new_name =new std::string(Class_Copy->Name.str());
  if(Args.size() != Class_Copy->TempleteArg.size()){
    Diags.report(Loc, diag::err_wrong_number_of_parameters_templete);
  }
  int index = 0;
  for(auto V:Args){
    switch(V.index()){
      case 0:
      {
        auto Ty = std::get<TypeDeclaration*>(V);
        if(auto T = dyn_cast_or_null<Alias_TypeDeclaration>(Class_Copy->TempleteArg[index])){
          T->Realone = Ty;
        } else {
          Diags.report(Loc, diag::err_passing_type_to_non_type_templete_arg);
        };
      }
      break;
      case 1:
      {
        auto Exp = std::get<Expr*>(V);
        if(auto Const = dyn_cast_or_null<ConstantDeclaration>(Class_Copy->TempleteArg[index])){
          Const->E = Exp;
        } else{
          Diags.report(Loc, diag::err_passing_var_to_non_var_templete_arg);
        };
      }
      break;
    };
    index++;
  };
  Class_Copy->Name=StringRef(*new_name);


  CurrentScope->getScopeAtDepth(0)->insert(Class_Copy);
  Decls.push_back(Class_Copy);
  return Class_Copy;
}
TypeDeclaration *Sema::Get_type(TypeDeclaration* Type){
  if(auto Alias = dyn_cast_or_null<Alias_TypeDeclaration>(Type)){
    return Get_type(Alias->Realone);
  }else return Type;
};

ArrayTypeDeclaration *Sema::actOnArrayTypeDeclaration(DeclList &Decls, SMLoc Loc,Expr *E,Decl *D){
  assert(CurrentScope && "CurrentScope not set");
  if (E && E->isConst() &&
      E->getType() == IntegerType) {
    if (TypeDeclaration *Ty =
            dyn_cast<TypeDeclaration>(D)) {
      auto str = new std::string(Ty->Name.str() + "Array");
      ArrayTypeDeclaration *Decl = new ArrayTypeDeclaration(
          CurrentDecl, Loc,StringRef(*str), E, Ty);
      if(CurrentScope->insert(Decl))
        return Decl;
      else
        return (ArrayTypeDeclaration *)CurrentScope->lookup(*str);
    } else {
      Diags.report(Loc,
                  diag::err_vardecl_requires_type);
    }
  } //TODO add error messeges handling
};
void Sema::checkFormalAndActualParameters(FunctionDeclaration *F,
    SMLoc Loc, const ParamList &Formals,
    ExprList &Actuals) {
  if ((Formals.size() != Actuals.size()) && !F->is_varg) {
    Diags.report(Loc, diag::err_wrong_number_of_parameters);
    return;
  }
  for (int i = 0; i<Formals.size();i++) {
    ParameterDeclaration *F = Formals[i];
    Expr *&Arg = Actuals[i];
    if (F->getType() != Arg->getType())
      if(Can_Be_Casted(Arg, F->getType())){
      Arg = Create_Cast(Arg, F->getType()); 
      }
      else
      Diags.report(
          Loc,
          diag::
              err_type_of_formal_and_actual_parameter_not_compatible);
    if (F->IsPassedbyReference() && !isa<Designator>(Arg))
      Diags.report(Loc,
                   diag::err_var_parameter_requires_var);
  }
}
TypeDeclaration *Sema::Get_Pointer_Type(TypeDeclaration *Ty){
    auto t = Ty->Name + "_Pointer";
    auto s = new std::string(t.str());
    if (auto typ = CurrentScope->lookup(StringRef{*s})) {
      return (TypeDeclaration *)typ;
    } else
    {
      auto typ2 = new PointerTypeDeclaration(CurrentDecl,Ty->Loc,StringRef{*s},Ty);
      if(CurrentScope->getParent())
      CurrentScope->getParent()->insert(typ2);
      else 
      CurrentScope->insert(typ2);
      return typ2;
    };
}
Expr *Sema::Get_Refernce(SMLoc loc,Expr *E){
  return new PrefixExpression(E,OperatorInfo(loc,tok::Amper),Get_Pointer_Type(E->getType()),E->isConst());
};
Expr *Sema::DeRefernce(SMLoc loc,Expr *E){
  return new PrefixExpression(E,OperatorInfo(loc,tok::star),dyn_cast_or_null<PointerTypeDeclaration>(E->getType())->getType(),E->isConst());
};
bool is_int(TypeDeclaration *Dest){
  return isa<Integer_TypeDeclaration>(Dest);
}
bool Sema::Can_Be_Casted(Expr *Org, TypeDeclaration* Dest){
  auto Org_Ty = Org->getType();
  if(is_int(Org_Ty) && is_int(Dest))
    return true;
  if (isa<PointerTypeDeclaration>(Org_Ty)) {
    if(isa<PointerTypeDeclaration>(Dest)){
      return true;
    }
  } 
  if(isa<PointerTypeDeclaration>(Org_Ty) && Dest == BoolType){
    return true;
  }
  return false;
};
Expr *Sema::Create_Cast(Expr* Orginal, TypeDeclaration* Type_To_Cast){
    return new CastExpr(Orginal,Type_To_Cast);
}
Decl *Sema::Insert_Decl(Decl *D){
  CurrentScope->insert(D);
  return D;
};
Expr *Sema::Cast(Expr *E,TypeDeclaration* Dest){
  if (Can_Be_Casted(E, Dest)) {
    return Create_Cast(E, Dest);
  } else {
  //TODO error out
    return nullptr;
  }
};
Expr *Sema::actOnSizeof(TypeDeclaration* Ty_G,TypeDeclaration* Ty_P){
  auto E = new SizeofExpr(nullptr,IntegerType);
  if (Ty_G && Ty_P) {
    if(Ty_G == Ty_P){
      E->setType(Ty_G);
    } else {
      //TODO error out
    }
  }
  if(Ty_G){
    E->TypeTogetsize=Ty_G;
  } else if (Ty_P) {
    E->TypeTogetsize = Ty_P;
  }
  assert(E->TypeTogetsize);
  return E;
};