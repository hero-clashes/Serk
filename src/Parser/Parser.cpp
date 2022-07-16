#include "Parser.hpp"
#include <string>
#include <utility>
#include <vector>

namespace {
OperatorInfo fromTok(Token Tok) {
  return OperatorInfo(Tok.getLocation(), Tok.getKind());
}
} // namespace

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  Lex.next(Tok);
};

ModuleDeclaration  *Parser::parse(StringRef Name) {
  auto _errorhandler = [this] {
    SkipUntil(tok::eof);
    return nullptr;
  };
  module = Actions.actOnCompileUnitDeclaration(SMLoc::getFromPointer(Lex.getBuffer().begin()), Name);
  // EnterDeclScope S(Actions, module);
  Actions.CurrentDecl = module;
  Actions.CurrentScope->P_Decl = module;
  DeclList Decls;
  StmtList Stmts;
  while (Tok.isNot(tok::eof)) {
    if (Tok.is(tok::kw_import)) {
      if(ParseImport()){
        return _errorhandler();
      };
    };
    if (Tok.is(tok::kw_fn)) {
      if (ParseFuction(Decls)) {
        return _errorhandler();
      };
    }
    if (Tok.is(tok::kw_extern)) {
      if (ParseExternFunction(Decls)) {
        return _errorhandler();
      };
    }
    if (Tok.is(tok::kw_var)) {
      if (parseVarDecleration(Decls, Stmts)) {
        return _errorhandler();
      };
      if(expect(tok::semi)){
        return _errorhandler();
      };
      advance();
    }
    if (Tok.is(tok::kw_class)) {
      if (ParseClass(Decls)) {
        return _errorhandler();
      };
    };
    if (Tok.is(tok::kw_enum)) {
      if (ParseEnum(Decls, Stmts)) {
        return _errorhandler();
      };
    }
    if (Tok.is(tok::kw_using)) {
      if (ParseUsing(Decls)) {
        return _errorhandler();
      };
    }
  }

  Actions.actOnCompileUnitDeclaration(module, SMLoc(), Name , Decls, Stmts);
  imported[Name] = module;
  return module;
};
bool Parser::ParseConstructorOrDecostructor(DeclList &ParentDecls,Decl *Class){
  bool dest = false;
  if(Tok.is(tok::tild)){
    advance();
    dest = true;
  }
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  auto function_name = new std::string(dest ? "Delete":"Create");
  auto function_loc = Tok.getLocation();
  auto D =
      Actions.actOnFunctionDeclaration(Tok.getLocation(), *function_name);
  advance();
  DeclList Decls;
  StmtList Stmts;
                      // eat function_name identifer
  {EnterDeclScope S(Actions, D,Stmts); // added befor the parmeters so the parmeters
                                // get added to the function scope
  if (expect(tok::l_paren)) {
    return _errorhandler();
  };
  ParamList Params;
  if (parseParameters(ParentDecls, Params)) {
    return _errorhandler();
  };

  if (expect(tok::l_parth)) {
    return _errorhandler();
  };
  advance();
  Actions.actOnFunctionHeading(D, Params, Class);

  

  if (parseBlock(Decls, Stmts)) {
    return _errorhandler();
  };}

  ParentDecls.push_back(D);
  Actions.actOnFunctionDeclaration(D, function_loc, *function_name, Decls,
                                   Stmts);
  if (expect(tok::r_parth)) {
    return _errorhandler();
  };
  advance();
  return false;
};
bool Parser::ParseFuction(DeclList &ParentDecls) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  advance(); // eat fn

  TypeDeclaration *RetType;
  if (ParseType(ParentDecls, RetType)) {
    return _errorhandler();
  }


  if (expect(tok::identifier)) { // expect function name
    return _errorhandler();
  };
  DeclList Decls;
  StmtList Stmts;
  auto function_name = Tok.getIdentifier();
  auto function_loc = Tok.getLocation();
  auto D =
      Actions.actOnFunctionDeclaration(Tok.getLocation(), Tok.getIdentifier());
  advance();                    // eat function_name identifer
  {EnterDeclScope S(Actions, D,Stmts); // added befor the parmeters so the parmeters
                                // get added to the function scope
  if (expect(tok::l_paren)) {
    return _errorhandler();
  };
  ParamList Params;
  if (parseParameters(ParentDecls, Params)) {
    return _errorhandler();
  };

  if (expect(tok::l_parth)) {
    return _errorhandler();
  };
  advance();
  Actions.actOnFunctionHeading(D, Params, RetType);

 

  if (parseBlock(Decls, Stmts)) {
    return _errorhandler();
  };}

  ParentDecls.push_back(D);
  Actions.actOnFunctionDeclaration(D, function_loc, function_name, Decls,
                                   Stmts);
  if (expect(tok::r_parth)) {
    return _errorhandler();
  };
  advance();
  return false;
}
bool Parser::ParseExternFunction(DeclList &ParentDecls){
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren}); };
  advance();
  TypeDeclaration *RetType;
  if (ParseType(ParentDecls, RetType)) {
    return _errorhandler();
  }


  if (expect(tok::identifier)) { // expect function name
    return _errorhandler();
  };

  auto function_name = Tok.getIdentifier();
  auto function_loc = Tok.getLocation();
  auto D =
      Actions.actOnFunctionDeclaration(Tok.getLocation(), Tok.getIdentifier());
  advance();                    // eat function_name identifer
  StmtList a;
  EnterDeclScope S(Actions, D,a); // added befor the parmeters so the parmeters
                                // get added to the function scope
  if (expect(tok::l_paren)) {
    return _errorhandler();
  };
  ParamList Params;
  if (parseParameters(ParentDecls, Params)) {
    return _errorhandler();
  };

  Actions.actOnFunctionHeading(D, Params, RetType);
  D->Type = FunctionDeclaration::Extern;
  ParentDecls.push_back(D);
  return false;
};
bool Parser::parseParameters(DeclList &ParentDecls, ParamList &Params) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren}); };
  consume(tok::l_paren);
  while (Tok.isOneOf(tok::identifier, tok::kw_ref)) {
    if (parseParameter(ParentDecls ,Params)) {
      return _errorhandler();
    };
    if (!Tok.isOneOf(tok::comma, tok::r_paren)) {
      return _errorhandler();
    }
    consume(tok::comma);
    if (Tok.is(tok::r_paren)) {
      break;
    }
  };
  consume(tok::r_paren);
  if(Tok.is(tok::semi)) advance();
  return false;
};
bool Parser::parseParameter(DeclList &ParentDecls,ParamList &Params) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren, tok::semi}); };
  bool by_refernce = false;
  
  if (expect(tok::identifier)) {
    return _errorhandler();
  };
  auto Name = Tok;
  advance();
  if (expect(tok::colon)) {
    return _errorhandler();
  }
  advance();

  if (Tok.is(tok::kw_ref)) {
    by_refernce = true;
    advance();
  }
  TypeDeclaration *type_D;
  if(ParseType(ParentDecls , type_D)){
    return _errorhandler();
  }
  auto Parem = Actions.actOnParmaDecl(Name.getLocation(), Name.getIdentifier(),
                                      type_D, by_refernce);
  consume(tok::identifier);
  Params.push_back(Parem);
  return false;
}
bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  while (Tok.isNot(tok::r_parth)) {
    if (Tok.is(tok::kw_var)) {
      // parse var defintion
      if (parseVarDecleration(Decls, Stmts)) {
        return _errorhandler();
      };
      if(expect(tok::semi)){
        return _errorhandler();
      };

      advance();
    } else if (Tok.is(tok::kw_enum)) {
      if (ParseEnum(Decls, Stmts)) {
        return _errorhandler();
      };
    } else {
      // parse statements
      if (parseStatementSequence(Decls, Stmts)) {
        return _errorhandler();
      };
    }
  };
  return false;
}
bool Parser::parseVarDecleration(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi}); };
  advance(); // eat var
  std::vector<Token> Var_Names;
  while (Tok.is(tok::identifier)) {
    Var_Names.push_back(Tok);
    advance();
    if (Tok.isOneOf(tok::colon,tok::equal)) {
      break;
    }
    if(expect(tok::comma)){
      return _errorhandler();
    }
    advance();
  }

  TypeDeclaration *type_D = nullptr;
  if (Tok.is(tok::colon)) {
    advance();
    if(ParseType(Decls, type_D)){
      return _errorhandler();
    };
  };

  Expr *Desig = nullptr;
  std::vector<Expr*> Es;
  std::vector<SMLoc> Locs;
  if (Tok.is(tok::equal)) {
    advance();

    do  {
      Expr* E;
      Locs.push_back(Tok.getLocation());

      if (parseExpression(E)) {
        return _errorhandler();
      };
      Es.push_back(E);
    } while(Tok.is(tok::comma));
  }
  if (!type_D && (!Es.empty()) &&Es[0]) {
    type_D = Es[0]->getType();
  }
  for (int i = 0; i < Var_Names.size(); i++) {
    auto var = Var_Names[i];
    Expr* E = i < Es.size() ? Es[i] : nullptr;

    auto Var = Actions.actOnVarDeceleration(
        var.getLocation(), var.getIdentifier(), type_D, E ? true : false);
    if (E) {
      Desig = Actions.actOnDesignator(Var);
      Actions.actOnAssignment(Stmts, Locs[i], Desig, E);
    }
    Decls.push_back(Var);
  }
  return false;
}
bool Parser::parseTemepleteList(DeclList & Decls,TypeDeclaration* &type_D,std::vector<std::variant<TypeDeclaration *, Expr *>> &Args){
  auto _errorhandler = [this] { return SkipUntil({tok::greater}); };
  advance();//eat tok::less
  if (expect(tok::identifier)) {
    return _errorhandler();
  }
   do {
        if (Tok.isOneOf(tok::identifier,tok::star)) {
          TypeDeclaration* Ty;
          if(ParseType(Decls, Ty)){
            return _errorhandler();
          }
          Args.push_back(Ty);
        } else {
          Expr *EXP;
          if(parseSimpleExpression(EXP)) {
            return _errorhandler();
          };
          Args.push_back(EXP);
        }
        if (Tok.is(tok::comma))
          advance();
      } while (Tok.isNot(tok::greater));
      advance();
      type_D =
          (TypeDeclaration *)Actions.init_genric_class(Decls, type_D, Args, Tok.getLocation());
  return false;
};
bool Parser::parseStatement(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi,tok::kw_else,tok::r_parth}); };
  switch (Tok.getKind()) {
  case tok::kw_return:
    if(parseReturnStatement(Decls, Stmts)){
      return _errorhandler();
    };
    break;
  case tok::star:
  case tok::identifier:
    {
    bool derefernce = false;
    if(Tok.is(tok::star)){
      derefernce = true;
      advance();
    }
    if (Lex.peak(0).is(tok::l_paren)) {
      if(parseFunctionCallStatment(Stmts)){
        return _errorhandler();
      };
    } else {
      auto Var =
          Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
      advance(); // eat var
      auto Desig = Actions.actOnDesignator(Var);
      if (auto d =  dyn_cast_or_null<Designator>(Desig)) {
        if (derefernce) d->Derfernced();
      }
      if(parseSelectors(Desig)){
        return _errorhandler();
      };
      Expr *E;
      if (Tok.is(tok::period)) {
        if(ParseMethodCallStatment(Stmts, Desig)){
          return _errorhandler();
        };
      } else {
        if(expect(tok::equal)){
          return _errorhandler();
        }
        advance(); // eat =
        if(parseExpression(E)){
          return _errorhandler();
        };
        Actions.actOnAssignment(Stmts, Tok.getLocation(), Desig, E);
      }
    }
    break;
    }
  case tok::kw_if:
    // parse if
    if(parseIfStatement(Decls, Stmts)){
      return _errorhandler();
    };
    break;
  case tok::kw_while:
    // parse while
    if(parseWhileStatement(Decls, Stmts)){
      return _errorhandler();
    };
    break;
  case tok::kw_for:
    if(parseForStatement(Decls, Stmts)){
      return _errorhandler();
    };
    break;
  case tok::kw_var:
    if(parseVarDecleration(Decls, Stmts)){
      return _errorhandler();
    }
    break;
  default:
    break;
  }
  return false;
}

bool Parser::parseStatementSequence(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::kw_else,tok::r_parth}); };
  if(parseStatement(Decls, Stmts)){
    return _errorhandler();
  };
  if(expect(tok::semi)){
    return _errorhandler();
  };
  while (Tok.is(tok::semi)) {
    advance();
    if (Tok.isOneOf(tok::r_parth,tok::r_paren)) {
      break;
    }
    if(parseStatement(Decls, Stmts)){
      return _errorhandler();
    };
  }
  return false;
}

bool Parser::parseReturnStatement(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi}); };
  Expr *E = nullptr;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_return);
  if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                  tok::integer_literal,tok::star)) {
    if(parseExpression(E)){
      return _errorhandler();
    };
  }
  Actions.actOnReturnStatement(Stmts, Loc, E);
  // if(expect(tok::semi)){
  //   return _errorhandler();
  // };

  // advance();

  return false;
};

bool Parser::parseFunctionCallStatment(StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi}); };
  ExprList Exprs;
  Decl *D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
  auto loc = Tok.getLocation();
  advance();
  if(expect(tok::l_paren)){
    return _errorhandler();
  };

  advance();
  if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                  tok::integer_literal, tok::string_literal)) {
    if(parseExpList(Exprs)){
      return _errorhandler();
    };
  } 
  if(expect(tok::r_paren)){
    return _errorhandler();
  };
  auto Statment = Actions.actOnFunctionCallStatemnt(loc, D, Exprs);
  Stmts.push_back(Statment);
  advance();
  
  return false;
};
bool Parser::parseExpList(ExprList &Exprs) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren}); };
  Expr *E = nullptr;
  if(parseExpression(E)){
    return _errorhandler();
  };
  if (E)
    Exprs.push_back(E);
  while (Tok.is(tok::comma)) {
    E = nullptr;
    advance();
    if(parseExpression(E)){
      return _errorhandler();
    };
    if (E)
      Exprs.push_back(E);
  }
  return false;
}

bool Parser::parseExpression(Expr *&E) {
  auto _errorhandler = [this] {
    return SkipUntil({tok::r_paren, tok::comma, tok::semi, tok::kw_else,
                      tok::r_parth, tok::r_square});
  };
  if(parseSimpleExpression(E)){
    return _errorhandler();
  };
  if (Tok.isOneOf(tok::less, tok::lessequal, tok::equal_equal, tok::greater,
                  tok::not_equal, tok::Not, tok::And, tok::Or,
                  tok::greaterequal)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if(parseRelation(Op)){
      return _errorhandler();
    };
    if(parseSimpleExpression(Right)){
      return _errorhandler();
    };
    E = Actions.actOnExpression(E, Right, Op);
  }
  return false;
}
bool Parser::parseSimpleExpression(Expr *&E) {
  auto _errorhandler = [this] {
    return SkipUntil(
        {tok::r_paren, tok::comma, tok::semi, // add new operators here
         tok::less, tok::lessequal, tok::equal, tok::greater, tok::greaterequal,
         tok::kw_else, tok::r_parth, tok::r_square});
  };
  OperatorInfo PrefixOp;
  if (Tok.isOneOf(tok::plus, tok::minus)) {
    if (Tok.is(tok::plus)) {
      PrefixOp = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::minus)) {
      PrefixOp = fromTok(Tok);
      advance();
    }
  }
  if(parseTerm(E)){
    return _errorhandler();
  };
  while (Tok.isOneOf(tok::plus, tok::minus)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if(parseAddOperator(Op)){
      return _errorhandler();
    };

    if(parseTerm(Right)){
      return _errorhandler();
    };

    E = Actions.actOnSimpleExpression(E, Right, Op);
  }
  if (!PrefixOp.isUnspecified())

    E = Actions.actOnPrefixExpression(E, PrefixOp);
  return false;
}
bool Parser::parseTerm(Expr *&E) {
  auto _errorhandler = [this] {
    return SkipUntil({tok::r_paren, tok::plus,
                      tok::comma, tok::Reminder, // add new oprators here
                      tok::minus, tok::semi, tok::less, tok::lessequal,
                      tok::equal, tok::greater, tok::greaterequal, tok::kw_else,
                      tok::r_parth, tok::Or, tok::r_square});
  };
  if(parseFactor(E)){
    return _errorhandler();
  };
  while (Tok.isOneOf(tok::star, tok::slash,tok::Reminder)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if(parseMulOperator(Op)){
      return _errorhandler();
    };
    if(parseFactor(Right)){
      return _errorhandler();
    };
    E = Actions.actOnTerm(E, Right, Op);
  }
  return false;
}
bool Parser::parseFactor(Expr *&E) {
  auto _errorhandler = [this] {
    return SkipUntil(
        {tok::r_paren, tok::star, tok::plus, tok::Reminder, // new oprators get added here
         tok::comma, tok::minus, tok::slash, tok::semi, tok::less,
         tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::And,
         tok::kw_else, tok::r_parth, tok::Or, tok::r_square});
  };
  if (Tok.is(tok::float_literal)) {
    E = Actions.actOnFloatLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else if (Tok.is(tok::integer_literal)) {
    E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else if (Tok.is(tok::identifier)) {
    Decl *D;
    ExprList Exprs;
    //   if (parseQualident(D))//TODO add this again with support for name spaces
    //     goto _error;
    auto call = Tok;
    D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
    advance();
    if (Tok.is(tok::l_paren)) {
      // here function calls handling
      advance();
        if(parseExpList(Exprs)){
          return _errorhandler();
      }
      if(expect(tok::r_paren)){
        return _errorhandler();
      };
      if(D->getName().endswith("Create"))
      E = Actions.actOnConstructorCallExpr(call.getLocation(), D, Exprs);
      else
      E = Actions.actOnFunctionCallExpr(call.getLocation(), D, Exprs);
      advance();
    } else {
      E = Actions.actOnDesignator(D);

      if(parseSelectors(E)){
        return _errorhandler();
      };

      if (Tok.is(tok::period)) {
        advance();
        if(expect(tok::identifier)){
          return _errorhandler();
        }
        auto Method_name = Tok;
        ExprList Exprs;
        advance();
        if (Tok.is(tok::l_paren)) {
          advance();
          if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                          tok::integer_literal)) {
            if(parseExpList(Exprs)){
              return _errorhandler();
            };
          }
          if(expect(tok::r_paren)){
            return _errorhandler();
          };
          advance();
          E = Actions.actOnMethodCallExpr(Method_name.getLocation(), D, Method_name.getIdentifier(), Exprs);
        }
      }
    }
  } else if (Tok.is(tok::l_paren)) {
    if (Lex.peak(0).is(tok::identifier) && Lex.peak(1).is(tok::r_paren)) {
      advance();
      TypeDeclaration *T;
      DeclList a;
      if(ParseType(a,T)){
        return _errorhandler();
      }
      if (expect(tok::r_paren)) {
        return _errorhandler();
      }
      advance();
      Expr *EE;
      if(parseExpression(EE)){
        return _errorhandler();
      };
      E = Actions.Cast(EE, T);
    } else {
      advance();
      if (parseExpression(E)) {
        return _errorhandler();
      };
      if (expect(tok::r_paren)) {
        return _errorhandler();
      };
      advance();
    }
  } else if (Tok.is(tok::Not)) {
    OperatorInfo Op = fromTok(Tok);
    advance();
    if(parseFactor(E)){
      return _errorhandler();
    };
    E = Actions.actOnPrefixExpression(E, Op);
  } else if (Tok.is(tok::string_literal)) {
    // todo move stuff to the sema
    E = Actions.actOnStringLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else if (Tok.is(tok::Amper)) {
    auto loc =Tok.getLocation();
    advance();
    Decl *D;
    D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
    advance();
    E = Actions.actOnDesignator(D);

    if(parseSelectors(E)){
      return _errorhandler();
    };
    auto dis = dyn_cast<Designator>(E);
    dis->Get_Pointer();
    dis->setType(Actions.Get_Pointer_Type(dis->getType()));
  } else if (Tok.is(tok::star)) {
    auto loc =Tok.getLocation();
    advance();
    Decl *D;
    D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
    advance();
    E = Actions.actOnDesignator(D);

    if(parseSelectors(E)){
      return _errorhandler();
    };
    dyn_cast<Designator>(E)->Derfernced();    
  } else if (Tok.is(tok::kw_sizeof)) {
    advance();
    TypeDeclaration* Ty_G = nullptr;
    TypeDeclaration* Ty_P = nullptr;
    if(Tok.is(tok::less)){
      advance();
      DeclList a;
      if (ParseType(a, Ty_G)) {
        return _errorhandler();
      }
      if (expect(tok::greater)) {
        return _errorhandler();
      }
      advance();
    }
    if (expect(tok::l_paren)) {
        return _errorhandler();
    }
    advance();
    if (Tok.is(tok::r_paren)) {
      advance();
    } else {
      Expr *EE;
      if (parseExpression(EE)) {
        return _errorhandler();
      }
      Ty_P = EE->getType();
    }
    E = Actions.actOnSizeof(Ty_G, Ty_P);
  } else {
    // return _errorhandler();
  }
  return false;
}

bool Parser::parseRelation(OperatorInfo &Op) {
  auto _errorhandler = [this] {
    return SkipUntil(
        {tok::l_paren, tok::plus, tok::minus, tok::Not,
      tok::identifier, tok::integer_literal});
  };
  if (Tok.is(tok::Not)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::not_equal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::equal_equal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::less)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::lessequal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::greater)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::greaterequal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::Or)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::And)) {
    Op = fromTok(Tok);
    advance();
  } else {
    return _errorhandler();
  }
  return false;
}
bool Parser::parseMulOperator(OperatorInfo &Op) {
  {
    auto _errorhandler = [this] {
      return SkipUntil({tok::l_paren, tok::plus, tok::minus, tok::Not,
                        tok::identifier, tok::integer_literal});
    };
    if (Tok.is(tok::star)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::slash)) {
      Op = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::Reminder)) {
      Op = fromTok(Tok);
      advance();
    } 
    // else if (Tok.is(tok::kw_DIV)) {
    //   Op = fromTok(Tok);
    // advance();
    //}
    // else if (Tok.is(tok::kw_MOD)) {
    //  Op = fromTok(Tok);
    // advance();
    //}
    else if (Tok.is(tok::And)) {
      Op = fromTok(Tok);
      advance();
    }
    else {
    return _errorhandler();
    }
  }
  return false;
}

bool Parser::parseAddOperator(OperatorInfo &Op) {
  auto _errorhandler = [this] {
      return SkipUntil({tok::l_paren, tok::Not,
                      tok::identifier,
                      tok::integer_literal});
  };
  if (Tok.is(tok::plus)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::minus)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::Or)) {
    Op = fromTok(Tok);
    advance();
  }
  else {
    return _errorhandler();
  }
  return false;
}

bool Parser::parseIfStatement(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] {
      return SkipUntil({tok::semi, tok::kw_else,
                      tok::r_parth});
  };
  Expr *E = nullptr;
  StmtList IfStmts, ElseStmts;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_if);
 { 
  EnterDeclScope s(Actions,IfStmts);
  if(expect(tok::l_paren)){
    return _errorhandler();
  };
  advance();

  if(parseExpression(E)){
    return _errorhandler();
  };

  if(expect(tok::r_paren)){
    return _errorhandler();
  };
  advance();

  if (Tok.is(tok::l_parth)) {
    if (expect(tok::l_parth)) {
      return _errorhandler();
    };
    advance();

    if (parseStatementSequence(Decls, IfStmts)) {
      return _errorhandler();
    };
    if (expect(tok::r_parth)) {
      return _errorhandler();
    };
    advance();
  } else {
    if(parseStatement(Decls, IfStmts)){
        return _errorhandler();
    };
  }
  }

  if (Tok.is(tok::kw_else)) {
    advance();
    EnterDeclScope s(Actions,ElseStmts);
    if (Tok.isNot(tok::l_parth)) {
      if(parseStatement(Decls, ElseStmts)){
        return _errorhandler();
      };
    } else {
      if(expect(tok::l_parth)){
        return _errorhandler();
      };
      advance();
      if(parseBlock(Decls, ElseStmts)){

      };

      if(expect(tok::r_parth)){
        return _errorhandler();
      };

      advance();
    }
  }
  Actions.actOnIfStatement(Stmts, Loc, E, IfStmts, ElseStmts);
  return false;
};
bool Parser::parseWhileStatement(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] {
      return SkipUntil({tok::semi,
                      tok::r_parth});
  };
  Expr *E = nullptr;
  StmtList WhileStmts;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_while);
  {EnterDeclScope s(Actions,WhileStmts);
  if(expect(tok::l_paren)){
    return _errorhandler();
  };

  advance();

  if(parseExpression(E)){
    return _errorhandler();
  };

  if(expect(tok::r_paren)){
    return _errorhandler();
  };

  advance();

  if(expect(tok::l_parth)){
    return _errorhandler();
  };

  advance();

  if(parseBlock(Decls, WhileStmts)){
    return _errorhandler();
  };

  if(expect(tok::r_parth)){
    return _errorhandler();
  };}
  advance();
  Actions.actOnWhileStatement(Stmts, Loc, E, WhileStmts);
  return false;
};
bool Parser::parseForStatement(DeclList &Decls, StmtList &Stmts) {
  auto _errorhandler = [this] {
      return SkipUntil({tok::semi,
                      tok::r_parth});
  };

  Expr *E = nullptr;
  StmtList Start_Val;
  StmtList ForStepStmts;
  StmtList ForBodyStmts;

  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_for);
  EnterDeclScope s(Actions,Stmts);
  if(expect(tok::l_paren)){
    return _errorhandler();
  };
  advance();
  
  if(parseVarDecleration(Decls, Start_Val)){
    return _errorhandler();
  };
  if(expect(tok::semi)){
    return _errorhandler();
  };

  advance();
  if(parseExpression(E)){
    return _errorhandler();
  };

  if(expect(tok::semi)){
    return _errorhandler();
  };
  advance();

  if(parseStatementSequence(Decls, ForStepStmts)){
    return _errorhandler();
  };

  if(expect(tok::r_paren)){
    return _errorhandler();
  };
  advance();

  if(expect(tok::l_parth)){
    return _errorhandler();
  };
  advance();

  if(parseBlock(Decls, ForBodyStmts)){
    return _errorhandler();
  };

  if(expect(tok::r_parth)){
    return _errorhandler();
  };
  advance();
  Actions.actOnForStatement(Stmts, Loc, E, Start_Val, ForStepStmts,
                            ForBodyStmts);
  return false;
}
bool Parser::ParseClass(DeclList &ParentDecls) {
  auto _errorhandler = [this] {
      return SkipUntil({tok::semi,
                      tok::r_parth});
  };

  bool Genric = false;
  advance(); // eat class
  if (expect(tok::identifier)) {
    return _errorhandler();
  }
  auto Class_Name = Tok;
  advance();
  Decl *D;
  std::vector<std::tuple<int, StringRef, TypeDeclaration *, SMLoc>> List;
  if (Tok.is(tok::less)) {

    if(ParseTempleteArgs(ParentDecls,List)){
      return _errorhandler();
    };
    D = Actions.actOnClassDeclaration(Class_Name.getLocation(),
                                      Class_Name.getIdentifier(), true);
    Genric = true;
  } else {
    D = Actions.actOnClassDeclaration(Class_Name.getLocation(),
                                      Class_Name.getIdentifier(), false);
  };
  StmtList s;
  EnterDeclScope S(Actions, D,s);
  DeclList Decls;
  StmtList StartStmt;
  if (Genric) {
    for (auto D : List) {
      switch (std::get<0>(D)) {
      case 0: {
        Actions.Create_Genric_type(std::get<1>(D), std::get<3>(D));
      } break;
      case 1: {
        Actions.Create_Genric_Var(Decls, std::get<1>(D), std::get<3>(D),
                                  std::get<2>(D));
      } break;
      }
    }
  }

  if(expect(tok::l_parth)){
    return _errorhandler();
  };
  advance();
  while (Tok.isNot(tok::r_parth)) {
    if (Tok.is(tok::kw_fn)) {
      // handle function decleration
      //
      if (ParseFuction(Decls)) {
        return _errorhandler();
      };
    } else if (Tok.is(tok::kw_var)) {
      // handle var decleration
      if(parseVarDecleration(Decls, StartStmt)){
        return _errorhandler();
      };
      if(expect(tok::semi)){
        return _errorhandler();
      }
      advance();
    } else if (Tok.is(tok::identifier)) {
      if(Tok.getIdentifier() != Class_Name.getIdentifier()){
        //TODO error out
      } else {
        if(ParseConstructorOrDecostructor(Decls,D)){
          return _errorhandler();
        }
        static_cast<ClassDeclaration*>(D)->has_constructor = true;
      }
    } else if (Tok.is(tok::tild)) {

        if(ParseConstructorOrDecostructor(Decls,D)){
          return _errorhandler();
        }
        static_cast<ClassDeclaration*>(D)->has_constructor = true;
      
    }
  }
  if(expect(tok::r_parth)){
    return _errorhandler();
  };
  advance();

  Actions.actOnClassBody(D, Decls, StartStmt);
  // D->Decls =Decls;
  // D->Stmts = StartStmt;

  ParentDecls.push_back(D);
  return false;
};
bool Parser::ParseMethodCallStatment(StmtList &Stmts, Expr *E) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi}); };
  advance();
  if(expect(tok::identifier)){
    return _errorhandler();
  }
  auto Method_name = Tok.getIdentifier();
  auto loc = Tok.getLocation();
  ExprList Exprs;
  advance();

  if (Tok.is(tok::l_paren)) {
    advance();
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                    tok::integer_literal)) {
      if(parseExpList(Exprs)){
        return _errorhandler();
      };
    }
    if(expect(tok::r_paren)){
      return _errorhandler();
    };
    advance();
  }
  Stmts.push_back(new MethodCallStatement(E, Method_name, Exprs, loc));
  //TODO add error checking
  return false;
};
bool Parser::ParseEnum(DeclList &ParentDecls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  advance(); // eat enum
  TypeDeclaration *Ty = Actions.IntegerType;
  if (Tok.is(tok::colon)) {
    advance();
    if(ParseType(ParentDecls, Ty)){
      return _errorhandler();
    }
  };
  if(Tok.is(tok::identifier)) advance();
  if(expect(tok::l_parth)){
    return _errorhandler();
  };
  advance();
  std::vector<std::pair<Token,Expr*>> idents;
  while (Tok.is(tok::identifier)) {
    auto Name = Tok;
    advance();
    Expr* E = nullptr;
    if(Tok.is(tok::equal)){
      advance();
      if (parseExpression(E)) {
        return _errorhandler();
      }
      if (!E) {
      //TODO error out
      } else {
        if(!E->isConst()){
          //TODO error out
        }
      }
    }
    idents.push_back({Name,E});
    if(expect(tok::comma)){
      return _errorhandler();
    };
    advance();
  }
  int num = 0;
  for (auto p : idents) {
    auto iden = p.first;
    auto E = p.second;
    //TODO check if e is const
    
    Actions.actOnConstantDeclaration(
        ParentDecls, iden.getLocation(), iden.getIdentifier(),
        E ? E:Actions.actOnIntegerLiteral(iden.getLocation(), num));
    if(E) num = static_cast<IntegerLiteral*>(E)->getValue().getExtValue();
    num++;
  }
  if(expect(tok::r_parth)){
    return _errorhandler();
  };
  advance();
  return false;
}

bool Parser::ParseUsing(DeclList &ParentDecls) {
  auto _errorhandler = [this] { return SkipUntil({tok::semi},true); };
  advance(); // eat using

  if(expect(tok::identifier)){
    return _errorhandler();
  };
  auto Aliased_name = Tok;
  advance();

  if(expect(tok::equal)){
    return _errorhandler();
  };
  advance();

  TypeDeclaration *Type;
  if(ParseType(ParentDecls, Type)){
    return _errorhandler();
  }

  if(expect(tok::identifier)){
    return _errorhandler();
  };
  Actions.actOnAliasTypeDeclaration(ParentDecls, Aliased_name.getLocation(),
                                    Aliased_name.getIdentifier(), Type);
  advance();

  if(expect(tok::semi)){
    return _errorhandler();
  };
  advance();
  return false;
};
bool Parser::parseSelectors(Expr *&E) {
  auto _errorhandler = [this] {
    return SkipUntil({tok::r_paren, tok::star, tok::plus, tok::comma,
                      tok::minus, tok::slash, tok::equal_equal, tok::semi,
                      tok::less, tok::lessequal, tok::equal, tok::greater,
                      tok::greaterequal, tok::And, // add op
                      tok::kw_else, tok::Or, tok::r_square});
  };
  while (Tok.isOneOf(tok::period, tok::l_square)) {
    if (Tok.is(tok::l_square)) {
      SMLoc Loc = Tok.getLocation();
      Expr *IndexE = nullptr;
      advance();
      if(parseExpression(IndexE)){
        return _errorhandler();
      };
      if(expect(tok::r_square)){
        return _errorhandler();
      };
      Actions.actOnIndexSelector(E, Loc, IndexE);
      advance();
    } else if (Tok.is(tok::period)) {
      if (Lex.peak(1).is(tok::l_paren))
        return false;
      advance();
      if(expect(tok::identifier)){
        return _errorhandler();
      };
      Actions.actOnFieldSelector(E, Tok.getLocation(), Tok.getIdentifier());
      advance();
    }
  }
  return false;
}
bool Parser::ParseTempleteArgs(
    DeclList &ParentDecls,std::vector<std::tuple<int, StringRef, TypeDeclaration *, SMLoc>> &Decls) {
  auto _errorhandler = [this] { return SkipUntil({tok::greater},true); };
  consume(tok::less);
  while (Tok.isOneOf(tok::kw_var, tok::kw_type)) {
    if (Tok.is(tok::kw_type)) {
      advance();
      if(expect(tok::identifier)){
        return _errorhandler();
      };
      Decls.push_back({0, Tok.getIdentifier(), nullptr, Tok.getLocation()});
      advance();
    } else if (tok::kw_var) {
      advance();
      if(expect(tok::identifier)){
        return _errorhandler();
      };
      auto name = Tok.getIdentifier();
      auto Loc = Tok.getLocation();
      advance();

      if(expect(tok::colon)){
        return _errorhandler();
      };
      advance();
      TypeDeclaration *Ty;
      if(ParseType(ParentDecls, Ty)){
        return _errorhandler();
      }
      Decls.push_back({1, name, Ty, Loc});
    }
    if (!Tok.isOneOf(tok::comma, tok::greater)) {
      // TODO error out
    }
    if (Tok.is(tok::greater)) {
      break;
    }
    if(expect(tok::comma)){
      return _errorhandler();
    };
    advance();
    
  };
  consume(tok::greater);
  return false;
};
bool Parser::SkipUntil(ArrayRef<tok::TokenKind> Toks,bool eat) {

  while (true) {
    // If we found one of the tokens, stop and return true.
    for (unsigned i = 0, NumToks = Toks.size(); i != NumToks; ++i) {
      if (Tok.is(Toks[i])) {
        //   if (HasFlagsSet(Flags, StopBeforeMatch)) {
        //   // Noop, don't consume the token.
        // } else {
        //   ConsumeAnyToken();
        // }
        if (eat) {
          advance();
        }
        return true;
      }
    }
    // Important special case: The caller has given up and just wants us to
    // skip the rest of the file. Do this without recursing, since we can
    // get here precisely because the caller detected too much recursion.
    if (Toks.size() == 1 && Toks[0] == tok::eof) {
      while (Tok.isNot(tok::eof))
        advance();
      return true;
    }
    switch (Tok.getKind()) {
    case tok::eof:
      // Ran out of tokens.
      return false;
      break;
    default:
      advance();
    }
  }
}
bool Parser::ParseType(DeclList &ParentDecls, TypeDeclaration *&Ty) {
  auto _errorhandler = [this] { return SkipUntil({tok::identifier}); };
    bool PointerTy = false;
    if (Tok.is(tok::star)) {
      PointerTy = true;
      advance();
    }
    if(expect(tok::identifier)){
      return _errorhandler();
    }
    Ty = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
    if (Tok.is(tok::less)) {
      std::vector<std::variant<TypeDeclaration *, Expr *>> Args;
      if (parseTemepleteList(ParentDecls, Ty, Args)) {
        return _errorhandler();
      };
    }
    if (Tok.is(tok::l_square)) {
      advance();
      Expr *E = nullptr;
      auto Loc = Tok.getLocation();
      if (parseExpression(E)) {
        return _errorhandler();
      };
      ;
      Ty = Actions.actOnArrayTypeDeclaration(ParentDecls, Loc, E, Ty);
      if (expect(tok::r_square)) {
        return _errorhandler();
      };
      advance();
    }
    if (PointerTy) {
      Ty = Actions.Get_Pointer_Type(Ty);
    }
  return false;
};
bool Parser::ParseImport(){
  auto _errorhandler = [this] { return SkipUntil({tok::kw_fn,tok::kw_class,tok::kw_enum,tok::kw_import}); };
  advance(); //eat import
  if(expect(tok::string_literal)){
    return _errorhandler();
  }
  auto loc = Tok.getLocation();
  auto str = Tok.getLiteralData();
  advance();
  
  
  str.consume_back("\"");
  str.consume_front("\"");
  auto new_name = new std::string((module->Name.slice(0, module->Name.find_last_of("/\\") + 1) + str).str());
  if(imported.find(*new_name) != imported.end()){
    module->Imported_Module.push_back(imported[str]);
    return false;
  }
  auto new_lex = Lex.includefile(str, loc);
  if(new_lex.getCurBuffer() == Lex.getCurBuffer()){
    return _errorhandler();
  }
  auto new_parser = Parser(new_lex,Actions);
  auto m = new_parser.parse(*new_name);
  imported[*new_name] = m;
  module->Imported_Module.push_back(m);
  return false;
}