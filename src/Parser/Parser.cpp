#include "Parser.hpp"
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

CompileUnitDeclaration *Parser::parse() {
  auto _errorhandler = [this] {
    SkipUntil(tok::eof);
    return nullptr;
  };
  CompileUnitDeclaration *module = nullptr;
  module = Actions.actOnCompileUnitDeclaration(SMLoc(), "Main");
  EnterDeclScope S(Actions, module);
  DeclList Decls;
  StmtList Stmts;
  while (Tok.isNot(tok::eof)) {
    while (Tok.is(tok::kw_import)) {
      // handle_import()
    };
    if (Tok.is(tok::kw_fn)) {
      if (ParseFuction(Decls)) {
        return _errorhandler();
      };
    }
    if (Tok.is(tok::kw_var)) {
      if (parseVarDecleration(Decls, Stmts)) {
        return _errorhandler();
      };
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

  Actions.actOnCompileUnitDeclaration(module, SMLoc(), "Main", Decls, Stmts);

  return module;
};

bool Parser::ParseFuction(DeclList &ParentDecls) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  advance(); // eat fn
  bool returnref = false;
  if (Tok.is(tok::kw_ref)) {
    advance();
    returnref = true;
  };
  if (expect(tok::identifier)) {
    return _errorhandler();
  };
  auto type = Tok.getIdentifier();
  auto RetType =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  advance(); // eat type identifer

  if (expect(tok::identifier)) { // expect function name
    return _errorhandler();
  };

  auto function_name = Tok.getIdentifier();
  auto function_loc = Tok.getLocation();
  auto D =
      Actions.actOnFunctionDeclaration(Tok.getLocation(), Tok.getIdentifier());
  D->ReturnRef = returnref;
  advance();                    // eat function_name identifer
  EnterDeclScope S(Actions, D); // added befor the parmeters so the parmeters
                                // get added to the function scope
  if (expect(tok::l_paren)) {
    return _errorhandler();
  };
  ParamList Params;
  if (parseParameters(Params)) {
    return _errorhandler();
  };

  if (expect(tok::l_parth)) {
    return _errorhandler();
  };
  advance();
  Actions.actOnFunctionHeading(D, Params, RetType);

  DeclList Decls;
  StmtList Stmts;

  if (parseBlock(Decls, Stmts)) {
    return _errorhandler();
  };

  ParentDecls.push_back(D);
  Actions.actOnFunctionDeclaration(D, function_loc, function_name, Decls,
                                   Stmts);
  if (expect(tok::r_parth)) {
    return _errorhandler();
  };
  advance();
  return false;
}

bool Parser::parseParameters(ParamList &Params) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren}); };
  consume(tok::l_paren);
  while (Tok.isOneOf(tok::identifier, tok::kw_ref)) {
    if (parseParameter(Params)) {
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
  return false;
};
bool Parser::parseParameter(ParamList &Params) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_paren, tok::semi}); };
  bool by_refernce = false;
  if (Tok.is(tok::kw_ref)) {
    by_refernce = true;
    advance();
  }
  auto type_D =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  consume(tok::identifier);
  if (expect(tok::identifier)) {
    return _errorhandler();
  };
  auto Parem = Actions.actOnParmaDecl(Tok.getLocation(), Tok.getIdentifier(),
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
  auto var = Tok;

  if (expect(tok::identifier)) {
    return _errorhandler();
  };
  consume(tok::identifier);
  TypeDeclaration *type_D = nullptr;
  if (Tok.is(tok::colon)) {
    advance();
    if (expect(tok::identifier)) {
      return _errorhandler();
    };
    type_D = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
    if (Tok.is(tok::less)) {
      std::vector<std::variant<TypeDeclaration *, Expr *>> Args;
      if (parseTemepleteList(Decls, type_D, Args)){
        return _errorhandler();
      };
    } else if (Tok.is(tok::l_square)) {
      advance();
      Expr *E = nullptr;
      auto Loc = Tok.getLocation();
      if(parseExpression(E)){
        return _errorhandler();
      };;
      type_D = Actions.actOnArrayTypeDeclaration(Decls, Loc, E, type_D);
      if (expect(tok::r_square)) {
        return _errorhandler();
      };
      advance();
    }
  };

  Expr *Desig = nullptr;
  Expr *E = nullptr;
  SMLoc Loc;
  if (Tok.is(tok::equal)) {
    advance();
    Loc = Tok.getLocation();

    if(parseExpression(E)){
      return _errorhandler();
    };
  }
  if(expect(tok::semi)){
    return _errorhandler();
  };

  advance();
  if (!type_D) {
    type_D = E->getType();
  }
  auto Var = Actions.actOnVarDeceleration(
      var.getLocation(), var.getIdentifier(), type_D, E ? true : false);
  if (E) {
    Desig = Actions.actOnDesignator(Var);
    Actions.actOnAssignment(Stmts, Loc, Desig, E);
  }
  Decls.push_back(Var);
  return false;
}
bool Parser::parseTemepleteList(DeclList & Decls,TypeDeclaration* &type_D,std::vector<std::variant<TypeDeclaration *, Expr *>> &Args){
  auto _errorhandler = [this] { return SkipUntil({tok::greater}); };
  advance();//eat tok::less
  if (expect(tok::identifier)) {
    return _errorhandler();
  }
   do {
        if (Tok.is(tok::identifier)) {
          Args.push_back(Actions.actOnTypeRefernce(Tok.getLocation(),
                                                   Tok.getIdentifier()));
          advance();
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
      // auto intited_type = Actions.actOnTypeRefernce(Tok.getLocation(),
      // Tok.getIdentifier());
      advance();
      type_D =
          (TypeDeclaration *)Actions.init_genric_class(Decls, type_D, Args);
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
  case tok::identifier:
    if (Lex.peak(0).is(tok::l_paren)) {
      if(parseFunctionCallStatment(Stmts)){
        return _errorhandler();
      };
    } else {
      auto Var =
          Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
      advance(); // eat var
      auto Desig = Actions.actOnDesignator(Var);
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
  default:
    return _errorhandler();
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
                  tok::integer_literal)) {
    if(parseExpression(E)){
      return _errorhandler();
    };
  } else {
    return _errorhandler();
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
                      tok::comma, // add new oprators here
                      tok::minus, tok::semi, tok::less, tok::lessequal,
                      tok::equal, tok::greater, tok::greaterequal, tok::kw_else,
                      tok::r_parth, tok::Or, tok::r_square});
  };
  if(parseFactor(E)){
    return _errorhandler();
  };
  while (Tok.isOneOf(tok::star, tok::slash)) {
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
        {tok::r_paren, tok::star, tok::plus, // new oprators get added here
         tok::comma, tok::minus, tok::slash, tok::semi, tok::less,
         tok::lessequal, tok::equal, tok::greater, tok::greaterequal, tok::And,
         tok::kw_else, tok::r_parth, tok::Or, tok::r_square});
  };
  if (Tok.is(tok::integer_literal)) {
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
      if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                      tok::integer_literal)) {
        if(parseExpList(Exprs)){
          return _errorhandler();
        };
      }
      if(expect(tok::r_paren)){
        return _errorhandler();
      };
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
        auto Method_name = Tok.getIdentifier();
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
          E = new MethodCallExpr((VariableDeclaration *)D, Method_name, Exprs);
        }
      }
    }
  } else if (Tok.is(tok::l_paren)) {
    advance();
    if(parseExpression(E)){
      return _errorhandler();
    };
    if(expect(tok::r_paren)){
      return _errorhandler();
    };
    advance();
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
  } else {
    return _errorhandler();
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

  if(parseStatementSequence(Decls, IfStmts)){
    return _errorhandler();
  };
  if(expect(tok::r_parth)){
    return _errorhandler();
  };
  advance();

  if (Tok.is(tok::kw_else)) {
    if (Tok.isNot(tok::l_parth)) {
      if(parseStatement(Decls, ElseStmts)){
        return _errorhandler();
      };
    } else {
      advance();
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
  };
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

  if(expect(tok::l_paren)){
    return _errorhandler();
  };
  advance();
  
  if(parseVarDecleration(Decls, Start_Val)){
    return _errorhandler();
  };

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
  auto Class_Name = Tok;
  advance();
  Decl *D;
  std::vector<std::tuple<int, StringRef, TypeDeclaration *, SMLoc>> List;
  if (Tok.is(tok::less)) {

    if(ParseTempleteArgs(List)){
      return _errorhandler();
    };

    D = Actions.actOnClassDeclaration(Class_Name.getLocation(),
                                      Class_Name.getIdentifier(), true);
    Genric = true;
  } else {
    D = Actions.actOnClassDeclaration(Class_Name.getLocation(),
                                      Class_Name.getIdentifier(), false);
  };

  EnterDeclScope S(Actions, D);
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
  return false;
};
bool Parser::ParseEnum(DeclList &ParentDecls, StmtList &Stmts) {
  auto _errorhandler = [this] { return SkipUntil({tok::r_parth}); };
  advance(); // eat enum
  TypeDeclaration *Ty = Actions.IntegerType;
  if (Tok.is(tok::colon)) {
    advance();
    if(expect(tok::identifier)){
      return _errorhandler();
    }
    Ty = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
  };
  if(expect(tok::l_parth)){
    return _errorhandler();
  };
  advance();
  std::vector<Token> idents;
  while (Tok.is(tok::identifier)) {
    idents.push_back(Tok);
    advance();
    if(expect(tok::comma)){
      return _errorhandler();
    };
    advance();
  }
  int num = 0;
  for (auto iden : idents) {
    Actions.actOnConstantDeclaration(
        ParentDecls, iden.getLocation(), iden.getIdentifier(),
        Actions.actOnIntegerLiteral(iden.getLocation(), num));
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

  if(expect(tok::identifier)){
    return _errorhandler();
  };
  auto Type = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());

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
    std::vector<std::tuple<int, StringRef, TypeDeclaration *, SMLoc>> &Decls) {
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
      auto Ty =
          Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
      advance();
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
        if(eat){
          advance();
        }
        return true;
      }
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
  }
}