#include "Parser.hpp"

namespace {
OperatorInfo fromTok(Token Tok) {
  return OperatorInfo(Tok.getLocation(), Tok.getKind());
}
} // namespace

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  Lex.next(Tok);
};

CompileUnitDeclaration *Parser::parse() {
  CompileUnitDeclaration *module = nullptr;
  module = Actions.actOnCompileUnitDeclaration(SMLoc(), "Main");

  DeclList Decls;
  StmtList Stmts;
  while (Tok.isNot(tok::eof)) {
    while (Tok.is(tok::kw_import)) {
      // handle_import()
    };
    if (Tok.is(tok::identifier)) {
      if (Lex.peak(1).is(tok::l_paren)) {
        // handle function decleration
        //
        if (ParseFuction(Decls)) {
        };
      } else if (Lex.peak(0).is(tok::identifier)) {
        // handle var decleration
        parseVarDecleration(Decls, Stmts);
      }
    }
  }

  Actions.actOnCompileUnitDeclaration(module, SMLoc(), "Main", Decls, Stmts);

  return module;
};

bool Parser::ParseFuction(DeclList &ParentDecls) {
  auto type = Tok.getIdentifier();
  auto RetType =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  advance(); // eat type identifer
  auto function_name = Tok.getIdentifier();
  auto D =
      Actions.actOnFunctionDeclaration(Tok.getLocation(), Tok.getIdentifier());

  advance();                    // eat function_name identifer
  EnterDeclScope S(Actions, D); // added befor the parmeters so the parmeters
                                // get added to the function scope
  expect(tok::l_paren);
  ParamList Params;
  parseParameters(Params);

  expect(tok::l_parth);
  advance();
  Actions.actOnFunctionHeading(D, Params, RetType);

  DeclList Decls;
  StmtList Stmts;

  parseBlock(Decls, Stmts);

  ParentDecls.push_back(D);
  Actions.actOnFunctionDeclaration(D, SMLoc(), function_name, Decls, Stmts);
  expect(tok::r_parth);
  advance();
  return false;
}

bool Parser::parseParameters(ParamList &Params) {
  consume(tok::l_paren);
  while (Tok.is(tok::identifier)) {
    parseParameter(Params);
    if (!Tok.isOneOf(tok::comma, tok::r_paren)) {
      // TODO error out
    }
    if (Tok.is(tok::r_paren)) {
      break;
    }
  };
  consume(tok::r_paren);
  return false;
};
bool Parser::parseParameter(ParamList &Params) {
  auto type_D =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  consume(tok::identifier);
  auto Parem =
      Actions.actOnParmaDecl(Tok.getLocation(), Tok.getIdentifier(), type_D);
  consume(tok::identifier);
  Params.push_back(Parem);
  return false;
}
bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {

  while (Tok.isNot(tok::r_parth)) {
    if (Tok.is(tok::identifier) && Lex.peak(0).is(tok::identifier)) {
      // parse var defintion
      parseVarDecleration(Decls, Stmts);

    } else {
      // parse statements
      parseStatementSequence(Decls, Stmts);
    }
  };
  return false;
}
bool Parser::parseVarDecleration(DeclList &Decls, StmtList &Stmts) {
  auto type_D =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  consume(tok::identifier);
  auto Var = Actions.actOnVarDeceleration(Tok.getLocation(),
                                          Tok.getIdentifier(), type_D);
  Decls.push_back(Var);
  consume(tok::identifier);
  // TODO add check for assiginment
  if (Tok.is(tok::equal)) {
    advance();
    auto Desig = Actions.actOnDesignator(Var);
    Expr *E;
    parseExpression(E);
    Actions.actOnAssignment(Stmts, Tok.getLocation(), Desig, E);
  }
  expect(tok::semi);

  advance();

  return false;
}
bool Parser::parseStatement(DeclList &Decls, StmtList &Stmts) {
  switch (Tok.getKind()) {
  case tok::kw_return:
    parseReturnStatement(Decls, Stmts);
    break;
  case tok::identifier:
    if (Lex.peak(0).is(tok::l_paren)) {
      parseFunctionCallStatment(Stmts);
    }
  default:
    break;
  }
  return false;
}

bool Parser::parseStatementSequence(DeclList &Decls, StmtList &Stmts) {
  parseStatement(Decls, Stmts);

  while (Tok.is(tok::semi)) {
    advance();
    parseStatement(Decls, Stmts);
  }
  return false;
}

bool Parser::parseReturnStatement(DeclList &Decls, StmtList &Stmts) {
  Expr *E = nullptr;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_return);
  if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                  tok::integer_literal)) {
    parseExpression(E);// this should be added
    // auto D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
    // advance();
  }
  Actions.actOnReturnStatement(Stmts, Loc, E);
  expect(tok::semi);

  advance();

  return false;
};

bool Parser::parseFunctionCallStatment(StmtList &Stmts) {
  ExprList Exprs;
  Decl *D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
  auto loc = Tok.getLocation();
  if (Tok.is(tok::l_paren)) {
    advance();
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                    tok::integer_literal)) {
      parseExpList(Exprs);
      // goto _error;
    }
    expect(tok::r_paren);
    // goto _error;
    auto Statment = Actions.actOnFunctionCallStatemnt(loc, D, Exprs);
    Stmts.push_back(Statment);
    advance();
  }
  return false;
};
bool Parser::parseExpList(ExprList &Exprs) {
  Expr *E = nullptr;
  parseExpression(E);
  // goto _error;
  if (E)
    Exprs.push_back(E);
  while (Tok.is(tok::comma)) {
    E = nullptr;
    advance();
    parseExpression(E);
    // goto _error;
    if (E)
      Exprs.push_back(E);
  }
  return false;
}

bool Parser::parseExpression(Expr *&E) {
  parseSimpleExpression(E);
  if (Tok.isOneOf(tok::less, tok::lessequal, tok::equal_equal, tok::greater,
                  tok::greaterequal)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    parseRelation(Op);
    //  goto _error;
    parseSimpleExpression(Right);
    //  goto _error;
    E = Actions.actOnExpression(E, Right, Op);
  }
  return false;
}
bool Parser::parseSimpleExpression(Expr *&E) {
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
  parseTerm(E);
  while (Tok.isOneOf(tok::plus, tok::minus)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    parseAddOperator(Op);

    parseTerm(Right);
    E = Actions.actOnSimpleExpression(E, Right, Op);
  }
  if (!PrefixOp.isUnspecified())

    E = Actions.actOnPrefixExpression(E, PrefixOp);
  return false;
}
bool Parser::parseTerm(Expr *&E) {
  parseFactor(E);
  while (Tok.isOneOf(tok::star, tok::slash)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    parseMulOperator(Op);
    parseFactor(Right);
    E = Actions.actOnTerm(E, Right, Op);
  }
  return false;
}
bool Parser::parseFactor(Expr *&E) {
  if (Tok.is(tok::integer_literal)) {
    E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else if (Tok.is(tok::identifier)) {
    Decl *D;
    ExprList Exprs;
    //   if (parseQualident(D))
    //     goto _error;
    auto call = Tok;
    D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
    advance();
    if (Tok.is(tok::l_paren)) {
      //TODO add here function calls handling
          advance();
          if (Tok.isOneOf(tok::l_paren, tok::plus,
                          tok::minus,
                          tok::identifier,
                          tok::integer_literal)) {
            parseExpList(Exprs);
              // goto _error;
          }
          expect(tok::r_paren);
            // goto _error;
          E = Actions.actOnFunctionCallExpr(call.getLocation(),D,Exprs);
          advance();
    } else {
      // D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
      // advance();
      E = Actions.actOnDesignator(D);
      // if (parseSelectors(E))
      //       // goto _error;
    }
  } else if (Tok.is(tok::l_paren)) {
    advance();
    parseExpression(E);
    //     // goto _error;
    consume(tok::r_paren);
    // goto _error;
    // } else if (Tok.is(tok::kw_NOT)) {
    //   OperatorInfo Op = fromTok(Tok);
    //   advance();
    //   if (parseFactor(E))
    //     goto _error;
    //   E = Actions.actOnPrefixExpression(E, Op);
  } else {
    /*ERROR*/
  }
  return false;
}

bool Parser::parseRelation(OperatorInfo &Op) {
  if (Tok.is(tok::equal)) {
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
  } else {
    /*ERROR*/
  }
  return false;
}
bool Parser::parseMulOperator(OperatorInfo &Op) {
  {
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
    // else if (Tok.is(tok::kw_AND)) {
    //  Op = fromTok(Tok);
    // advance();
    //}
    // else {
    /*ERROR*/
    //  goto _error;
  }
  return false;
}

bool Parser::parseAddOperator(OperatorInfo &Op) {

  if (Tok.is(tok::plus)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::minus)) {
    Op = fromTok(Tok);
    advance();
  }
  /*else if (Tok.is(tok::kw_OR)) {
      Op = fromTok(Tok);
      advance();
  }
  else {
      /*ERROR
      goto _error;
  }*/
  return false;
}