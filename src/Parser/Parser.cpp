#include "Parser.hpp"
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
      // if (Lex.peak(1).is(tok::l_paren)) {
        // handle function decleration
        //
        if (ParseFuction(Decls)) {
        };
      // } else if (Lex.peak(0).is(tok::identifier)) {
        // handle var decleration
      // }
    }
    if(Tok.is(tok::kw_var)){
      parseVarDecleration(Decls, Stmts);
    }
    if(Tok.is(tok::kw_class)){
      ParseClass(Decls);
    };
    if (Tok.is(tok::kw_enum)) {
      ParseEnum(Decls,  Stmts);
    }
    if(Tok.is(tok::kw_using)){
      ParseUsing(Decls);
      expect(tok::semi);
      advance();
    }
  }

  Actions.actOnCompileUnitDeclaration(module, SMLoc(), "Main", Decls, Stmts);

  return module;
};

bool Parser::ParseFuction(DeclList &ParentDecls) {
  advance(); // eat fn
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
  while (Tok.isOneOf(tok::identifier,tok::kw_ref)) {
    parseParameter(Params);
    if (!Tok.isOneOf(tok::comma, tok::r_paren)) {
      // TODO error out
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
  bool by_refernce = false;
  if(Tok.is(tok::kw_ref)){
    by_refernce = true;
    advance();
  }
  auto type_D =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  consume(tok::identifier);
  auto Parem =
      Actions.actOnParmaDecl(Tok.getLocation(), Tok.getIdentifier(), type_D,by_refernce);
  consume(tok::identifier);
  Params.push_back(Parem);
  return false;
}
bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {

  while (Tok.isNot(tok::r_parth)) {
    if (Tok.is(tok::kw_var)) {
      // parse var defintion
      parseVarDecleration(Decls, Stmts);
    } else if (Tok.is(tok::kw_enum)) {
      ParseEnum(Decls,  Stmts);
    } else {
      // parse statements
      parseStatementSequence(Decls, Stmts);
    }
  };
  return false;
}
bool Parser::parseVarDecleration(DeclList &Decls, StmtList &Stmts) {
  advance(); // eat var
  auto var = Tok;
  // auto type_D =
  //     Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  consume(tok::identifier);
  TypeDeclaration *type_D = nullptr;
  if(Tok.is(tok::colon)){
    advance();
    type_D =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
    if(Tok.is(tok::less)){
      advance();
      auto intited_type = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
      advance();
      type_D = (TypeDeclaration *)Actions.init_genric_class(Decls,type_D, intited_type);
      expect(tok::greater);
      advance();
    } else if (Tok.is(tok::l_square)) {
      advance();
      Expr *E = nullptr;
      parseExpression(E);
      type_D = Actions.actOnArrayTypeDeclaration(Decls,Tok.getLocation(),E,type_D);
      expect(tok::r_square);
      advance();
    }
  };
  
  Expr *Desig = nullptr;
  Expr *E = nullptr;
  if (Tok.is(tok::equal)) {
    advance();
    parseExpression(E);
  }
  expect(tok::semi);

  advance();
  if(!type_D){
    type_D = E->getType();
  }
  auto Var = Actions.actOnVarDeceleration(var.getLocation(),
                                          var.getIdentifier(), type_D);
  if (E) {
    Desig = Actions.actOnDesignator(Var);
    Actions.actOnAssignment(Stmts, Tok.getLocation(), Desig, E);
  }
  Decls.push_back(Var);
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
    } else {
      auto Var = Actions.actOnVarRefernce(Tok.getLocation(),
                                          Tok.getIdentifier());
      advance();// eat var
      auto Desig = Actions.actOnDesignator(Var);
      parseSelectors(Desig);
      Expr *E;
      if(Tok.is(tok::period)){
        ParseMethodCallStatment(Stmts,E);
      } else{
      advance();// eat =
      parseExpression(E);
      Actions.actOnAssignment(Stmts, Tok.getLocation(), Desig, E);}
    }
  break;
  case tok::kw_if:
    //parse if 
    parseIfStatement(Decls, Stmts);
  break;
  case tok::kw_while:
  //parse while
    parseWhileStatement(Decls, Stmts);
  case tok::kw_for:
    parseForStatement(Decls, Stmts);
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
  advance();
  if (Tok.is(tok::l_paren)) {
    advance();
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                    tok::integer_literal,tok::string_literal)) {
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
  if (Tok.isOneOf(tok::less, tok::lessequal, tok::equal_equal, tok::greater, tok::not_equal,tok::Not,tok::And,tok::Or,
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
      // here function calls handling
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
      E = Actions.actOnDesignator(D);
      parseSelectors(E);
      //Expr *E;
      if(Tok.is(tok::period)){
        //ParseMethodCallStatment(Stmts,E);
      }
      //advance();
    //TODO fix method calls handling
      // auto Method_name = Tok.getIdentifier();
      // ExprList Exprs;
      // advance();
      // //TODO add members access
      // if (Tok.is(tok::l_paren)) {
      //   advance();
      //   if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
      //                   tok::integer_literal)) {
      //     parseExpList(Exprs);
      //     // goto _error;
      //   }
      //   expect(tok::r_paren);
      //   // goto _error;
      //   advance();
      // //   E = new MethodCallExpr(dyn_cast<VariableDeclaration>(D),Method_name,Exprs);
      // } else {

      // }
      
      } //else {
      // D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
      // advance();
      //here simple variable referencing
    //   E = Actions.actOnDesignator(D);
    //   parseSelectors(E);
    //   //       // goto _error;
    // }
  } else if (Tok.is(tok::l_paren)) {
    advance();
    parseExpression(E);
    //     // goto _error;
    consume(tok::r_paren);
    // goto _error;
    } else if (Tok.is(tok::Not)) {
      OperatorInfo Op = fromTok(Tok);
      advance();
      parseFactor(E);
      E = Actions.actOnPrefixExpression(E, Op);
  }else if (Tok.is(tok::string_literal)) {
    //todo move stuff to the sema
    E = Actions.actOnStringLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else {
    /*ERROR*/
  }
  return false;
}

bool Parser::parseRelation(OperatorInfo &Op) {
   if (Tok.is(tok::Not)) {
    Op = fromTok(Tok);
    advance();
  } else
  if (Tok.is(tok::not_equal)) {
    Op = fromTok(Tok);
    advance();
  } else   
  if (Tok.is(tok::equal_equal)) {
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
    else if (Tok.is(tok::And)) {
     Op = fromTok(Tok);
    advance();
    }
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
  else if (Tok.is(tok::Or)) {
      Op = fromTok(Tok);
      advance();
  }
  // else {
  //     ERROR
  //     goto _error;
  // }
  return false;
}

bool Parser::parseIfStatement(DeclList &Decls, StmtList &Stmts) {
  Expr *E = nullptr;
  StmtList IfStmts, ElseStmts;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_if);

  expect(tok::l_paren);
  advance();

  parseExpression(E);

  expect(tok::r_paren);
  advance();

  expect(tok::l_parth);
  advance();
  parseStatementSequence(Decls, IfStmts);
  expect(tok::r_parth);
  advance();

  if (Tok.is(tok::kw_else)) {
    advance();
     expect(tok::l_parth);
    advance();
    parseBlock(Decls,ElseStmts);
    expect(tok::r_parth);
    advance();
  }
  Actions.actOnIfStatement(Stmts, Loc, E, IfStmts,
                             ElseStmts);
  return false;
};
bool Parser::parseWhileStatement(DeclList &Decls, StmtList &Stmts) {
  Expr *E = nullptr;
  StmtList WhileStmts;
  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_while);

  expect(tok::l_paren);
  advance();

  parseExpression(E);

  expect(tok::r_paren);
  advance();

  expect(tok::l_parth);
  advance();
  parseBlock(Decls, WhileStmts);
  expect(tok::r_parth);
  advance();
  Actions.actOnWhileStatement(Stmts, Loc, E, WhileStmts);
  return false;
};
bool Parser::parseForStatement(DeclList &Decls, StmtList &Stmts) {
  Expr *E = nullptr;
  StmtList Start_Val;
  StmtList ForStepStmts;
  StmtList ForBodyStmts;


  SMLoc Loc = Tok.getLocation();
  consume(tok::kw_for);

  expect(tok::l_paren);
  advance();
  parseVarDecleration(Decls,Start_Val);
  
  // expect(tok::semi);
  // advance();

  parseExpression(E);

  expect(tok::semi);
  advance();
  parseStatementSequence(Decls, ForStepStmts);
  
  expect(tok::r_paren);
  advance();

  expect(tok::l_parth);
  advance();
  parseBlock(Decls, ForBodyStmts);
  expect(tok::r_parth);
  advance();
  Actions.actOnForStatement(Stmts, Loc, E, Start_Val, ForStepStmts, ForBodyStmts);
  return false;
}
bool Parser::ParseClass(DeclList &ParentDecls){
  bool Genric = false;
  advance(); // eat class
  auto Class_Name = Tok;
  advance();
  Decl *D;
  if (Tok.is(tok::less)) {
    advance();
    expect(tok::identifier);
    advance();
    expect(tok::greater);
    advance();
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
    Actions.Create_Genric_type();
  }
  
  expect(tok::l_parth);
  advance();
  while (Tok.isNot(tok::r_parth)) {
    if (Lex.peak(1).is(tok::l_paren)) {
        // handle function decleration
        //
        if (ParseFuction(Decls)) {
        };
    } else if (Lex.peak(0).is(tok::identifier)) {
      // handle var decleration
      parseVarDecleration(Decls, StartStmt);
    }
  }
  expect(tok::r_parth);
  advance();

  Actions.actOnClassBody(D, Decls, StartStmt);
  // D->Decls =Decls;
  // D->Stmts = StartStmt;

  ParentDecls.push_back(D);
  return false;
};
bool Parser::ParseMethodCallStatment(StmtList& Stmts,Expr *E){
  auto Method_name = Tok.getIdentifier();
  auto loc = Tok.getLocation();
  ExprList Exprs;
  advance();

  if (Tok.is(tok::l_paren)) {
    advance();
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::identifier,
                    tok::integer_literal)) {
      parseExpList(Exprs);
      // goto _error;
    }
    expect(tok::r_paren);
    // goto _error;
    advance();
  }
  Stmts.push_back(new MethodCallStatement(E,Method_name,Exprs));
  return false;
};
bool Parser::ParseEnum(DeclList &ParentDecls,StmtList& Stmts){
  advance(); //eat enum
  TypeDeclaration* Ty = Actions.IntegerType;
  if(Tok.is(tok::colon)){
    advance();
    Ty =
      Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);

  };
  expect(tok::l_parth);
  advance();
  std::vector<Token> idents;
  while (Tok.is(tok::identifier)) {
    idents.push_back(Tok);
    advance();
    expect(tok::comma);
    advance();
  }
  int num = 0;
  for (auto iden : idents) {
    Actions.actOnConstantDeclaration(ParentDecls,iden.getLocation(),
                                          iden.getIdentifier(), Actions.actOnIntegerLiteral(iden.getLocation(), num));
    num++;
  }
  expect(tok::r_parth);
  advance();
  return false;
}

bool Parser::ParseUsing(DeclList &ParentDecls){
  advance(); // eat using

  expect(tok::identifier);
  auto Aliased_name = Tok; 
  advance();

  expect(tok::equal);
  advance();

  auto Type = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());

  expect(tok::identifier);
  Actions.actOnAliasTypeDeclaration(ParentDecls, Aliased_name.getLocation(), Aliased_name.getIdentifier(), Type);
  advance();
  return false;
};
bool Parser::parseSelectors(Expr *&E) 
  {
    while (Tok.isOneOf(tok::period, tok::l_square)) {
      if (Tok.is(tok::l_square)) {
        SMLoc Loc = Tok.getLocation();
        Expr *IndexE = nullptr;
        advance();
        parseExpression(IndexE);
        expect(tok::r_square);
        Actions.actOnIndexSelector(E, Loc, IndexE);
        advance();
      } else if (Tok.is(tok::period)) {
        advance();
        expect(tok::identifier);
        if(Lex.peak(0).is(tok::l_paren)) return false;
        Actions.actOnFieldSelector(E, Tok.getLocation(),
                                   Tok.getIdentifier());
        advance();
      }
    }
    return false;
  }