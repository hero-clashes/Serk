#include "Parser.hpp"

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  Lex.next(Tok);
};


CompileUnitDeclaration *Parser::parse() {
  CompileUnitDeclaration *module = nullptr;
  module = Actions.actOnCompileUnitDeclaration(
        SMLoc(), "Main");

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
        if(ParseFuction(Decls)){

        };
      } else if(Lex.peak(0).is(tok::identifier)){
        // handle var decleration
      }
    }
  }

    Actions.actOnCompileUnitDeclaration(module, SMLoc(),
                                   "Main",
                                   Decls, Stmts);

  return module;
};

bool Parser::ParseFuction(DeclList &ParentDecls){
  auto type = Tok.getIdentifier();
  auto RetType = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
  advance(); //eat type identifer
  auto function_name = Tok.getIdentifier();
  auto D = Actions.actOnFunctionDeclaration(
            Tok.getLocation(), Tok.getIdentifier());

  advance(); //eat function_name identifer
  EnterDeclScope S(Actions, D);// added befor the parmeters so the parmeters get added to the function scope
  expect(tok::l_paren);
  ParamList Params;
  parseParameters(Params);
  
  expect(tok::l_parth);
  advance();
  Actions.actOnProcedureHeading(D, Params, RetType);


  DeclList Decls;
  StmtList Stmts;
  
  parseBlock(Decls, Stmts);

    ParentDecls.push_back(D);
  Actions.actOnProcedureDeclaration(
        D, SMLoc(), "Main",
        Decls, Stmts);
  expect(tok::r_parth);
  advance();
  return false;
}

bool Parser::parseParameters(ParamList &Params){
  consume(tok::l_paren);
  while (Tok.is(tok::identifier)) {
      parseParameter(Params);
      if (!Tok.isOneOf(tok::comma, tok::r_paren)) {
          //TODO error out
      }
      if (Tok.is(tok::r_paren)) {
          break;
      }
  };
  consume(tok::r_paren);
  return false;
};
bool Parser::parseParameter(ParamList& Params) {
    auto type_D = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
    auto Parem = Actions.actOnParmaDecl(Tok.getLocation(), Tok.getIdentifier(),type_D);
    Params.push_back(Parem);
    return false;
}
bool Parser::parseBlock(DeclList& Decls, StmtList& Stmts)
{

    while (Tok.isNot(tok::r_parth)) {
        if (Tok.is(tok::identifier) && Lex.peak(0).is(tok::identifier)) {
            //parse var defintion
            parseVarDecleration(Decls, Stmts);
        }
        else {
            //parse statements
            parseStatementSequence(Decls, Stmts);
        }
    };
    return false;
}
bool Parser::parseVarDecleration(DeclList& Decls, StmtList& Stmts)
{
    auto type_D = Actions.actOnTypeRefernce(Tok.getLocation(), Tok.getIdentifier());
    consume(tok::identifier);
    auto Var = Actions.actOnVarDeceleration(Tok.getLocation(), Tok.getIdentifier(), type_D);
    Decls.push_back(Var);
    consume(tok::identifier);
    // TODO add check for assiginment
    if (Tok.is(tok::equal)) {
        advance();
        auto Desig = Actions.actOnDesignator(Var);
        auto E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
        Actions.actOnAssignment(Stmts, Tok.getLocation(), Desig, E);
        advance();
    }
    expect(tok::semi);
    
    advance();

    return false;
}
bool Parser::parseStatement(DeclList& Decls, StmtList& Stmts)
{
    switch (Tok.getKind()) {
    case tok::kw_return:
        parseReturnStatement(Decls,Stmts);
        break;
    default:
        break;
    }
    return false;
}


bool Parser::parseStatementSequence(DeclList& Decls, StmtList& Stmts)
{
    parseStatement(Decls,Stmts);

    while (Tok.is(tok::semi)) {
        advance();
        parseStatement(Decls, Stmts);
    }
    return false;
}


bool Parser::parseExpression(Expr* &E)
{
    parseSimpleExpression(E);
    if (Tok.isOneOf( tok::less,
        tok::lessequal, tok::equal,
        tok::greater, tok::greaterequal)) {
        OperatorInfo Op;
        Expr* Right = nullptr;
        //if (parseRelation(Op))
        //    goto _error;
        //if (parseSimpleExpression(Right))
        //    goto _error;
        //E = Actions.actOnExpression(E, Right, Op);
    }
    return false;
}
bool Parser::parseSimpleExpression(Expr*& E)
{
    return false;
}
bool Parser::parseReturnStatement(DeclList& Decls,
    StmtList& Stmts)
{
    Expr* E = nullptr;
    SMLoc Loc = Tok.getLocation();
    consume(tok::kw_return);
    if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus,
        tok::identifier,
        tok::integer_literal)) {
        //parseExpression(E);// this should be added //TODO
        auto D = Actions.actOnVarRefernce(Tok.getLocation(), Tok.getIdentifier());
        E = Actions.actOnDesignator(D);
        advance();
    }
    Actions.actOnReturnStatement(Stmts, Loc, E);
    expect(tok::semi);

    advance();

    return false;
}
;