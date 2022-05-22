#pragma once
#include "Diag/Diagnostic.hpp"
#include "Lexer/Lexer.hpp"
#include "Sema/Sema.hpp"
#include "AST/AST.hpp"

class Parser {
  Lexer &Lex;

  Sema &Actions;

  Token Tok;
  DiagnosticsEngine &getDiagnostics() const { return Lex.getDiagnostics(); }

  void advance() { Lex.next(Tok); }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return false;
    }
    // There must be a better way!
    const char *Expected = tok::getPunctuatorSpelling(ExpectedTok);
    if (!Expected)
      Expected = tok::getKeywordSpelling(ExpectedTok);
    llvm::StringRef Actual(Tok.getLocation().getPointer(), Tok.getLength());
    getDiagnostics().report(Tok.getLocation(), diag::err_expected, Expected,
                            Actual);
    return true;
  }

  bool consume(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      advance();
      return false;
    }
    return true;
  }

public:
  Parser(Lexer &Lex, Sema &Actions);

  CompileUnitDeclaration *parse();
  bool ParseFuction(DeclList &ParentDecls);
  bool parseParameters(ParamList &Params);
  bool parseParameter(ParamList& Params);
  bool parseBlock(DeclList& Decls,
      StmtList& Stmts);
  bool parseVarDecleration(DeclList& Decls,
      StmtList& Stmts);
  bool parseStatement(DeclList& Decls,
      StmtList& Stmts);
  bool parseStatementSequence(DeclList& Decls,
      StmtList& Stmts);
  bool parseReturnStatement(DeclList& Decls,
      StmtList& Stmts);
  bool parseFunctionCallStatment(StmtList& Stmts);

  bool parseExpList(ExprList &Exprs);
  bool parseExpression(Expr* &E);
  bool parseSimpleExpression(Expr*& E);
  bool parseRelation(OperatorInfo &Op);
  bool parseAddOperator(OperatorInfo &Op);
  bool parseTerm(Expr *&E);
  bool parseMulOperator(OperatorInfo &Op);
  bool parseFactor(Expr *&E);
  bool parseIfStatement(DeclList& Decls,
      StmtList& Stmts);
  bool parseWhileStatement(DeclList& Decls,
      StmtList& Stmts);
  bool parseForStatement(DeclList& Decls,
      StmtList& Stmts);    
};