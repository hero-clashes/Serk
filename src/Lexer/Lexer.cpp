#include "Lexer.hpp"

void KeywordFilter::addKeyword(StringRef Keyword, tok::TokenKind TokenCode) {
  HashTable.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(StringRef(#NAME), tok::kw_##NAME);
#include "TokenKinds.inl"
}

namespace charinfo {
LLVM_READNONE inline bool isASCII(char Ch) {
  return static_cast<unsigned char>(Ch) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == '\r' || Ch == '\n');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == ' ' || Ch == '\t' || Ch == '\f' || Ch == '\v');
}

LLVM_READNONE inline bool isWhitespace(char Ch) {
  return isHorizontalWhitespace(Ch) || isVerticalWhitespace(Ch);
}

LLVM_READNONE inline bool isDigit(char Ch) {
  return isASCII(Ch) && Ch >= '0' && Ch <= '9';
}

LLVM_READNONE inline bool isHexDigit(char Ch) {
  return isASCII(Ch) && (isDigit(Ch) || (Ch >= 'A' && Ch <= 'F'));
}

LLVM_READNONE inline bool isIdentifierHead(char Ch) {
  return isASCII(Ch) &&
         (Ch == '_' || (Ch >= 'A' && Ch <= 'Z') || (Ch >= 'a' && Ch <= 'z'));
}

LLVM_READNONE inline bool isIdentifierBody(char Ch) {
  return isIdentifierHead(Ch) || isDigit(Ch);
}
} // namespace charinfo

void Lexer::next(Token &Result) {
  while (*CurPtr && charinfo::isWhitespace(*CurPtr)) {
    ++CurPtr;
  }
  if (!*CurPtr) {
    Result.setKind(tok::eof);
    return;
  }
  if (charinfo::isIdentifierHead(*CurPtr)) {
    identifier(Result);
    return;
  } else if (charinfo::isDigit(*CurPtr)) {
    number(Result);
    return;
  } else if (*CurPtr == '"') {
    string(Result);
    return;
  } else {
    switch (*CurPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(Result, CurPtr + 1, tok);                                        \
    break
      case '=':
      if (*(CurPtr + 1) == '=')
        formToken(Result, CurPtr + 2, tok::equal_equal);
      else
        formToken(Result, CurPtr + 1, tok::equal);
      break;
      case '!':
      if (*(CurPtr + 1) == '=')
        formToken(Result, CurPtr + 2, tok::not_equal);
      else
        formToken(Result, CurPtr + 1, tok::Not);
      break;
      case '|':
      if (*(CurPtr + 1) == '|')
        formToken(Result, CurPtr + 2, tok::Or);
      break;
      case '&':
      if (*(CurPtr + 1) == '&')
        formToken(Result, CurPtr + 2, tok::And);
      else
        formToken(Result, CurPtr + 1, tok::Amper);
      break;
      // CASE('=', tok::equal);
      CASE('+', tok::plus);
      CASE('-', tok::minus);
      CASE('*', tok::star);
    // CASE('/', tok::slash);
    case '/':
      if (*(CurPtr + 1) == '/') {
        comment();
        next(Result);
      } else
        formToken(Result, CurPtr + 1, tok::slash);
      break;
      CASE(',', tok::comma);
      CASE('.', tok::period);
      CASE(';', tok::semi);
      CASE('(', tok::l_paren);
      CASE(')', tok::r_paren);
      CASE('{', tok::l_parth);
      CASE('}', tok::r_parth);
      CASE(':', tok::colon);
      CASE('[', tok::l_square);
      CASE(']', tok::r_square);
#undef CASE
    case '<':
      if (*(CurPtr + 1) == '=')
        formToken(Result, CurPtr + 2, tok::lessequal);
      else
        formToken(Result, CurPtr + 1, tok::less);
      break;
    case '>':
      if (*(CurPtr + 1) == '=')
        formToken(Result, CurPtr + 2, tok::greaterequal);
      else
        formToken(Result, CurPtr + 1, tok::greater);
      break;
    default:
      Result.setKind(tok::unknown);
    }
    return;
  }
}

void Lexer::identifier(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isIdentifierBody(*End))
    ++End;
  StringRef Name(Start, End - Start);
  formToken(Result, End, Keywords.getKeyword(Name, tok::identifier));
}

void Lexer::number(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  tok::TokenKind Kind = tok::unknown;
  bool IsHex = false;
  while (*End) {
    if (!charinfo::isHexDigit(*End))
      break;
    if (!charinfo::isDigit(*End))
      IsHex = true;
    ++End;
  }
  switch (*End) {
  case 'H': /* hex number */
    Kind = tok::integer_literal;
    ++End;
    break;
  default: /* decimal number */
    if (IsHex)
      Diags.report(getLoc(), diag::err_hex_digit_in_decimal);
    Kind = tok::integer_literal;
    break;
  }
  formToken(Result, End, Kind);
}

void Lexer::string(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (*End && *End != *Start && !charinfo::isVerticalWhitespace(*End))
    ++End;
  if (charinfo::isVerticalWhitespace(*End)) {
    Diags.report(getLoc(), diag::err_unterminated_char_or_string);
  }
  formToken(Result, End + 1, tok::string_literal);
}

void Lexer::comment() {
  const char *End = CurPtr + 2;
  while (*End) {
    // Check for end of comment
    if (*End == '\n') {
      End++;
      break;
    }
    ++End;
  }
  CurPtr = End;
}

void Lexer::formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind) {
  size_t TokLen = TokEnd - CurPtr;
  Result.Ptr = CurPtr;
  ;
  Result.Length = TokLen;
  Result.Kind = Kind;
  CurPtr = TokEnd;
}

Token Lexer::peak(int n) {
  auto Lexer_Copy = *this;
  Token Tok;
  Lexer_Copy.next(Tok);
  for (int i = 0; i < n; i++) {
    Lexer_Copy.next(Tok);
    if (Tok.is(tok::eof)) {
      return Tok;
    }
  }
  return Tok;
};