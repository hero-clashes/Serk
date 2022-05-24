#ifndef TOK
#define TOK(ID)
#endif
#ifndef PUNCTUATOR
#define PUNCTUATOR(ID, SP) TOK(ID)
#endif
#ifndef KEYWORD
#define KEYWORD(ID, FLAG) TOK(kw_ ## ID)
#endif



// These define members of the tok::* namespace.

TOK(unknown)             // Not a token.
TOK(eof)                 // End of file.

TOK(identifier)          // abcde123

TOK(integer_literal)     // 123, 123B, 123H
TOK(string_literal)      // "foo", 'foo'

PUNCTUATOR(plus,                "+")
PUNCTUATOR(minus,               "-")
PUNCTUATOR(star,                "*")
PUNCTUATOR(slash,               "/")
PUNCTUATOR(period,              ".")
PUNCTUATOR(comma,               ",")
PUNCTUATOR(semi,                ";")
PUNCTUATOR(equal,               "=")
PUNCTUATOR(equal_equal,         "==")
PUNCTUATOR(not_equal,           "!=")
PUNCTUATOR(Not,                 "!")
PUNCTUATOR(Or,                  "||")
PUNCTUATOR(And,                 "&&")
PUNCTUATOR(less,                "<")
PUNCTUATOR(greater,             ">")
PUNCTUATOR(lessequal,           "<=")
PUNCTUATOR(greaterequal,        ">=")
PUNCTUATOR(l_paren,             "(")
PUNCTUATOR(r_paren,             ")")
PUNCTUATOR(l_parth,             "{")
PUNCTUATOR(r_parth,             "}")


KEYWORD(import                         , KEYALL)
KEYWORD(return                         , KEYALL)
KEYWORD(if                             , KEYALL)
KEYWORD(else                           , KEYALL)
KEYWORD(while                          , KEYALL)
KEYWORD(for                            , KEYALL)
KEYWORD(class                          , KEYALL)
#undef KEYWORD
#undef PUNCTUATOR
#undef TOK