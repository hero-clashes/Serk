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
TOK(float_literal)     // 12.3
TOK(string_literal)      // "foo", 'foo'

PUNCTUATOR(plus,                "+")
PUNCTUATOR(minus,               "-")
PUNCTUATOR(star,                "*")
PUNCTUATOR(slash,               "/")
PUNCTUATOR(period,              ".")
PUNCTUATOR(comma,               ",")
PUNCTUATOR(semi,                ";")
PUNCTUATOR(colon,               ":")
PUNCTUATOR(equal,               "=")
PUNCTUATOR(equal_equal,         "==")
PUNCTUATOR(not_equal,           "!=")
PUNCTUATOR(Not,                 "!")
PUNCTUATOR(Or,                  "||")
PUNCTUATOR(Amper,               "&")
PUNCTUATOR(And,                 "&&")
PUNCTUATOR(Reminder,            "%")
PUNCTUATOR(less,                "<")
PUNCTUATOR(greater,             ">")
PUNCTUATOR(lessequal,           "<=")
PUNCTUATOR(greaterequal,        ">=")
PUNCTUATOR(l_paren,             "(")
PUNCTUATOR(r_paren,             ")")
PUNCTUATOR(l_parth,             "{")
PUNCTUATOR(r_parth,             "}")
PUNCTUATOR(l_square,            "[")
PUNCTUATOR(r_square,            "]")
PUNCTUATOR(tild,                "~")
KEYWORD(using                          , KEYALL)
KEYWORD(enum                           , KEYALL)
KEYWORD(ref                            , KEYALL)
KEYWORD(fn                             , KEYALL)
KEYWORD(var                            , KEYALL)
KEYWORD(import                         , KEYALL)
KEYWORD(return                         , KEYALL)
KEYWORD(if                             , KEYALL)
KEYWORD(else                           , KEYALL)
KEYWORD(while                          , KEYALL)
KEYWORD(for                            , KEYALL)
KEYWORD(class                          , KEYALL)
KEYWORD(type                           , KEYALL)
KEYWORD(extern                         , KEYALL)
KEYWORD(sizeof                         , KEYALL)
KEYWORD(new                            , KEYALL)
#undef KEYWORD
#undef PUNCTUATOR
#undef TOK