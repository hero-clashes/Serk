#ifndef DIAG
#define DIAG(ID, Level, Msg)
#endif

DIAG(err_expected, Error, "expected {0} but found {1}")
DIAG(err_unterminated_block_comment, Error, "unterminated (* comment")
DIAG(err_unterminated_char_or_string, Error, "missing terminating character")
DIAG(err_hex_digit_in_decimal, Error, "decimal number contains hex digit")
DIAG(err_symbold_declared, Error, "symbol {0} already declared")
DIAG(err_returntype_must_be_type, Error, "return type of function must be declared type")
DIAG(err_function_and_return_type, Error, "Type of RETURN value is not compatible with function type")

#undef DIAG