#ifndef DIAG
#define DIAG(ID, Level, Msg)
#endif


DIAG(err_unterminated_block_comment, Error, "unterminated (* comment")
DIAG(err_unterminated_char_or_string, Error, "missing terminating character")
DIAG(err_hex_digit_in_decimal, Error, "decimal number contains hex digit")

#undef DIAG