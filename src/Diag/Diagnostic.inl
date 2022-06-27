#ifndef DIAG
#define DIAG(ID, Level, Msg)
#endif

DIAG(err_expected, Error, "expected {0} but found {1}")
DIAG(err_symbold_declared, Error, "symbol {0} already declared")
DIAG(err_hex_digit_in_decimal, Error, "decimal number contains hex digit")
DIAG(err_unterminated_char_or_string, Error, "missing terminating character")
DIAG(err_returntype_must_be_type, Error, "return type of function must be declared type")
DIAG(err_function_and_return_type, Error, "Type of return value is not compatible with function defintion")
DIAG(err_type_isnt_found, Error, "The Type {0} don't exist")
DIAG(err_var_isnt_found, Error, "The Variable {0} don't exist")
DIAG(war_var_shadowing, Warning, "The Variable {0} is shodowing Variable outside this scope")
DIAG(err_cant_type, Error, "cant assgin variable type {0} with a expr type {1}")
DIAG(err_cant_assgin_to_empty_expr, Error, "cant assgin to empty expr")
DIAG(err_types_for_operator_not_compatible, Error, "types not compatible for operator {0}")
DIAG(warn_ambigous_negation, Warning, "Negation is ambigous. Please consider using parenthesis.")
DIAG(err_function_call_on_nonfunction, Error, "function call requires a function")
DIAG(err_vardecl_requires_type, Error, "variable declaration requires type")
DIAG(err_if_expr_must_be_bool, Error, "expression of if statement must have type bool")
DIAG(err_wrong_number_of_parameters, Error, "wrong number of parameters")
DIAG(err_type_of_formal_and_actual_parameter_not_compatible, Error, "type of defintion and actual parameter are not compatible")
DIAG(err_var_parameter_requires_var, Error, "parameter refernce requires variable as argument")
DIAG(err_indexing_non_array,Error, "Inedxing a non array variable")
DIAG(err_accessing_member_non_class,Error, "Accessing a Member Varible on non Class variable")
DIAG(err_member_not_found,Error, "Member Variable {0} is nonexisting inside the Class {1}")
DIAG(err_type_not_defined,Error, "Type is not defined for Variable {0}")

#undef DIAG