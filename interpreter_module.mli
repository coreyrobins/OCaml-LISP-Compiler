type return_vals
val evaluate: Parser_module. token->return_vals
val int_of_token: Parser_module.token->int
val int_of_return: return_vals->int
val add: Parser_module.token list->int
val subtract: Parser_module.token list->int
val multiply: Parser_module.token list->int
val divide: Parser_module.token list->int
val equals: Parser_module.token list->return_vals
val not_equals: Parser_module.token list->return_vals
val less_than: Parser_module.token list->return_vals
val greater_than: Parser_module.token list->return_vals
val less_than_equal: Parser_module.token list->return_vals
val greater_than_equal: Parser_module.token list->return_vals
val check_bool: Parser_module.token list->return_vals
val check_num: Parser_module.token list->return_vals
val check_string: Parser_module.token list->return_vals
val check_list: Parser_module.token list->return_vals
val if_check: Parser_module.token list->return_vals
val define_check: Parser_module.token list -> return_vals
val add_to_list: return_vals -> return_vals -> return_vals
val check_if_defined: string->bool
val get_val: string->return_vals
val print_val: unit->unit
