type token=
    Identifier of string
  | Integ of string
  | Bool of string
  | Stri of string
  | Expr of token list
val parseL : string list -> string list * token list
val parseS : string list -> string list * token
val parse_expr : string list -> token
val check_valid : unit -> unit 
