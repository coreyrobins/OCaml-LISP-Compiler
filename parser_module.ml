(* Corey Robins. Section 0201. crobins. 111185399 *)
(* I pledge on my honor that I have not given or receieved any unauthorized
assistance on this assignment *)
let parens="^\\((\\|)\\)+";;
let ids="^\\([a-zA-Z0-9]\\|=\\|*\\|+\\|/\\|<\\|>\\|!\\|?\\|-\\)+";;
let booleans="^\\(#t\\|#f\\)";;
let integers="^\\([0-9]\\)+";;
let strings="^\\(\"\\(\\((\\|)\\|[a-zA-Z0-9 ]\\|=\\|*\\|+\\|/\\|<\\|>\\|!\\|?\\|-\\|#\\)*\\)\"\\)+";;

type token=
    Identifier of string
  | Integ of string
  | Bool of string
  | Stri of string
  | Expr of token list
;;

let rec parseL lr = match lr with
    (h::t)->if h=")" then (t, [])
      else let (rem_lr, x)=parseS lr in let (rem_lr1, y)=parseL rem_lr in
					(rem_lr1, x::y)
  | _ -> raise (Failure " ")

and

parseS lr = 
  match lr with
      []->raise (Failure "parseS() was called with an empty list") 
    | (h::t)->if h="(" then let (x,y)=(parseL t) in (x, Expr y)
      else if Str.string_match (Str.regexp integers) h 0 then (t,Integ h)
      else if Str.string_match (Str.regexp booleans) h 0 then (t,Bool h)
      else if Str.string_match (Str.regexp ids) h 0 then (t,Identifier h)
      else if Str.string_match (Str.regexp strings) h 0 then (t,Stri h)
      else raise (Failure "Error")
;;

let parse_expr s =
  let (token_list, ast)=parseS s in
  if token_list = [] then ast
  else raise (Failure "Invalid Expression")
;;

let rec check_valid() =
  try
    let tokens = Scanner_module.parse_line_np() in
    parse_expr (tokens);
    print_endline "Valid.";
    check_valid()
  with End_of_file ->
    print_string ""
    | Failure n->
      print_endline "Invalid.";
      check_valid()
;;
