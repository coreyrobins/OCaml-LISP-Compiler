(* Corey Robins. Section 0201. crobins. 111185399 *)
(* I pledge on my honor that I have not given or receieved any unauthorized
assistance on this assignment *)
type return_vals =
  Intege of int
  | Stringe of string
  | Boole of string
  | Nil of string
;;

let defines=ref [];;

let rec evaluate s=
  match s with
      Parser_module.Integ s -> Intege (int_of_string s)
    | Parser_module.Bool s -> Boole s
    | Parser_module.Stri s -> if s="nil" then Nil s 
      else if (check_if_defined s) then get_val s
      else Stringe s
    | Parser_module.Identifier s->if s="nil" then Nil s 
      else if (check_if_defined s) then get_val s
      else Stringe s
    | Parser_module.Expr s -> begin match s with (h::t)-> if h=(Parser_module.Identifier "+") then (Intege (add t))
      else if h=(Parser_module.Identifier "-") then (Intege (subtract t))
      else if h=(Parser_module.Identifier "*") then (Intege (multiply t))
      else if h=(Parser_module.Identifier "/") then (Intege (divide t))
      else if h=(Parser_module.Identifier "=") then (equals t)
      else if h=(Parser_module.Identifier "<>") then (not_equals t)
      else if h=(Parser_module.Identifier "<") then (less_than t)
      else if h=(Parser_module.Identifier ">") then (greater_than t)
      else if h=(Parser_module.Identifier "<=") then (less_than_equal t)
      else if h=(Parser_module.Identifier ">=") then (greater_than_equal t)
      else if h=(Parser_module.Identifier "boolean?") then (check_bool t)
      else if h=(Parser_module.Identifier "number?") then (check_num t)
      else if h=(Parser_module.Identifier "string?") then (check_string t)
      else if h=(Parser_module.Identifier "list?") then (check_list t)
      else if h=(Parser_module.Identifier "if") then (if_check t)
      else if h=(Parser_module.Identifier "define") then (define_check t)
      else raise (Failure "invalid identifier")
	| _-> raise (Failure "error") end

and

int_of_token x=
  match x with Parser_module.Integ x->int_of_string x
    | _->raise (Failure "Expected Integ")

and

int_of_return x=
  match x with Intege x->x
    | _->raise (Failure "Expected Intege")

and

add=function
[]->0
  | (h::t)->(int_of_return (evaluate h))+(add t)

and

subtract x=
match x with []->0
  | h::[]->(int_of_return (evaluate h))*(-1)
  | h::t->(int_of_return (evaluate h))-(add t)

and

multiply=function
[]->1
  | h::t->(int_of_return (evaluate h))*(multiply t)

and

divide=function
[]->1
  | h::t->(int_of_return (evaluate h))/(multiply t)

and

equals=function
  (h:Parser_module.token)::(t:Parser_module.token)::[]->if (evaluate h)=(evaluate t) then Boole "#t" else Boole "#f"
  | _->raise (Failure "Expecting two booleans")

and

not_equals=function
(h:Parser_module.token)::(t:Parser_module.token)::[]->if ((evaluate h)=(evaluate t)) then Boole "#f" else Boole "#t"
  | _->raise (Failure "Expecting two booleans")

and

less_than=function
(h:Parser_module.token)::(t:Parser_module.token)::[]->begin let first=(evaluate h) in let second=(evaluate t) in
							  if second=(Boole "#t") then begin if
							      first=(Boole "#f") then (Boole "#t")
							    else (Boole "#f") end
							  else if second=Boole "#f" then Boole"#f"
							  else if first<second then Boole "#t"
							  else Boole "#f" end
  | _->raise (Failure "Expecting two booleans")

and

greater_than=function
(h:Parser_module.token)::(t:Parser_module.token)::[]->begin let first=(evaluate h) in let second=(evaluate t) in
							  if second=(Boole "#f") then begin if
							      first=(Boole "#t") then (Boole "#t")
							    else (Boole "#f") end
							  else if second=Boole "#t" then Boole"#f"
							  else if first>second then Boole "#t"
							  else Boole "#f" end
  | _->raise (Failure "Expecting two booleans")

and

less_than_equal=function
(h:Parser_module.token)::(t:Parser_module.token)::[]->begin let first=(evaluate h) in let second=(evaluate t) in
							  if second=Boole "#t" then Boole "#t"
							  else if second=Boole "#f" then begin
							    if first=Boole "#f" then Boole "#t" 
							    else Boole"#f" end
							  else if first<=second then Boole "#t"
							  else Boole "#f" end
  | _->raise (Failure "Expecting two booleans")

and

greater_than_equal=function
(h:Parser_module.token)::(t:Parser_module.token)::[]->begin let first=(evaluate h) in let second=(evaluate t) in
							  if second=Boole "#f" then Boole "#t"
							  else if second=Boole "#t" then begin
							    if first=Boole "#t" then Boole "#t" 
							    else Boole"#f" end
							  else if first>=second then Boole "#t"
							  else Boole "#f" end
  | _->raise (Failure "Expecting two booleans")

and

check_bool=function
(h:Parser_module.token)::[]->begin let x=(evaluate h) in match x with Boole x->Boole "#t"
  | _-> Boole "#f"  end
  | _->raise (Failure "expecting one boolean argument")
    
and

check_num=function
(h:Parser_module.token)::[]->begin let x=(evaluate h) in match x with Intege s->Boole "#t"
  | _-> Boole "#f"  end
  | _->raise (Failure "expecting one number argument")

and

check_string=function
(h:Parser_module.token)::[]->begin let x=(evaluate h) in match x with Stringe x->Boole "#t"
  | _-> Boole "#f"  end
  | _->raise (Failure "expecting one string argument")

and

check_list=function
(h:Parser_module.token)::[]->begin let x=(evaluate h) in match x with Nil "nil"->Boole "#t"
  | _-> Boole "#f"  end
  | _->raise (Failure "expecting one list argument")

and

if_check=function
(h:Parser_module.token)::(x:Parser_module.token)::(y:Parser_module.token)::[]->if (evaluate h)=(Boole "#t") then (evaluate x) else (evaluate y)
  | (h:Parser_module.token)::(x:Parser_module.token)::[]->if (evaluate h)=(Boole "#t") then (evaluate x) else (Nil "nil")
  | _->raise (Failure "wrong type of list")

and

define_check=function
(name:Parser_module.token)::(value:Parser_module.token)::[]->begin let x=name in match x with Parser_module.Identifier x ->(add_to_list (Stringe x) (evaluate value)) 
  | _->raise (Failure "wrong inputs") end
  | _-> raise (Failure "wrong inputs")

and

add_to_list name (value:return_vals)=
  (defines:=List.append [(name,value)] !defines); value
    
and

check_if_defined s=(List.mem_assoc (Stringe s) !defines)

and

get_val s=
  if (List.mem_assoc (Stringe s) !defines) then (List.assoc (Stringe s) !defines)
  else (Stringe s)

and

print_val()=
  try
    let value=evaluate (Parser_module.parse_expr (Scanner_module.parse_line_np()))
    in begin match value with Intege value->print_endline (string_of_int value)
      | Stringe value->print_endline value
      | Boole value->print_endline value
      | Nil value->print_endline value end;
    print_val()
  with End_of_file->
    print_string ""					   
;;
