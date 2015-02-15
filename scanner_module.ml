(* Corey Robins. Section 0201. crobins. 111185399 *)
(* I pledge on my honor that I have not given or receieved any unauthorized
assistance on this assignment *)
let parens="^\\((\\|)\\)";;
let identifiers="^\\([a-zA-Z0-9]\\|=\\|*\\|+\\|/\\|<\\|>\\|!\\|?\\|-\\)+";;
let booleans="^\\(#t\\|#f\\)";;
let integers="^\\([0-9]\\)+";;
let strings="^\\(\"\\(\\((\\|)\\|[a-zA-Z0-9 ]\\|=\\|*\\|+\\|/\\|<\\|>\\|!\\|?\\|-\\|#\\)*\\)\"\\)+";;

let rec string_of_string_list' x list = 
  match list with []->"[]"
  | h::[]->if x=1 then "["^(h)^"]"
  else " "^(h)^"]"
  | (h::t)->if x=1 then "["^(h)^";"^(string_of_string_list' (x+1) t)
  else " "^(h)^";"^(string_of_string_list' (x+1) t)
;;

let rec string_of_string_list list =
  string_of_string_list' 1 list
;;

let rec split_line line list=
  match line with 
      ""->list
    | e-> let temp_length= ref 0 in 
	  if Str.string_match (Str.regexp " +") line 0 then
	    split_line (Str.string_after e (!temp_length+1)) (list)
	  else begin if Str.string_match (Str.regexp parens) line 0 then
	      begin if Str.match_end()>(!temp_length) then temp_length := Str.match_end() end
	    else if Str.string_match (Str.regexp identifiers) line 0 then
	      begin if Str.match_end()>(!temp_length) then temp_length := Str.match_end() end
	    else if Str.string_match (Str.regexp booleans) line 0 then
	      begin if Str.match_end()>(!temp_length) then temp_length := Str.match_end() end
	    else if Str.string_match (Str.regexp integers) line 0 then
	      begin if Str.match_end()>(!temp_length) then temp_length := Str.match_end() end
	    else if Str.string_match (Str.regexp strings) line 0 then
	      begin if Str.match_end()>(!temp_length) then temp_length := Str.match_end() end
	    else raise (Failure "Invalid input");
	    split_line (Str.string_after e (!temp_length)) (list@[(Str.string_before e (!temp_length))]) end
;;

let rec parse_line=function()->
  let temp_list=ref [] in
  try
    let line=Get_input.get_input() in
    temp_list := (!temp_list)@(split_line line []);
    print_endline (string_of_string_list (!temp_list)); 
    parse_line()
  with End_of_file->
    print_string ""
;;

let parse_line_np=function()->
  let temp_list=ref [] in
  let line=Get_input.get_input() in
  temp_list := (!temp_list)@(split_line line []);
  !temp_list
;;
