(*******************************************************************
   This file ("interpreter.ml") provides stub functions for the extra
   code you need to write for this assignment.
   
   To use this file in the top-level interpreter, type the following
   at the prompt:
   
      #use "interpreter.ml";;

   There are two major stub functions in "interpreter.ml":
   "ast_ize_P" (which transforms a parse tree for a program into
                an abstract syntax tree [AST]); and
   "interpret" (which interprets an AST, using input supplied in
				the form of a parameter which is a string containing
				a list of integers, e.g., "4 6").

   You are also being provided with a file called "parser.ml" which
   contains working code to produce a parse tree for a program.
   Everything in the file "parser.ml" is complete and usable as-is.

   The major entry point for the parser provided in "parser.ml" is
   a function called "parse" invoked with two parameters: A parse table,
   and a string containing a calculator language program.

   The file "parser.ml" constructs two example parse tables:
   "cg_parse_table" (for the original calculator language grammar), and
   "ecg_parse_table" (for the extended calculator language grammar).

   Here are two parser examples which will work as-is:

      parse cg_parse_table sum_ave_prog;;
      parse ecg_parse_table primes_prog;;
   		
   "sum_ave_prog" and "primes_prog" are provided at the end of the parser.ml
   file (as strings). "sum_ave_prog" uses the original calculator
   language grammar, while "primes_prog" uses the extended calculator
   language grammar.

   When complete, your interpreter code should work when invoked as
   follows:

      interpret (ast_ize_P (parse ecg_parse_table primes_prog)) "10";;
   
   which should print "2 3 5 7 11 13 17 19 23 29" (i.e., the first 10 primes).
   (See the end of this file for additional test cases).
 
 *******************************************************************)

#load "str.cma";;

#use "parser.ml";;


type ast_sl = ast_s list
and ast_s =
| AST_error
| AST_assign of (string * ast_e)
| AST_read of string
| AST_write of ast_e
| AST_if of (ast_c * ast_sl)
| AST_while of (ast_c * ast_sl)
and ast_e =
| AST_binop of (string * ast_e * ast_e)
| AST_id of string
| AST_num of string
and ast_c = (string * ast_e * ast_e);;


let rec ast_ize_P (p:parse_tree) : ast_sl =
  (* your code should replace the following line *)
  match p with
  | PT_nt ("P", [stmtlist; PT_term "$$"])    (* P -> SL $$ *)
    -> ast_ize_SL stmtlist
    
  | _ -> raise (Failure "malformed parse tree in ast_ize_P")

and ast_ize_SL (sl:parse_tree) : ast_sl =
  match sl with
  | PT_nt ("SL", []) -> []
  | PT_nt ("SL", [stmt; stmtlist])  (* SL -> S SL *)
  -> (ast_ize_S stmt) :: ast_ize_SL stmtlist
  
  | _ -> raise (Failure "malformed parse tree in ast_ize_SL")

and ast_ize_S (s:parse_tree) : ast_s =
  match s with
  | PT_nt ("S", [PT_id lhs; PT_term ":="; expr])
    -> AST_assign (lhs, (ast_ize_expr expr))
  | PT_nt ("S", [PT_term "read"; PT_id id])
		-> AST_read (id)
  | PT_nt ("S", [PT_term "write"; expr])
		-> AST_write (ast_ize_expr expr)
  | PT_nt ("S", [PT_term "if"; cond; stmtlist; PT_term "end"])      (* S -> if C SL *)
    -> AST_if (ast_ize_C cond, ast_ize_SL stmtlist)
  | PT_nt ("S", [PT_term "while"; cond; stmtlist; PT_term "end"])      (* S -> if C SL *)
    -> AST_while (ast_ize_C cond, ast_ize_SL stmtlist)
  
  | _ -> raise (Failure "malformed parse tree in ast_ize_S")

and ast_ize_expr (e:parse_tree) : ast_e =
  (* e is an E, T, or F parse tree node *)
  match e with
  | PT_nt ("E", [term; termtail])   (* E -> T TT *)
    -> ast_ize_expr_tail (ast_ize_expr term) termtail
  | PT_nt ("T", [fact; facttail])   (* T -> F FT *)
    -> ast_ize_expr_tail (ast_ize_expr fact) facttail
  | PT_nt ("F", p)            (* F -> ( E ) | id | lit *)
    -> ( match p with
         (* code for ( E ) *)
         (* code for id *)
         | [PT_term "("; expr; PT_term ")"]
            -> ast_ize_expr expr
         | [PT_id id]
            -> AST_id id
         | [PT_num (num)] -> AST_num (num) (* code for numeric literal *)
         | _ -> raise (Failure "malformed F parse tree in ast_ize_expr")
       )
       
  | _ -> raise (Failure "malformed parse tree in ast_ize_expr")

and ast_ize_expr_tail (lhs:ast_e) (tail:parse_tree) :ast_e =
  (* lhs is an inherited attribute.
     tail is a TT or FT parse tree node *)
  match tail with
  | PT_nt ("TT", []) -> lhs  (* TT -> epsilon *)
  
  (* your code here *)       (* TT -> ao T TT *)
  | PT_nt ("TT", [PT_nt ("ao", [PT_term op]); term; termtail])
  -> ast_ize_expr_tail (AST_binop (op, lhs, ast_ize_expr term)) termtail
  
  | PT_nt ("FT", []) -> lhs  (* FT -> epsilon *)
  
  (* your code here *)       (* FT -> mo F FT *)
  | PT_nt ("FT", [PT_nt ("mo", [PT_term op]); fact; facttail])
  -> ast_ize_expr_tail (AST_binop (op, lhs, ast_ize_expr fact)) facttail
  
  | _ -> raise (Failure "malformed parse tree in ast_ize_expr_tail")

and ast_ize_C (c:parse_tree) : ast_c =
  match c with

  | PT_nt("C", [lhs; PT_nt ("ro", [PT_term op]); rhs])
      -> (op, ast_ize_expr lhs, ast_ize_expr rhs);
  
  | _ -> raise (Failure "malformed parse tree in ast_ize_C")
;;


(*******************************************************************
    Interpreter
 *******************************************************************)

type memory = (string * int) list;;
(*             name   * val         *)

type value =    (* an integer or an error message *)
| Value of int
| Error of string;;

(* concatenate strings, with a space in between if both were nonempty *)
let str_cat sep a b =
  match (a, b) with
  | (a, "") -> a
  | ("", b) -> b
  | (_, _) -> a ^ sep ^ b;;

(* Input to a calculator program is just a sequence of numbers.  We use
   the standard Str library to split the single input string into
   whitespace-separated words, each of which is subsequently checked
   for valid integer format.
*)
let rec interpret (ast:ast_sl) (full_input:string) : string =
  let inp = split (regexp "[ \t\n\r]+") full_input in
  let (_, _, _, outp) = interpret_sl ast [] inp [] in
    (List.fold_left (str_cat " ") "" outp) ^ "\n"

and interpret_sl (sl:ast_sl) (mem:memory)
                 (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
    (* ok?   new_mem  new_input     new_output *)
  match sl with
  | [] -> (true, mem, inp, outp)
  | head :: rest
  (* your code should replace the following line *)
    -> let (ok, new_mem, new_inp, new_outp) = interpret_s head mem inp outp in
    if ok then interpret_sl rest new_mem new_inp new_outp
    else (false, new_mem, new_inp, new_outp)


(* NB: the following routine is complete.  You can call it on any
   statement node and it figures out what more specific case to invoke.
*)
and interpret_s (s:ast_s) (mem:memory)
                (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  match s with
  | AST_assign(id, expr) -> interpret_assign id expr mem inp outp
  | AST_read(id)         -> interpret_read id mem inp outp
  | AST_write(expr)      -> interpret_write expr mem inp outp
  | AST_if(cond, sl)     -> interpret_if cond sl mem inp outp
  | AST_while(cond, sl)  -> interpret_while cond sl mem inp outp
  | AST_error            -> raise (Failure "cannot interpret erroneous tree")

and interpret_assign (lhs:string) (rhs:ast_e) (mem:memory)
                     (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  let (rhsv, mem2) = interpret_expr rhs mem in
  match rhsv with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])
  | Value (rhs_val)
    -> let mem3 = List.remove_assoc lhs mem2 in
       (true, (lhs, rhs_val) :: mem3, inp, outp)

and interpret_read (id:string) (mem:memory)
                   (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  match inp with
  | [] -> (false, mem, inp, outp @ ["Error: Unexpected end of input"])
  | value :: rest ->
    (try
       let int_value = int_of_string value in
       let mem2 = List.remove_assoc id mem in
       (true, (id, int_value) :: mem2, rest, outp)  (* Store the numeric value in memory *)
     with Failure _ ->
       (false, mem, inp, outp @ ["Error: Non-numeric input for read: " ^ value]))  (* Append error message *)
  (* | h :: t ->
    try
      let v = int_of_string h in
      let mem2 = List.remove_assoc id mem in
      (true, (id, v) :: mem2, t, outp)
    with Failure _ ->
      (false, mem, inp, outp @ ["Invalid input: " ^ h]) *)

and interpret_write (expr:ast_e) (mem:memory)
                    (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  let (value, mem2) = interpret_expr expr mem in
  match value with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])  (* Append error message to output *)
  | Value (v) -> (true, mem2, inp, outp @ [string_of_int v])  (* Append evaluated value to output *)

and interpret_if (cond:ast_c) (sl:ast_sl) (mem:memory)
                 (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  (* your code should replace the following line *)
  let (cond_val, mem2) = interpret_cond cond mem in
  match cond_val with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])  (* Append error message to output *)
  | Value (v) ->
      if v <> 0 then
        interpret_sl sl mem2 inp outp  (* Execute the statement list if condition is true *)
      else
        (true, mem2, inp, outp)  (* Skip the statement list if condition is false *)

and interpret_while (cond:ast_c) (sl:ast_sl) (mem:memory)
                    (inp:string list) (outp:string list)
    : bool * memory * string list * string list =
  let (cond_val, mem2) = interpret_cond cond mem in
  match cond_val with
  | Error (msg) -> (false, mem2, inp, outp @ [msg])
  | Value (continue)
    -> if continue = 0 then (true, mem2, inp, outp)
       else let (ok, new_mem, new_input, new_output) = interpret_sl sl mem2 inp outp in
            interpret_while cond sl new_mem new_input new_output

and interpret_expr (expr:ast_e) (mem:memory) : value * memory =
  match expr with
  | AST_num num
    -> ( try (Value (int_of_string num), mem)
         with Failure _ -> (Error ("Non-numeric literal: " ^ num), mem) )
         
  | AST_id id ->
          (match List.assoc_opt id mem with
           | Some v -> (Value v, mem)
           | None -> (Error ("Error: Symbol not found: " ^ id), mem))
    
  | AST_binop (op, lo, ro)
    (* your code should replace the following line *)
    ->  let (lov, mem1) = interpret_expr lo mem in
        let (rov, mem2) = interpret_expr ro mem1 in
        (match lov with
        | Error msg -> (Error msg, mem)
        | Value lovi ->
            (match rov with
              | Error msg -> (Error msg, mem)
              | Value rovi ->
                  (match op with
                  | "+" -> (Value (lovi + rovi), mem2)
                  | "-" -> (Value (lovi - rovi), mem2)
                  | "*" -> (Value (lovi * rovi), mem2)
                  | "/" ->
                      if rovi = 0 then
                        (Error "Division by zero", mem2)
                      else
                        (Value (lovi / rovi), mem2)
                  | _ -> (Error ("Invalid binary operator: " ^ op), mem2))))
    
  | _ -> (Error ("Invalid AST in interpret_expr"), mem)

and interpret_cond ((op:string), (lo:ast_e), (ro:ast_e)) (mem:memory)
    : value * memory =
  let int_of_bool b = if b then 1 else 0 in
  let (lov, mem1) = interpret_expr lo mem in
  let (rov, mem2) = interpret_expr ro mem1 in
  match lov with
  | Error (msg) -> (Error (msg), mem)
  | Value (lovi)
    -> ( match rov with
         | Error (msg) -> (Error (msg), mem)
         | Value (rovi)
           -> ( match op with
                | "="  -> (Value (int_of_bool (lovi =  rovi)), mem2)
                | "<>" -> (Value (int_of_bool (lovi <> rovi)), mem2)
                | "<"  -> (Value (int_of_bool (lovi <  rovi)), mem2)
                | ">"  -> (Value (int_of_bool (lovi >  rovi)), mem2)
                | "<=" -> (Value (int_of_bool (lovi <= rovi)), mem2)
                | ">=" -> (Value (int_of_bool (lovi >= rovi)), mem2)
                | _ -> (Error ("Invalid relational operator: " ^ op), mem2)
              )
       )
;;

(*******************************************************************
    Testing
 *******************************************************************)

let sum_ave_parse_tree = parse ecg_parse_table sum_ave_prog;;
let sum_ave_syntax_tree = ast_ize_P sum_ave_parse_tree;;

let primes_parse_tree = parse ecg_parse_table primes_prog;;
let primes_syntax_tree = ast_ize_P primes_parse_tree;;

let ecg_run prog inp =
  interpret (ast_ize_P (parse ecg_parse_table prog)) inp;;

(* Sample test cases
  print_string (interpret sum_ave_syntax_tree "4 6");
    (* should print "10 5" *)
  print_newline ();
  print_string (interpret primes_syntax_tree "10");
    (* should print "2 3 5 7 11 13 17 19 23 29" *)
  print_newline ();
  print_string (interpret sum_ave_syntax_tree "4 foo");
    (* should print "non-numeric input" *)
  print_newline ();
  print_string (ecg_run "write 3 write 2 / 0" "");
    (* should print "3 divide by zero" *)
  print_newline ();
  print_string (ecg_run "write foo" "");
    (* should print "foo: symbol not found" *)
  print_newline ();
  print_string (ecg_run "read a read b" "3");
    (* should print "unexpected end of input" *)
  print_newline ();;
*)
