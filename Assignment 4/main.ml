open Ast

let test_cases = [
  "sibling(X, Y).";
  "parent_child(Z, X):- parent_child(Z, Y).";
  "if fail then fail else false endif.";
]

let prog = [
  Fact (TermExp ("sibling", [VarExp "X"; VarExp "Y"]));
  Rule (TermExp ("parent_child", [VarExp "Z"; VarExp "X"]),
        [TermExp ("parent_child", [VarExp "Z"; VarExp "Y"])]);
]

let rec string_of_term term =
  match term with
  | NumeralConst n -> "NumeralConst " ^ string_of_int n
  | BooleanConst b -> "BooleanConst " ^ b
  | StringConst s -> "StringConst " ^ "\"" ^ s ^ "\""
  | VarExp v -> "VarExp " ^ v
  | Atom s -> "Atom " ^ s
  | Tuple args -> "Tuple (" ^ String.concat ", " (List.map string_of_term args) ^ ")"
  | List args -> "List [" ^ String.concat ", " (List.map string_of_term args) ^ "]"
  | Term (op, args) -> "\nTermExp (" ^ op ^ ", [" ^ String.concat ", " (List.map string_of_term args) ^ "])"

let rec string_of_term_exp term_exp =
  match term_exp with
  | BooleanConst b -> "BooleanConst " ^ b
  | TermExp (op, args) -> "\nTermExp (" ^ op ^ ", [" ^ String.concat ", " (List.map string_of_term args) ^ "])"
  | Atom s -> "Atom " ^ s
  | IfTE args ->
    "IfThenElse [" ^ String.concat "; " (List.map string_of_term_exp args) ^ "]"

let string_of_program program =
  let clause_to_string clause =
    match clause with
    | Comment comment -> ""
    | Fact term -> "Fact (" ^ string_of_term_exp term ^ "\n)"
    | Rule (head, body) ->
        "Rule (" ^ string_of_term_exp head ^ ",\n[" ^ String.concat "; " (List.map string_of_term_exp body) ^ "\n])"
  in
  "Program (\n" ^ String.concat ",\n" (List.map (fun clause -> "  " ^ clause_to_string clause) program) ^ "\n)"
;;

(* print_endline (string_of_program prog);; *)

let print_tokens lexbuf =
  try
    while true do
      let token = Lexer.token lexbuf in
      match token with
      | EOF -> raise Exit
      | _ ->
        print_endline (match token with
        | IF s -> "IF"
        | THEN s -> "THEN"
        | ELSE s -> "ELSE"
        | ENDIF s -> "ENDIF"
        | Comment s -> "Comment " ^ s
        | VarIdentifier s -> "VarIdentifier " ^ s
        | TermIdentifier s -> "TermIdentifier " ^ s
        | BooleanConst s -> "BooleanConst " ^ s
        | StringConst s -> "StringConst " ^ s
        | ArithOp s -> "ArithOp " ^ s
        | UnaryBoolOp s -> "UnaryBoolOp " ^ s
        | BinaryBoolOp s -> "BinaryBoolOp " ^ s
        | StringOp s -> "StringOp " ^ s
        | CompOp s -> "CompOp " ^ s
        | NumeralConst n -> "NumeralConst " ^ string_of_int n
        | ParenOpen -> "ParenOpen"
        | ParenClose -> "ParenClose"
        | BracketOpen -> "BracketOpen"
        | BracketClose -> "BracketClose"
        | Comma -> "Comma"
        | Dot -> "Dot"
        | Colon -> "Colon"
        | SemiColon -> "SemiColon"
        | Arrow -> "Arrow"
        | Pipe -> "Pipe"
        | EOF -> "End of File"
        )
    done
  with
  | Exit -> ()
  | Lexer.LexicalError msg -> Printf.printf "Lexical error: %s\n" msg

let print_ast lexbuf =
let finished = ref false in
while not !finished do
  try
    let result = Parser.program Lexer.token lexbuf in
    print_endline (string_of_program result);
    finished := true 
  with
  | Lexer.LexicalError msg -> Printf.printf "Lexical error: %s\n" msg
  | Stdlib.Parsing.Parse_error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    let pos_str = Printf.sprintf "line %d, character %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
    Printf.printf "Parse error at %s\n" pos_str;
    exit 0
done

let _ =
  let test_cases = [
    "sibling(X, Y).";
    "sibling(X, Y).\nparent_child(Z, X):- parent_child(Z, Y).";
    "if fail then fail else false endif."
  ] in
  List.iter (fun input ->
    let lexbuf = Lexing.from_string input in
    print_endline input;
    print_ast lexbuf
  ) test_cases
  
let _ =
  let lexbuf = Lexing.from_channel stdin in
    print_ast lexbuf
