{
open Lexing
open Parser

exception EoF
exception LexicalError of string

let rec is_valid_alnum_underscore s =
  match s with
  | [] -> true
  | c :: rest ->
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> is_valid_alnum_underscore rest
    | _ -> false

let is_valid_var_string s =
  match s with
  | [] -> false
  | c :: rest ->
    match c with
    | 'A' .. 'Z' | '_' -> is_valid_alnum_underscore rest
    | _ -> false

let is_valid_term_string s =
  match s with
  | [] -> false
  | c :: rest ->
    match c with
    | 'a' .. 'z' -> is_valid_alnum_underscore rest
    | _ -> false

let to_token s = match s with
  | "fail" -> BooleanConst "fail"
  | "false" -> BooleanConst "false"
  | "true" -> BooleanConst "true"
  | s when is_valid_var_string (s |> String.to_seq |> List.of_seq) -> VarIdentifier s
  | s when is_valid_term_string (s |> String.to_seq |> List.of_seq) -> TermIdentifier s
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let alphanumeric = letter | digit

let ident = ['a'-'z' 'A'-'Z' '_']
let numeral = ['0'-'9']


rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | '(' { ParenOpen }
  | ')' { ParenClose }
  | '[' { BracketOpen }
  | ']' { BracketClose }
  | ',' { Comma }
  | '.' { Dot }
  | ';' { SemiColon }
  | '|' { Pipe }
  | ":-" { Arrow }
  | '+' { ArithOp "+" }
  | '-' { ArithOp "-" }
  | '*' { ArithOp "*" }
  | "/*" { Comment (comment lexbuf) }
  | '/' { ArithOp "/" }
  | '%' { ArithOp "%" }
  | "\\+" { UnaryBoolOp "!" }
  | '!' { BooleanConst "cut" }
  | '^' { StringOp "^" }
  | "<=" { CompOp "<=" }
  | ">=" { CompOp ">=" }
  | "<" { CompOp "<" }
  | ">" { CompOp ">" }
  | "==" { CompOp "==" }
  | "!=" { CompOp "!=" }
  | "=" { CompOp "=" }
  | '"' { StringConst (string_literal lexbuf) }
  | (letter | '_') (letter | digit | '_')* as s { to_token s }
  | digit+ as s { NumeralConst (int_of_string s) }
  | eof { EOF }
  | _ { raise (LexicalError (lexeme lexbuf)) }

and string_literal = parse
  | '"' { "" }
  | '\\' '"' { "\"" }
  | '\\' '\\' { "\\" }
  | '\\' 'n' { "\n" }
  | '\\' 't' { "\t" }
  | '\\' '\\' { "\\" }
  | _ as c { String.make 1 c ^ string_literal lexbuf }

and comment = parse
  | '*' '/' { "" }
  | _ as c { String.make 1 c ^ comment lexbuf }