let op_to_string op = match op with
  | "&" -> "and"
  | "|" -> "or"
  | "!" -> "not"
  | "+" -> "plus"
  | "-" -> "minus"
  | "*" -> "times"
  | "/" -> "divide"
  | "%" -> "modulo"
  | "^" -> "concat"
  | "<=" -> "leq"
  | ">=" -> "geq"
  | "<" -> "lt"
  | ">" -> "gt"
  | "==" -> "eq"
  | "!=" -> "neq"
;;

type term = 
  | NumeralConst of int
  | BooleanConst of string
  | StringConst of string
  | VarExp of string
  | Term of string * term list
  | Atom of string
  | Tuple of term list
  | List of term list
;;

type term_exp = 
  | TermExp of string * term list
  | BooleanConst of string
  | IfTE of term_exp list
  | Atom of string
;;

type clause = 
  | Fact of term_exp
  | Rule of term_exp * term_exp list
  | Comment of string
;;

type program = clause list;;


