type term = 
  | NumeralConst of int
  | BooleanConst of string
  | StringConst of string
  | VarExp of string
  | Func of string * term list
;;

type term_exp = 
  | AtomicFormula of string * term list
  | BooleanConst of string
;;

type clause = 
  | Fact of term_exp
  | Rule of term_exp * term_exp list
  | Comment of string
;;

type goal = term_exp;;

type program = clause list;;