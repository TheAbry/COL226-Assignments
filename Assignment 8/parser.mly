%{
open Ast
%}

%token<string> Comment VarIdentifier TermIdentifier BooleanConst StringConst ArithOp UnaryBoolOp StringOp CompOp
%token<int> NumeralConst 
%token ParenOpen ParenClose BracketOpen BracketClose Comma Dot SemiColon Arrow Pipe EOF

%start program
%type <Ast.program> program
%type <Ast.term_exp list> query

%%

program:
  | clause_list EOF { $1 }
  | query EOF { List.map (fun te -> Fact te) $1 }

query:
  | term_exp_list Dot { $1 }

clause_list:
  | clause { [$1] }
  | Comment { [Comment $1] }
  | clause clause_list { $1 :: $2 }
  | Comment clause_list { (Comment $1) :: $2 }

clause:
  | fact Dot { $1 }
  | rule Dot { $1 }

fact:
  term_exp { Fact $1 }

rule:
  term_exp Arrow term_exp_list { Rule ($1, $3) }

term_exp:
  | TermIdentifier ParenOpen term_list ParenClose { AtomicFormula ($1, $3) }
  | TermIdentifier { AtomicFormula ($1, []) }
  | ParenOpen term_exp ParenClose { $2 }
  | BooleanConst { BooleanConst $1 }
  | UnaryBoolOp term { AtomicFormula ($1, [$2]) }
  | term CompOp term { AtomicFormula ($2, [$1; $3]) }

term:
  | TermIdentifier ParenOpen term_list ParenClose { Func ($1, $3) }
  | term bin_op term { Func ($2, [$1; $3]) }
  | term_constant { $1 }
  | TermIdentifier { Func ($1, []) }
  | ParenOpen term ParenClose { $2 }
  | ParenOpen term Comma term_list ParenClose { Func ("_tuple", $2 :: $4) }
  | list { $1 }

bin_op:
  | ArithOp { $1 }
  | CompOp { $1 }
  | StringOp { $1 }

term_constant:
  | VarIdentifier { VarExp $1 }
  | NumeralConst { NumeralConst $1 }
  | BooleanConst { BooleanConst $1 }
  | StringConst { StringConst $1 }

term_exp_list:
  | term_exp { [$1] }
  | term_exp Comma term_exp_list { $1 :: $3 }

term_list:
  | term { [$1] }
  | term Comma term_list { $1 :: $3 }

list:
  | BracketOpen BracketClose { Func ("_empty_list", []) }
  | BracketOpen list_body BracketClose { $2 }

list_body:
  | term { Func ("_list", [$1; Func ("_empty_list", [])]) }
  | term Comma list_body { Func ("_list", [$1; $3]) }
  | term Pipe term { Func ("_list", [$1; $3]) }
%%

