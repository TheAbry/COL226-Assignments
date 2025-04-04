%{
open Ast
%}

%token<string> Comment IF THEN ELSE ENDIF VarIdentifier TermIdentifier BooleanConst StringConst ArithOp UnaryBoolOp BinaryBoolOp StringOp CompOp
%token<int> NumeralConst 
%token ParenOpen ParenClose BracketOpen BracketClose Comma Dot Colon SemiColon Arrow Pipe EOF

%start program
%type <Ast.program> program

%%

program:
  | clause_list EOF { $1 }

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
  | TermIdentifier ParenOpen term_list ParenClose { TermExp ($1, $3) }
  | IF term_exp THEN term_exp ELSE term_exp ENDIF { IfTE [$2;$4;$6] }
  | BooleanConst { BooleanConst $1 }
  | TermIdentifier { Atom $1 }
  | term_constant bin_op term_constant { TermExp (op_to_string $2, [$1; $3]) }
  | UnaryBoolOp term { TermExp (op_to_string $1, [$2]) }
  | ParenOpen term_exp ParenClose { $2 }

term:
  | TermIdentifier ParenOpen term_list ParenClose { Term ($1, $3) }
  | term_constant bin_op term_constant { Term (op_to_string $2, [$1; $3]) }
  | term_constant { $1 }
  | TermIdentifier { Atom $1 }
  | BracketOpen term_list BracketClose { List $2 }
  | BracketOpen BracketClose { List [] }
  | BracketOpen term Pipe term BracketClose {List [$2;$4] }
  | ParenOpen term ParenClose { $2 }
  | ParenOpen term Comma term_list ParenClose { Tuple ($2 :: $4) }


bin_op:
  | ArithOp { $1 }
  | BinaryBoolOp { $1 }
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

%%
