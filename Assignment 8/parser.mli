type token =
  | Comment of (
# 5 "parser.mly"
       string
# 6 "parser.mli"
)
  | VarIdentifier of (
# 5 "parser.mly"
       string
# 11 "parser.mli"
)
  | TermIdentifier of (
# 5 "parser.mly"
       string
# 16 "parser.mli"
)
  | BooleanConst of (
# 5 "parser.mly"
       string
# 21 "parser.mli"
)
  | StringConst of (
# 5 "parser.mly"
       string
# 26 "parser.mli"
)
  | ArithOp of (
# 5 "parser.mly"
       string
# 31 "parser.mli"
)
  | UnaryBoolOp of (
# 5 "parser.mly"
       string
# 36 "parser.mli"
)
  | StringOp of (
# 5 "parser.mly"
       string
# 41 "parser.mli"
)
  | CompOp of (
# 5 "parser.mly"
       string
# 46 "parser.mli"
)
  | NumeralConst of (
# 6 "parser.mly"
       int
# 51 "parser.mli"
)
  | ParenOpen
  | ParenClose
  | BracketOpen
  | BracketClose
  | Comma
  | Dot
  | SemiColon
  | Arrow
  | Pipe
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
