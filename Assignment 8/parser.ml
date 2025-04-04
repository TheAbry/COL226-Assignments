type token =
  | Comment of (
# 5 "parser.mly"
       string
# 6 "parser.ml"
)
  | VarIdentifier of (
# 5 "parser.mly"
       string
# 11 "parser.ml"
)
  | TermIdentifier of (
# 5 "parser.mly"
       string
# 16 "parser.ml"
)
  | BooleanConst of (
# 5 "parser.mly"
       string
# 21 "parser.ml"
)
  | StringConst of (
# 5 "parser.mly"
       string
# 26 "parser.ml"
)
  | ArithOp of (
# 5 "parser.mly"
       string
# 31 "parser.ml"
)
  | UnaryBoolOp of (
# 5 "parser.mly"
       string
# 36 "parser.ml"
)
  | StringOp of (
# 5 "parser.mly"
       string
# 41 "parser.ml"
)
  | CompOp of (
# 5 "parser.mly"
       string
# 46 "parser.ml"
)
  | NumeralConst of (
# 6 "parser.mly"
       int
# 51 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 68 "parser.ml"
let yytransl_const = [|
  267 (* ParenOpen *);
  268 (* ParenClose *);
  269 (* BracketOpen *);
  270 (* BracketClose *);
  271 (* Comma *);
  272 (* Dot *);
  273 (* SemiColon *);
  274 (* Arrow *);
  275 (* Pipe *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* Comment *);
  258 (* VarIdentifier *);
  259 (* TermIdentifier *);
  260 (* BooleanConst *);
  261 (* StringConst *);
  262 (* ArithOp *);
  263 (* UnaryBoolOp *);
  264 (* StringOp *);
  265 (* CompOp *);
  266 (* NumeralConst *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\005\000\
\005\000\006\000\007\000\008\000\008\000\008\000\008\000\008\000\
\008\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\004\000\
\004\000\009\000\009\000\013\000\013\000\014\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\001\000\002\000\002\000\002\000\
\002\000\001\000\003\000\004\000\001\000\003\000\001\000\002\000\
\003\000\004\000\003\000\001\000\001\000\003\000\005\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\003\000\002\000\003\000\001\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\031\000\000\000\
\029\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\024\000\007\000\000\000\
\000\000\000\000\030\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\002\000\001\000\003\000\006\000\008\000\009\000\
\000\000\000\000\025\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\014\000\022\000\000\000\000\000\000\000\
\037\000\033\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\000\000\035\000\018\000\023\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\024\000\
\047\000\020\000\046\000\021\000\022\000\034\000"

let yysindex = "\014\000\
\048\255\000\000\048\255\000\000\015\255\000\000\000\000\114\255\
\000\000\102\255\060\255\000\000\024\000\030\000\018\255\048\255\
\038\255\041\255\025\255\003\255\000\000\000\000\000\000\042\255\
\114\255\057\255\000\000\114\255\090\255\064\255\027\255\000\000\
\255\254\055\255\000\000\000\000\000\000\000\000\000\000\000\000\
\102\255\102\255\000\000\000\000\114\255\114\255\067\255\125\255\
\114\255\120\255\000\000\000\000\000\000\114\255\114\255\114\255\
\000\000\000\000\073\255\000\000\090\255\090\255\000\000\114\255\
\083\255\096\255\000\000\090\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\072\000\000\000\069\255\074\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\102\000\
\000\000\000\000\094\255\000\000\000\000\000\000\000\000\094\255\
\000\000\013\255\000\000\000\000\132\255\000\000\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\108\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\098\255\000\000\137\255\127\255\085\255\000\000\
\000\000\000\000\000\000\107\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\007\000\025\000\000\000\000\000\000\000\003\000\
\208\255\248\255\000\000\000\000\000\000\067\000"

let yytablesize = 155
let yytable = "\029\000\
\065\000\031\000\033\000\019\000\043\000\066\000\044\000\051\000\
\043\000\023\000\044\000\045\000\030\000\055\000\001\000\069\000\
\048\000\056\000\021\000\050\000\021\000\021\000\038\000\035\000\
\021\000\025\000\021\000\021\000\021\000\036\000\021\000\021\000\
\043\000\037\000\044\000\045\000\061\000\062\000\053\000\041\000\
\048\000\054\000\042\000\059\000\059\000\048\000\033\000\068\000\
\003\000\004\000\005\000\006\000\007\000\039\000\008\000\048\000\
\040\000\009\000\010\000\042\000\011\000\004\000\026\000\027\000\
\007\000\058\000\060\000\049\000\057\000\009\000\028\000\005\000\
\011\000\032\000\021\000\052\000\021\000\021\000\063\000\030\000\
\013\000\030\000\030\000\013\000\013\000\015\000\013\000\041\000\
\015\000\015\000\018\000\015\000\018\000\018\000\070\000\043\000\
\012\000\044\000\051\000\012\000\012\000\004\000\012\000\004\000\
\005\000\006\000\007\000\071\000\008\000\010\000\038\000\009\000\
\010\000\032\000\011\000\004\000\026\000\027\000\007\000\034\000\
\040\000\067\000\000\000\009\000\028\000\043\000\011\000\044\000\
\051\000\000\000\043\000\053\000\044\000\051\000\054\000\000\000\
\000\000\000\000\019\000\064\000\019\000\019\000\019\000\016\000\
\019\000\019\000\016\000\016\000\017\000\016\000\000\000\017\000\
\017\000\000\000\017\000"

let yycheck = "\008\000\
\049\000\010\000\011\000\001\000\006\001\054\000\008\001\009\001\
\006\001\003\000\008\001\009\001\010\000\015\001\001\000\064\000\
\025\000\019\001\006\001\028\000\008\001\009\001\016\000\000\000\
\012\001\011\001\014\001\015\001\016\001\000\000\018\001\019\001\
\006\001\016\001\008\001\009\001\045\000\046\000\012\001\015\001\
\049\000\015\001\018\001\041\000\042\000\054\000\055\000\056\000\
\001\001\002\001\003\001\004\001\005\001\016\001\007\001\064\000\
\016\001\010\001\011\001\018\001\013\001\002\001\003\001\004\001\
\005\001\041\000\042\000\011\001\014\001\010\001\011\001\000\000\
\013\001\014\001\006\001\012\001\008\001\009\001\012\001\006\001\
\012\001\008\001\009\001\015\001\016\001\012\001\018\001\015\001\
\015\001\016\001\006\001\018\001\008\001\009\001\012\001\006\001\
\012\001\008\001\009\001\015\001\016\001\000\000\018\001\002\001\
\003\001\004\001\005\001\012\001\007\001\016\001\014\001\010\001\
\011\001\016\001\013\001\002\001\003\001\004\001\005\001\012\001\
\014\001\055\000\255\255\010\001\011\001\006\001\013\001\008\001\
\009\001\255\255\006\001\012\001\008\001\009\001\015\001\255\255\
\255\255\255\255\012\001\015\001\014\001\015\001\016\001\012\001\
\018\001\019\001\015\001\016\001\012\001\018\001\255\255\015\001\
\016\001\255\255\018\001"

let yynames_const = "\
  ParenOpen\000\
  ParenClose\000\
  BracketOpen\000\
  BracketClose\000\
  Comma\000\
  Dot\000\
  SemiColon\000\
  Arrow\000\
  Pipe\000\
  EOF\000\
  "

let yynames_block = "\
  Comment\000\
  VarIdentifier\000\
  TermIdentifier\000\
  BooleanConst\000\
  StringConst\000\
  ArithOp\000\
  UnaryBoolOp\000\
  StringOp\000\
  CompOp\000\
  NumeralConst\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause_list) in
    Obj.repr(
# 16 "parser.mly"
                    ( _1 )
# 230 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.term_exp list) in
    Obj.repr(
# 17 "parser.mly"
              ( List.map (fun te -> Fact te) _1 )
# 237 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term_exp_list) in
    Obj.repr(
# 20 "parser.mly"
                      ( _1 )
# 244 "parser.ml"
               : Ast.term_exp list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause) in
    Obj.repr(
# 23 "parser.mly"
           ( [_1] )
# 251 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
            ( [Comment _1] )
# 258 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 25 "parser.mly"
                       ( _1 :: _2 )
# 266 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 26 "parser.mly"
                        ( (Comment _1) :: _2 )
# 274 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fact) in
    Obj.repr(
# 29 "parser.mly"
             ( _1 )
# 281 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule) in
    Obj.repr(
# 30 "parser.mly"
             ( _1 )
# 288 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_exp) in
    Obj.repr(
# 33 "parser.mly"
           ( Fact _1 )
# 295 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_exp_list) in
    Obj.repr(
# 36 "parser.mly"
                               ( Rule (_1, _3) )
# 303 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 39 "parser.mly"
                                                  ( AtomicFormula (_1, _3) )
# 311 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "parser.mly"
                   ( AtomicFormula (_1, []) )
# 318 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_exp) in
    Obj.repr(
# 41 "parser.mly"
                                  ( _2 )
# 325 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
                 ( BooleanConst _1 )
# 332 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "parser.mly"
                     ( AtomicFormula (_1, [_2]) )
# 340 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "parser.mly"
                     ( AtomicFormula (_2, [_1; _3]) )
# 349 "parser.ml"
               : 'term_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 47 "parser.mly"
                                                  ( Func (_1, _3) )
# 357 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bin_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
                     ( Func (_2, [_1; _3]) )
# 366 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_constant) in
    Obj.repr(
# 49 "parser.mly"
                  ( _1 )
# 373 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                   ( Func (_1, []) )
# 380 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 51 "parser.mly"
                              ( _2 )
# 387 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 52 "parser.mly"
                                              ( Func ("_tuple", _2 :: _4) )
# 395 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 53 "parser.mly"
         ( _1 )
# 402 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
            ( _1 )
# 409 "parser.ml"
               : 'bin_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
           ( _1 )
# 416 "parser.ml"
               : 'bin_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
             ( _1 )
# 423 "parser.ml"
               : 'bin_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                  ( VarExp _1 )
# 430 "parser.ml"
               : 'term_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
                 ( NumeralConst _1 )
# 437 "parser.ml"
               : 'term_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                 ( BooleanConst _1 )
# 444 "parser.ml"
               : 'term_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                ( StringConst _1 )
# 451 "parser.ml"
               : 'term_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_exp) in
    Obj.repr(
# 67 "parser.mly"
             ( [_1] )
# 458 "parser.ml"
               : 'term_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_exp_list) in
    Obj.repr(
# 68 "parser.mly"
                                 ( _1 :: _3 )
# 466 "parser.ml"
               : 'term_exp_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 71 "parser.mly"
         ( [_1] )
# 473 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 72 "parser.mly"
                         ( _1 :: _3 )
# 481 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                             ( Func ("_empty_list", []) )
# 487 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list_body) in
    Obj.repr(
# 76 "parser.mly"
                                       ( _2 )
# 494 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
         ( Func ("_list", [_1; Func ("_empty_list", [])]) )
# 501 "parser.ml"
               : 'list_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_body) in
    Obj.repr(
# 80 "parser.mly"
                         ( Func ("_list", [_1; _3]) )
# 509 "parser.ml"
               : 'list_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 81 "parser.mly"
                   ( Func ("_list", [_1; _3]) )
# 517 "parser.ml"
               : 'list_body))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
;;
# 83 "parser.mly"

# 544 "parser.ml"
