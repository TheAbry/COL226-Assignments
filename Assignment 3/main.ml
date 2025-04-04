type keywords = 
  | IF 
  | THEN
  | ELSE
  | LET 
  | ENDIF 
  | PAIR 
  | FST
  | SND
  | IN
;;

type token = 
  | Identifier of string
  | Keyword of keywords 
  | NumeralConst of int
  | BooleanConst of bool
  | ArithOp of string
  | BoolOp of string
  | CompOp of string
  | StringConst of string
  | StringOp of string
  | ParenOpen of string
  | ParenClose of string
  | Comma
  | SemiColon
  | Colon
  | Equals
  | InvalidToken of string
;;

let keyword_to_str s = match s with 
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | LET -> "let"
  | ENDIF -> "endif"
  | PAIR -> "pair"
  | FST -> "first"
  | SND -> "second"
  | IN -> "in"
;;

let is_alpha a = match a with 'a' .. 'z' -> true  | 'A' .. 'Z' -> true  | _ -> false;;

let is_digit d = match d with '0' .. '9' -> true | _ -> false;;

let is_identifier_symbol s = is_alpha s || is_digit s || s == '_' || s == '\'' ;;

let is_identifier_start s = match s with 'a' .. 'z' -> true | '_' -> true | _ -> false;;

let is_end_symbol t = match t with ' ' -> true | '\n' -> true | '\t' -> true | ';' -> true | ':'  -> true | '=' -> true | _ -> false ;;

let is_arith_op o = match o with '+' -> true | '-' -> true | '*' -> true | '/' -> true | '%' -> true | _ -> false;;

let is_bool_op o = match o with '&' -> true | '|' -> true | '!' -> true | _ -> false;;

let is_string_op o = match o with '^' -> true | _ -> false;;

let is_paren p = match p with '(' -> true | '[' -> true | '{' -> true | '}' -> true | ']' ->true | ')' ->true | _ -> false;;

let is_comp_op_start o = match o with '>' -> true | '<' -> true | '=' -> true | '!' -> true | _ -> false;;

let is_quote q = match q with '"' -> true | _ -> false;;

let is_term t = is_end_symbol t || is_arith_op t  || is_comp_op_start t || is_paren t || is_bool_op t || is_string_op t;;

let rec invalid_token_handler sofar rem = match rem with
  [] -> InvalidToken sofar, rem
  | c :: rest when is_term c -> InvalidToken sofar, rem
  | c :: rest -> invalid_token_handler (sofar ^ (String.make 1 c)) rest
;;

let rec extract_identifier sofar rem = match rem with 
  | [] -> Identifier sofar, rem
  | c :: rest when is_term c -> Identifier sofar, rem
  | c :: rest when is_identifier_symbol c -> (extract_identifier (sofar ^ (String.make 1 c)) rest)
  | _ -> invalid_token_handler sofar rem
  (* | _ -> (Identifier sofar, rem) *)
;;

let rec extract_numeral sofar rem = match rem with 
  | [] -> NumeralConst sofar, rem
  | d :: rest when is_term d -> NumeralConst sofar, rem
  | d :: rest when is_digit d -> (extract_numeral (10 * sofar + int_of_string (String.make 1 d)) rest)
  | _ -> (invalid_token_handler (string_of_int(sofar)) rem)
  (* | _ -> (NumeralConst sofar, rem) *)
;;

let rec extract_string sofar rem op = match rem with
  | [] -> InvalidToken (String.make 1 op), List.init (String.length sofar) (String.get sofar) @ rem
  | c :: rest when c == op -> StringConst sofar, rest
  | c :: rest -> extract_string (sofar ^ (String.make 1 c)) rest op
;;

let rec tokeniser str = match str with 
  [] -> []
  | 't' :: 'r' :: 'u' :: 'e' :: c :: rest when is_end_symbol c -> (BooleanConst true) :: tokeniser rest
  | 't' :: 'a' :: 'l' :: 's' :: 'e' :: c :: rest when is_end_symbol c -> (BooleanConst false) :: tokeniser rest
  | '=' :: '=' :: rest -> (CompOp "==") :: tokeniser rest
  | '<' :: '=' :: rest -> (CompOp "<=") :: tokeniser rest
  | '>' :: '=' :: rest -> (CompOp ">=") :: tokeniser rest
  | '<' :: rest -> (CompOp "<") :: tokeniser rest
  | '>' :: rest -> (CompOp ">") :: tokeniser rest
  | '!' :: '=' :: rest -> (CompOp "!=") :: tokeniser rest
  | ';' :: rest -> SemiColon :: tokeniser rest
  | ':' :: rest -> Colon :: tokeniser rest
  | '=' :: rest -> Equals :: tokeniser rest
  | c :: rest when is_paren c -> ParenOpen (String.make 1 c) :: tokeniser rest
  | c :: rest when is_paren c -> ParenClose (String.make 1 c) :: tokeniser rest
  | 'i' :: 'f' :: c :: rest when is_end_symbol c -> (Keyword IF) :: tokeniser rest
  | 't' :: 'h' :: 'e' :: 'n' :: c :: rest when is_term c -> (Keyword THEN) :: tokeniser rest
  | 'e' :: 'l' :: 's' :: 'e' :: c :: rest when is_term c -> (Keyword ELSE) :: tokeniser rest
  | 'e' :: 'n' :: 'd' :: 'i' :: 'f' :: c :: rest when is_term c -> (Keyword ENDIF) :: tokeniser rest
  | 'p' :: 'a' :: 'i' :: 'r' :: c :: rest when is_term c -> (Keyword PAIR) :: tokeniser rest
  | 'f' :: 's' :: 't' :: c :: rest when is_term c -> (Keyword FST) :: tokeniser rest
  | 's' :: 'n' :: 'd' :: c :: rest when is_term c -> (Keyword SND) :: tokeniser rest
  | 'i' :: 'n' :: c :: rest when is_term c -> (Keyword IF) :: tokeniser rest
  | c :: rest when is_arith_op c -> (ArithOp (String.make 1 c)) :: tokeniser rest
  | c :: rest when is_bool_op c -> (BoolOp (String.make 1 c)) :: tokeniser rest
  | c:: rest when is_string_op c -> (StringOp (String.make 1 c)) :: tokeniser rest
  | c :: rest when is_identifier_start c -> 
    let (identifier, rem) = extract_identifier "" (c::rest) 
    in identifier :: tokeniser rem 
  | d :: rest when is_digit d -> 
    let (numeral, rem) = extract_numeral 0 (d::rest)
  in numeral :: tokeniser rem
  | c :: rest when is_end_symbol c -> tokeniser rest
  | c :: rest when is_quote c -> 
    let (strconst, rem) = extract_string "" rest c 
  in strconst :: tokeniser rem
  (* | t :: rest -> InvalidToken (String.make 1 t) :: tokeniser rest *)
  | t :: rest -> let (tok, rem) = invalid_token_handler "" (t::rest)
  in tok :: tokeniser rem
;;

let tokenise s =
  let tokens = tokeniser (List.of_seq (String.to_seq s)) in
    print_endline (String.concat "\n" (List.map (fun t -> match t with
    | Identifier s -> "Identifier " ^ s
    | Keyword s -> "Keyword " ^ keyword_to_str s
    | BoolOp s -> "BooleanOperator " ^ s
    | BooleanConst b -> "BoolConstant " ^ string_of_bool b
    | ArithOp s -> "ArithmeticOperator " ^ s
    | NumeralConst i -> "IntConstant " ^ string_of_int i
    | CompOp s -> "ComparisonOperator " ^ s
    | StringOp s -> "StringOperator " ^ s
    | StringConst s -> "StrConstant \"" ^ s ^ "\""
    | ParenOpen s -> "Left Parentheses " ^ s
    | ParenClose s -> "Right Parentheses " ^ s
    | Comma -> "Comma "
    | SemiColon -> "Semicolon "
    | Colon -> "Colon "
    | Equals -> "Equals "
    | InvalidToken s -> "Invalid token " ^ s) tokens))
;;

tokenise "_val 123 aB 2a1 'helo Abc aBYe true";;
tokenise "(3246 + abc)&gh!hf Y0123";;
tokenise "ifthen else truefalse|true^\"hello\n\"hi\" 'abc12 3";;
tokenise "fst pair(5, _'64) sndth";;
tokenise "Abc(123)>=45!=abc==for;true== when let:in=help";;


(* RULES *)
(* 1. Identifiers *)
(* 2. Numeric constants *)
(* 3. Boolean constants start with a capital letter True, False *)
(* 4. The same goes for keywords If, Then, Else, Endif, Let, Pair, Fst and Snd *)
(* 5. Strings have the ^ operator for concatenation *)
(* 6.  *)
