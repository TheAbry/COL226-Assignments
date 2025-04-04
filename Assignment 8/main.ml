open Ast

let rec string_of_term = function
  | NumeralConst n -> "NumeralConst " ^ string_of_int n
  | BooleanConst b -> "BooleanConst " ^ b
  | StringConst s -> "StringConst " ^ "\"" ^ s ^ "\""
  | VarExp v -> "VarExp " ^ v
  | Func (name, args) -> "\nFunc (" ^ name ^ ", [" ^ String.concat ", " (List.map string_of_term args) ^ "])"

let rec string_of_term_exp = function
  | AtomicFormula (name, args) -> "\nAtomicFormula (" ^ name ^ ", [" ^ String.concat ", " (List.map string_of_term args) ^ "])"
  | BooleanConst b -> "BooleanConst " ^ b

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

let test_cases = [
  "sibling(X, Y).";
  "parent_child(Z, X):- parent_child(Z, Y).";
  "if fail then fail else false endif.";
]

let get_ast lexbuf =
  let finished = ref false in
  let result = ref None in
  while not !finished do
    try
      let parsed_result = Parser.program Lexer.token lexbuf in
      result := Some parsed_result;
      finished := true 
    with
    | Lexer.LexicalError msg ->
      failwith ("Lexical error: " ^ msg)
    | Stdlib.Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let pos_str = Printf.sprintf "line %d, character %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
      failwith ("Parse error at " ^ pos_str)
  done;
  !result

let test_cases = [
  "sibling(X, Y).";
  "sibling(adam, eve).\nparent_child(Z, X):- parent_child(Z, Y), sibling(X, Y).";
];;

let lists = List.map (fun input ->
  let lexbuf = Lexing.from_string input in
  print_endline input;
  match get_ast lexbuf with
  | Some ast -> ast
  | None -> failwith "Error in program"
) test_cases;;

let ast_of_string s = 
  match get_ast (Lexing.from_string s) with
  | Some ast -> ast
  | None -> failwith "Error in program"
;;

let rec transform_program p =
  let rec transform_term i t = (match t with 
    | VarExp v -> VarExp ((string_of_int i) ^ v)
    | Func (f, ls) -> Func (f, (List.map (transform_term i) ls))
    | _ -> t
  )
  in 
  let transform_term_exp i t = (match t with 
    | AtomicFormula (a, ls) -> AtomicFormula (a, (List.map (transform_term i) ls))
    | _ -> t
  )
  in
  let transform_clause i c = (match c with 
    | Fact f -> Fact (transform_term_exp i f)
    | Rule (h, b) -> Rule ((transform_term_exp i h), (List.map (transform_term_exp i) b))
  )
  in 
  let rec iter_prog i p = (match p with
    | [] -> []
    | (Comment _)::rest -> iter_prog (i+1) rest
    | cl::rest -> (transform_clause i cl)::(iter_prog (i+1) rest)
  )
  in iter_prog 0 p

let rec transform_program2 p at =
  let rec transform_term t = (match t with 
    | VarExp v -> VarExp ("#" ^ v)
    | Func (f, ls) -> Func (f, (List.map transform_term ls))
    | _ -> t
  )
  in 
  let transform_term_exp t = (match t with 
    | AtomicFormula (a, ls) -> AtomicFormula (a, (List.map transform_term ls))
    | _ -> t
  )
  in
  let transform_clause c = (match c with 
    | Fact f -> Fact (transform_term_exp f)
    | Rule (h, b) -> Rule ((transform_term_exp h), (List.map transform_term_exp b))
  )
  in 
  let rec iter_prog (AtomicFormula (a, _)) p = (match p with
    | [] -> []
    | (Comment _)::rest -> iter_prog (AtomicFormula (a, [])) rest
    | cl::rest -> 
      match cl with 
      Fact (AtomicFormula (a', _)) | Rule (AtomicFormula (a', _), _) ->
      if a = a' then (transform_clause cl)::(iter_prog (AtomicFormula (a, [])) rest)
      else cl::(iter_prog (AtomicFormula (a, [])) rest)
  )
  in iter_prog at p

let rec vars t = 
  match t with
  | VarExp s -> [s]
  | Func (a, ls) -> List.flatten (List.map vars ls)
  | _ -> []
;;

let vars_atomic (AtomicFormula(a, ls)) = 
  vars (Func (a, ls))

type subst_pt = string * term;;
type substitution = subst_pt list;;

exception VariableNotFound;;

let get_subst s v =
  let tup = List.find_opt (fun (var, term) -> var = v) s in 
  match tup with 
    | Some (var, term) -> term
    | None -> raise VariableNotFound
;;

let rec subst s t =
  match t with
  | VarExp v ->
      (try get_subst s v
       with VariableNotFound -> t)
  | Func (a, ls) ->
      Func (a, List.map (subst s) ls)
  | _ -> t
;;

let subst_atomic s (AtomicFormula (a, ls)) = 
  subst s (Func (a, ls))

let compose_subst s1 s2 =
  let rec compose_subst_helper sub2 acc = match sub2 with
    | [] -> acc
    | (var, t)::rest -> compose_subst_helper rest (if (List.exists (fun (x, y) -> x = var) s1) then acc else (var, t)::acc)
  and res = List.map (fun (x, y) -> (x, (subst s2 y))) s1
  in compose_subst_helper s2 res
;;

exception NOT_UNIFIABLE;;

let rec mgu t1 t2 = match t1, t2 with
  | NumeralConst a1, NumeralConst a2 -> if (a1 = a2) then [] else raise NOT_UNIFIABLE
  | BooleanConst a1, BooleanConst a2 -> if (a1 = a2) then [] else raise NOT_UNIFIABLE
  | StringConst a1, StringConst a2 -> if (a1 = a2) then [] else raise NOT_UNIFIABLE
  | VarExp x, VarExp y -> if (x = y) then [] else [(x, t2)]
  | VarExp x, Func (a, ls) -> if (List.mem x (vars t2)) then raise NOT_UNIFIABLE else [(x, t2)]
  | Func (a, ls), VarExp x -> if (List.mem x (vars t1)) then raise NOT_UNIFIABLE else [(x, t1)]
  | VarExp x, _ -> [(x, t2)]
  | _, VarExp x -> [(x, t1)]
  | Func (a1, ls1), Func (a2, ls2) -> 
    if (a1 = a2) && ((List.length ls1) == (List.length ls2)) 
    then List.fold_left2 (fun sub x y -> compose_subst sub (mgu (subst sub x) (subst sub y))) [] ls1 ls2
    else raise NOT_UNIFIABLE
  | _, _ -> raise NOT_UNIFIABLE
;; 

let mgu_atomic (AtomicFormula (a1, ls1)) (AtomicFormula (a2, ls2)) = 
  mgu (Func (a1, ls1)) (Func (a2, ls2))

let rec mather t = match t with
  | NumeralConst _ -> t
  | BooleanConst _ -> t
  | StringConst _ -> t
  | Func ("+", [t1; t2]) -> 
      (match (mather t1), (mather t2) with
        | NumeralConst n1, NumeralConst n2 -> NumeralConst (n1 + n2)
        | _ -> raise NOT_UNIFIABLE)
    
  | Func("-", [t1; t2]) -> 
      (match (mather t1), (mather t2) with
        | NumeralConst n1, NumeralConst n2 -> NumeralConst (n1 - n2)
        | _ -> raise NOT_UNIFIABLE)
    
  | Func("*", [t1; t2]) -> 
      (match (mather t1), (mather t2) with
        | NumeralConst n1, NumeralConst n2 -> NumeralConst (n1 * n2)
        | _ -> raise NOT_UNIFIABLE)
    
  | Func("/", [t1; t2]) -> 
      (match ((mather t1), (mather t2)) with
        | NumeralConst n1, NumeralConst n2 -> NumeralConst (n1 / n2)
        | _ -> raise NOT_UNIFIABLE)

  | Func("^", [t1; t2]) -> 
      (match ((mather t1), (mather t2)) with
        | StringConst s1, StringConst s2 -> StringConst (s1 ^ s2)
        | _ -> raise NOT_UNIFIABLE)

  | _ -> t
;;

let rec calc e sub = match e with 
  | AtomicFormula ("=", [a1;a2])
  | AtomicFormula ("!=", [a1;a2]) -> compose_subst sub (mgu (mather (subst sub a1)) (mather (subst sub a2)))
  | AtomicFormula ("==", [a1;a2]) -> 
    (match mather (subst sub a1), mather (subst sub a2) with
      | NumeralConst n1, NumeralConst n2 -> if n1 == n2 then sub else raise NOT_UNIFIABLE
      | BooleanConst b1, BooleanConst b2 -> if b1 == b2 then sub else raise NOT_UNIFIABLE
      | StringConst s1, StringConst s2 -> if s1 == s2 then sub else raise NOT_UNIFIABLE
      | _ -> raise NOT_UNIFIABLE)
  | AtomicFormula (">=", [a1;a2]) -> 
    (match mather (subst sub a1), mather (subst sub a2) with
      | NumeralConst n1, NumeralConst n2 -> if n1 >= n2 then sub else raise NOT_UNIFIABLE
      | _ -> raise NOT_UNIFIABLE)
  | AtomicFormula (">", [a1;a2]) -> 
    (match mather (subst sub a1), mather (subst sub a2) with
      | NumeralConst n1, NumeralConst n2 -> if n1 > n2 then sub else raise NOT_UNIFIABLE
      | _ -> raise NOT_UNIFIABLE)
  | AtomicFormula ("<=", [a1;a2]) -> 
    (match mather (subst sub a1), mather (subst sub a2) with
      | NumeralConst n1, NumeralConst n2 -> if n1 <= n2 then sub else raise NOT_UNIFIABLE
      | _ -> raise NOT_UNIFIABLE)
  | AtomicFormula ("<", [a1;a2]) -> 
    (match mather (subst sub a1), mather (subst sub a2) with
      | NumeralConst n1, NumeralConst n2 -> if n1 < n2 then sub else raise NOT_UNIFIABLE
      | _ -> raise NOT_UNIFIABLE)

let a1 = ast_of_string "sibling(arunabh, ave).";;
let a2 = "sibling(X, Y).";;

exception FAIL;;

let rec interpret_program prog goals sub v = 
  match goals with 
  | [] -> 
    let filtered_vars = List.filter (fun (x, _) -> List.mem x v) sub
    in 
    let _ = 
      (if List.length filtered_vars = 0 
      then print_endline "true." 
      else List.iter (fun (x, y) -> print_string x; print_string " = "; print_string (string_of_term y); print_string ", ") filtered_vars; print_string "\n--\n")
      in (false, [])
  | g::gs -> match g with
      | AtomicFormula ("=", _) 
      | AtomicFormula (">=", _) 
      | AtomicFormula (">", _) 
      | AtomicFormula ("<=", _) 
      | AtomicFormula ("<", _) -> (
        try interpret_program prog gs (calc g sub) v 
        with NOT_UNIFIABLE -> (false, [])
        )
      | AtomicFormula ("!=", _) -> (
        try (false, calc g sub)
        with NOT_UNIFIABLE -> interpret_program prog gs sub v
        )
      | BooleanConst "cut" -> let _ = interpret_program prog gs sub v in (true, List.filter (fun (x, _) -> List.mem x v) sub)
      | BooleanConst "fail" -> raise FAIL
      | BooleanConst "false" -> (false, [])
      | AtomicFormula (a, ls) -> (
        let nprog = transform_program2 prog g in
        let rec iter prog = 
          (match prog, g with 
            | [], _ -> (false, [])
            | _, AtomicFormula ("!", [a]) -> (
              try
                let x = match a with | Func (s, l) -> AtomicFormula (s, l) | BooleanConst b -> BooleanConst b 
                in (match (interpret_program nprog (x :: (BooleanConst "cut") :: (BooleanConst "fail") :: gs) sub v) with
                  | (false, unif') -> (true, unif')
                  | _ -> (false, []))
              with FAIL -> (false, [])
              )
            | (Fact f)::pr, _ -> (
              try 
                let unif = compose_subst sub (mgu (subst_atomic sub f) (subst_atomic sub g))
                in (match (interpret_program nprog gs unif v) with
                  | (true, unif') -> (true, unif')
                  | _ -> iter pr)
              with 
              | NOT_UNIFIABLE -> iter pr
              | FAIL -> raise FAIL
              )
            | (Rule (h, b))::pr, _ -> (
              try 
                let unif = compose_subst sub (mgu (subst_atomic sub h) (subst_atomic sub g))
                in (match (interpret_program nprog (b @ gs) unif v) with
                  | (true, unif') -> (true, unif')
                  | _ -> iter pr)
              with 
              | NOT_UNIFIABLE -> iter pr
              | FAIL -> raise FAIL
              )
          )
         in iter prog)
      | _ -> raise NOT_UNIFIABLE
;;

let rec ast_to_goal t = match t with 
  | [] -> []
  | (Fact f)::ls -> f::(ast_to_goal ls)
  | _ -> failwith "Invalid query"
;;

let goal_of_string s = ast_to_goal (ast_of_string s);;

let query p s = 
  let qs = goal_of_string s 
  in interpret_program (transform_program p) qs [] (List.flatten (List.map vars_atomic qs))