open List;;

type exp = Num of int | Bl of bool | Var of string
         | Plus of exp * exp | Times of exp * exp | Neg of exp
         | And of exp * exp | Or of exp * exp
         | Eq of exp * exp | Gt of exp * exp
         | Not of exp
         | Pair of exp * exp | Fst of exp | Snd of exp
         | IfTE of exp * exp * exp
         | Let of def * exp 
         | Abs of string * exp | App of exp * exp
and def = Def of string * exp | Seq of def * def | Par of def * def | Local of def * def;;
    
type opcodes = LDN of exp | LDB of exp | LOOKUP of string
             | PLUS | TIMES | NEG
             | AND | OR 
             | EQ | GT 
             | NOT 
             | PAIR | FST | SND
             | IFTE 
             | LET of defcodes * opcodes list
             | MKCLOS of string * opcodes list | APP | RET | BIND of string
and defcodes = DEF of string * opcodes list | SEQ of defcodes * defcodes | PAR of defcodes * defcodes | LOCAL of defcodes * defcodes
;; 

type values = N of int | B of bool | P of values * values | Clos of string * exp * table | ClosI of string * opcodes list * table
and types = IntT | BoolT | PairT of types * types | AbsT of types * types | InvalidT of types
and table_values = Ty of types | Va of values
and record = { key : string; value : table_values }
and table = TypeAssumption of record list | Table of record list | Valuation of record list;;


let get f x = match f with 
  | TypeAssumption r -> 
      let rc = List.find_opt (fun record -> record.key = x) r in
      (match rc with
       | Some record -> record.value
       | None -> Ty (InvalidT IntT))
  | Table r -> 
      let rc = List.find_opt (fun record -> record.key = x) r in
      (match rc with
       |  Some record -> record.value
       | None -> Ty (InvalidT IntT))
  | Valuation r -> 
      let rc = List.find_opt (fun record -> record.key = x) r in
      (match rc with
       |  Some record -> record.value
       | None -> Ty (InvalidT IntT)) 
;;
    
let comb r1 r2 = List.fold_left
    (fun acc r ->
       if List.exists (fun rc -> rc.key = r.key) r2 then acc else r :: acc)
    r2
    r1
;;
    
let augment t1 t2 = match (t1, t2) with
  | (TypeAssumption r1, TypeAssumption r2) -> TypeAssumption (comb r1 r2) 
  | (Table r1, Table r2) -> Table (comb r1 r2) 
  | (Valuation r1, Valuation r2) -> Valuation (comb r1 r2) 
;;

let rec compile e = match e with
    Num n -> [LDN (Num n)]
  | Bl b -> [LDB (Bl b)]
  | Var x -> [LOOKUP x]
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS] 
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES] 
  | Neg (e1) -> (compile e1) @ [NEG]
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND] 
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR] 
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ] 
  | Gt (e1, e2) -> (compile e1) @ (compile e2) @ [GT] 
  | Not (e1) -> (compile e1) @ [NOT] 
  | Pair (e1, e2) -> (compile e1) @ (compile e2) @ [PAIR]
  | Fst (e1) -> (compile e1) @ [FST]
  | Snd (e1) -> (compile e1) @ [SND]
  | IfTE (e0, e1, e2) -> (compile e0) @ (compile e1) @ (compile e2) @ [IFTE] 
  | Let (d, e) -> [LET (compileDef d, (compile e) @ [RET])]
  | Abs (s, e) -> [MKCLOS (s, (compile e) @ [RET])]
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [APP] 
and compileDef d = match d with 
  | Def (s, e) -> DEF (s, compile e)
  | Seq (d1, d2) -> SEQ (compileDef d1, compileDef d2)
  | Par (d1, d2) -> PAR (compileDef d1, compileDef d2)
  | Local (d1, d2) -> LOCAL (compileDef d1, compileDef d2)
;;

exception Stuck of (values list * opcodes list);;
    
let rec stkmc s g c d = match s, c with
    x::_, [] -> x
  | s, (LDN (Num n))::c' -> stkmc ((N n)::s) g c' d
  | s, (LDB (Bl b))::c' -> stkmc ((B b)::s) g c' d
  | s, (LOOKUP x)::c' -> let Va v = get g x in stkmc ((v)::s) g c' d; 
  | (N n2)::(N n1)::s, (PLUS)::c' -> stkmc (N (n1 + n2)::s) g c' d
  | (N n2)::(N n1)::s, (TIMES)::c' -> stkmc (N (n1 * n2)::s) g c' d
  | (N n)::s, (NEG)::c' -> stkmc (N (-1 * n)::s) g c' d
  | (B b2)::(B b1)::s, (AND)::c' -> stkmc (B (b1 && b2)::s) g c' d
  | (B b2)::(B b1)::s, (OR)::c' -> stkmc (B (b1 || b2)::s) g c' d
  | (v2)::(v1)::s, (EQ)::c' -> stkmc (B (v1 = v2)::s) g c' d
  | (N n2)::(N n1)::s, (GT)::c' -> stkmc (B (n1 > n2)::s) g c' d
  | (B b)::s, (NOT)::c' -> stkmc (B (not b)::s) g c' d 
  | (v2)::(v1)::s, (PAIR)::c' -> stkmc (P (v1, v2)::s) g c' d
  | (P (v1, v2))::s, (FST)::c' -> stkmc (v1::s) g c' d
  | (P (v1, v2))::s, (SND)::c' -> stkmc (v2::s) g c' d
  | (v2)::(v1)::(B b0)::s, (IFTE)::c' -> if (b0) then stkmc (v1::s) g c' d else stkmc (v2::s) g c' d 
  | s, (LET (de, co))::c' -> let t' = compileElab g de
      in stkmc [] (augment g t') co ((s, g, c')::d)
  | (a::s), (BIND x)::c' -> ClosI ("", [], Table [{key = x; value = Va a}])
  | s, (MKCLOS (st, co))::c' -> stkmc (ClosI (st, co, g)::s) g c' d 
  | (v)::(ClosI (st, co, ga))::s, (APP)::c' -> 
      let t = Table [{key = st; value = Va v}]
      in stkmc [] (augment ga t) co ((s, g, c')::d)
  | (x)::_, (RET)::_ -> 
      (match d with
       | (st, ga, co)::d' -> stkmc (x::st) ga co d')
  | _, _ -> raise (Stuck (s, c)) 
and compileElab g d = match d with
  | DEF (s, co) -> let a = stkmc [] g co [] in Table [{key = s; value = Va a}]
  | SEQ (d1, d2) -> 
      let g1 = compileElab g d1 in
      let g2 = compileElab (augment g g1) d2
      in (augment g1 g2)
  | PAR (d1, d2) ->
      let g1 = compileElab g d1
      and g2 = compileElab g d2
      in (augment g1 g2)
  | LOCAL (d1, d2) -> 
      let g1 = compileElab g d1 in
      let g2 = compileElab (augment g g1) d2
      in g2 
;;  

(* let def x = 4 in x * 8 ni *)
(* let def x = 6 + 3 ; def z = x in z + 4 ni *)
(* 
let def x = 5 in 
let def y = 8 in 
let def x = y || def y = x in 
x + y 
ni 
ni 
ni
*)
(* 
let 
local def  x = 4 in def y = 2 + x ni 
in 
x * y 
ni
*)
(* 
let 
local def x = 4 * (3 + 5) in def y = 2 + x ni
and 
local def x = 2 + 8 in def z = 9 * x + 1 ni
in 
7 * 9 > y + z 
ni 
*)
(* ((fun x -> x * 2) 3) *)
(* 
let y = 3 in 
let def add3 x = x + y in 
let def y = 6 in 
add3 ( ( fun y -> y + 3 ) (1 + y) )
*)
(* let x = 1 + 2 in if (x = 3) then 1 else 0 *)
(* let x = (-1) + 2 in if (x = 1) then 1 else 0 *)

let codes = [Let ( Def ("x", Num 4), Times (Var "x", Num 8) );
             Let ( Seq ( Def ("x", Plus (Num 6, Num 3) ), Def ( "z", Var "x" ) ), Plus (Var "z", Num 4) );
             Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Par ( Def ("x", Var "y" ), Def ("y", Var "x" ) ) , Plus (Var "x", Var "y") ) ) );
             Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Local ( Def ("x", Num 4 ), Def ("y", Plus (Num 2, Var "x") ) ) , Times (Var "x", Var "y") ) ) );
             Let ( Par ( Local(Def ("x", Times(Num 4, Plus(Num 3, Num 5))), Def ("y", Plus(Num 2, Var "x"))) , Local(Def ("x", Plus(Num 2, Num 8)), Def("z", Plus(Times(Num 9, Var "x"), Num 1))) ) , Gt(Times(Num 7, Num 9), Plus(Var "y", Var "z")) );
             App ( Abs("x", Times(Var "x", Num 2)) , Num 3);
             Let (Def ("y", Num 3), Let (Def ("add3", Abs("x", Plus(Var "x", Var "y"))), Let (Def ("y", Num 6), App (Var "add3", App ( Abs ("y", Times (Num 3, Var "y") ),  Plus (Num 1, Var "y"))  ) ) ) );
             Let (Def ("x", Plus (Num 1, Num 2)), IfTE (Eq (Var "x", Num 3), Num 1, Num 0));
             Let (Def ("x", Plus (Neg (Num 1), Num 2)), IfTE (Eq (Var "x", Num 1), Num 1, Num 0));
            ];;

let _ = List.map (fun code -> stkmc [] (Table []) (compile code) []) codes 
