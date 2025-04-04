open List;;

type exp = Num of int | Bl of bool | Var of string
         | Plus of exp * exp | Times of exp * exp
         | And of exp * exp | Or of exp * exp
         | Eq of exp * exp | Gt of exp * exp
         | Not of exp
         | Pair of exp * exp | Fst of exp | Snd of exp
         | IfTE of exp * exp * exp
         | Let of def * exp 
         | Abs of string * exp | App of exp * exp
and def = Def of string * exp | Seq of def * def | Par of def * def | Local of def * def;;
    
type opcodes = LDN of exp | LDB of exp | LOOKUP of string
             | PLUS | TIMES 
             | AND | OR 
             | EQ | GT 
             | NOT 
             | PAIR | FST | SND
             | IFTE 
             | LET of defcodes * opcodes list
             | MKCLOS of string * opcodes list | APP | RET | BIND of string
and defcodes = DEF of string * opcodes list | SEQ of defcodes * defcodes | PAR of defcodes * defcodes | LOCAL of defcodes * defcodes
;; 

let rec size e = match e with 
    Num _ -> 1
  | Bl _ -> 1
  | Var _ -> 1
  | Plus (e1, e2) -> 1 + (size e1) + (size e2) 
  | Times (e1, e2) -> 1 + (size e1) + (size e2) 
  | And (e1, e2) -> 1 + (size e1) + (size e2) 
  | Or (e1, e2) -> 1 + (size e1) + (size e2) 
  | Eq (e1, e2) -> 1 + (size e1) + (size e2) 
  | Gt (e1, e2) -> 1 + (size e1) + (size e2) 
  | Not e1 -> 1 + (size e1) 
  | Pair (e1, e2) -> 1 + (size e1) + (size e2)
  | Fst (e1) -> 1 + (size e1)
  | Snd (e1) -> 1 + (size e1)
  | IfTE (e0, e1, e2) -> 1 + (size e0) + (size e1) + (size e2)
  | _ -> 0
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
    
exception SyntaxError of exp;;
    
let rec elab d rho = match d with 
  | Def (s, e) -> let v = eval e rho in Valuation [{key = s; value = Va v}]
  | Seq (d1, d2) -> 
      let rho1 = elab d1 rho in 
      let rho2 = elab d2 (augment rho rho1) 
      in (augment rho1 rho2)
  | Par (d1, d2) -> 
      let rho1 = elab d1 rho
      and rho2 = elab d2 rho
      in (augment rho1 rho2)
  | Local (d1, d2) ->  
      let rho1 = elab d1 rho in
      let rho2 = elab d2 (augment rho rho1)
      in rho2
and eval e rho = match e with 
  | Num n -> N n
  | Bl b -> B b
  | Var x -> let Va v = get rho x in v
  | Plus (e1, e2) -> 
      let N n1 = (eval e1 rho)
      and N n2 = (eval e2 rho) 
      in N (n1 + n2)
  | Times (e1, e2) -> 
      let N n1 = (eval e1 rho)
      and N n2 = (eval e2 rho) 
      in N (n1 * n2)
  | And (e1, e2) -> 
      let B b1 = (eval e1 rho)
      and B b2 = (eval e2 rho) 
      in B (b1 && b2)
  | Or (e1, e2) -> 
      let B b1 = (eval e1 rho)
      and B b2 = (eval e2 rho) 
      in B (b1 || b2)
  | Eq (e1, e2) -> 
      let v1 = (eval e1 rho)
      and v2 = (eval e2 rho) 
      in B (v1 = v2)
  | Gt (e1, e2) -> 
      let N n1 = (eval e1 rho)
      and N n2 = (eval e2 rho) 
      in B (n1 > n2)
  | Not e1 -> let B b1 = (eval e1 rho) in B (not b1)
  | Pair (e1, e2) -> 
      let v1 = (eval e1 rho)
      and v2 = (eval e2 rho)
      in P (v1, v2)
  | Fst (e1) -> let P (v1, v2) = (eval e1 rho) in v1
  | Snd (e1) -> let P (v1, v2) = (eval e1 rho) in v2
  | IfTE (e0, e1, e2) -> 
      let B b0 = (eval e0 rho)
      and v1 = (eval e1 rho)
      and v2 = (eval e2 rho)
      in if (b0) then v1 else v2
  | Let (d, e) -> let rho1 = elab d rho in eval e (augment rho rho1) 
  | Abs (s, e) -> Clos (s, e, rho)
  | App (e1, e2) -> 
      let cl = eval e1 rho 
      and ar = eval e2 rho
      in (match cl with 
          | Clos (s, e', rho') -> let rho1 = Valuation [{key = s; value = Va ar}] in eval e' (augment rho' rho1))
  | _ -> raise (SyntaxError(e)) 
;;
    
let rec typeElab g d = match d with 
  | Def (s, e) -> let t = typeChecker g e in TypeAssumption [{key = s; value = Ty t}]
  | Seq (d1, d2) -> 
      let g1 = typeElab g d1 in 
      let g2 = typeElab (augment g g1) d2 
      in (augment g1 g2)
  | Par (d1, d2) -> 
      let g1 = typeElab g d1
      and g2 = typeElab g d2
      in (augment g1 g2)
  | Local (d1, d2) ->  
      let g1 = typeElab g d1 in
      let g2 = typeElab (augment g g1) d2
      in g2
and typeChecker g e = match e with 
    Num n -> IntT
  | Bl b -> BoolT 
  | Var x -> let Ty t = get g x in t
  | Plus (e1, e2) -> 
      let n1 = typeChecker g e1
      and n2 = typeChecker g e2
      in if (n1 = IntT && n2 = IntT) then IntT else InvalidT IntT
  | Times (e1, e2) -> 
      let n1 = typeChecker g e1
      and n2 = typeChecker g e2
      in if (n1 = IntT && n2 = IntT) then IntT else InvalidT IntT
  | And (e1, e2) -> 
      let b1 = typeChecker g e1
      and b2 = typeChecker g e2
      in if (b1 = BoolT && b2 = BoolT) then BoolT else InvalidT BoolT
  | Or (e1, e2) -> 
      let b1 = typeChecker g e1
      and b2 = typeChecker g e2
      in if (b1 = BoolT && b2 = BoolT) then BoolT else InvalidT BoolT
  | Eq (e1, e2) -> 
      let v1 = typeChecker g e1
      and v2 = typeChecker g e2
      in if (v1 = v2) then BoolT else InvalidT v1
  | Gt (e1, e2) -> 
      let n1 = typeChecker g e1
      and n2 = typeChecker g e2
      in if (n1 = IntT && n2 = IntT) then BoolT else InvalidT IntT
  | Not (e1) -> let b1 = (typeChecker g e1) in if (b1 = BoolT) then BoolT else InvalidT BoolT
  | Pair (e1, e2) ->
      let v1 = typeChecker g e1
      and v2 = typeChecker g e2
      in PairT(v1, v2)
  | Fst (e1) -> let PairT (v1, v2) = (typeChecker g e1) in v1
  | Snd (e1) -> let PairT (v1, v2) = (typeChecker g e1) in v2
  | IfTE (e0, e1, e2) -> 
      let b0 = typeChecker g e0
      and v1 = typeChecker g e1
      and v2 = typeChecker g e2
      in if ((b0 = BoolT) && (v1 = v2)) then v1 else (if b0 <> BoolT then InvalidT BoolT else InvalidT v1)
  | Let (d, e) -> let g1 = typeElab g d in typeChecker (augment g g1) e
  | Abs (s, e) -> 
      (match typeChecker g e with
       | InvalidT t1 -> match t1 with InvalidT _ -> InvalidT IntT | _ -> 
         let t2 = typeChecker (augment g (TypeAssumption [{key = s; value = Ty t1}])) e in 
         (match t2 with 
          | InvalidT _ -> InvalidT t2
          | _ -> AbsT (t1, t2)
         )
                                    | _ -> InvalidT IntT 
      )
  | App (e1, e2) -> 
      let t1 = typeChecker g e1
      and t2 = typeChecker g e2
      in 
      match t1 with 
      | AbsT (t3, t4) -> if (t3 = t2) then t4 else InvalidT t3
      | _ -> InvalidT (AbsT (t2, t2))
;;

let rec compile e = match e with
    Num n -> [LDN (Num n)]
  | Bl b -> [LDB (Bl b)]
  | Var x -> [LOOKUP x]
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS] 
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES] 
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND] 
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR] 
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ] 
  | Gt (e1, e2) -> (compile e1) @ (compile e2) @ [GT] 
  | Not e1 -> (compile e1) @ [NOT] 
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
  | s, (LOOKUP x)::c' -> let Va v = get g x in stkmc ((v)::s) g c' d
  | (N n2)::(N n1)::s, (PLUS)::c' -> stkmc (N (n1 + n2)::s) g c' d
  | (N n2)::(N n1)::s, (TIMES)::c' -> stkmc (N (n1 * n2)::s) g c' d
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

(* let x = 4 in x * 8 *)
(* let x = 6 + 3 ; z = x in z + 4 *)
(* let x = 5 in let y = 8 in let x = y || y = x in x + y *)
(* let local x = 4 in y = 2 + x in x * y *)

let codes = [Let ( Def ("x", Num 4), Times (Var "x", Num 8) );
             Let ( Seq ( Def ("x", Plus (Num 6, Num 3) ), Def ( "z", Var "x" ) ), Plus (Var "z", Num 4) );
             Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Par ( Def ("x", Var "y" ), Def ("y", Var "x" ) ) , Plus (Var "x", Var "y") ) ) );
             Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Local ( Def ("x", Num 4 ), Def ("y", Plus (Num 2, Var "x") ) ) , Times (Var "x", Var "y") ) ) )
            ];;

let _ = List.map (fun code -> stkmc [] (Table []) (compile code) []) codes 