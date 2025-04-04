
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
         | Match of exp * exp list * exp list
and def = Def of string * exp | Seq of def * def | Par of def * def | Local of def * def | DefRec of string * exp;; 

type clos = Clos of exp * table
and table_values = Cl of clos
and record = { key : string; value : table_values }
and table = Table of record list;;

let get f x = match f with 
  | Table r -> 
      let rc = List.find_opt (fun record -> record.key = x) r in
      (match rc with
       |  Some record -> record.value)
;;
    
let comb r1 r2 = List.fold_left
    (fun acc r ->
       if List.exists (fun rc -> rc.key = r.key) r2 then acc else r :: acc)
    r2
    r1
;;
    
let augment t1 t2 = match (t1, t2) with
  | (Table r1, Table r2) -> Table (comb r1 r2) 
;; 

exception Stuck of (clos * clos list);;

let rec krivmc c s = match c with 
  | Clos (Num n, env) -> Clos (Num n, env)
  | Clos (Bl b, env) -> Clos (Bl b, env)
  | Clos (Var x, env) -> let Cl v = get env x in krivmc v s
  | Clos (Plus (v1, v2), env) -> 
      let Clos (n1, env1) = krivmc (Clos (v1, env)) []
      and Clos (n2, env2) = krivmc (Clos (v2, env)) []
      in (match n1, n2 with 
          | Num n1, Num n2 -> krivmc (Clos (Num (n1+n2), env)) s
          | _ -> failwith "Cannot add non-numbers")
  | Clos (Times (v1, v2), env) -> 
      let Clos (n1, env1) = krivmc (Clos (v1, env)) []
      and Clos (n2, env2) = krivmc (Clos (v2, env)) []
      in (match n1, n2 with 
          | Num n1, Num n2 -> krivmc (Clos (Num (n1*n2), env)) s
          | _ -> failwith "Cannot multiply non-numbers")
  | Clos (Neg v, env) -> 
      let Clos (n1, env1) =  krivmc (Clos (v, env)) []
      in (match n1 with 
          | Num n1 -> krivmc (Clos (Num (-1*n1), env)) s
          | _ -> failwith "Cannot negate non-numbers")
  | Clos (And (v1, v2), env) -> 
      let Clos (b1, env1) = krivmc (Clos (v1, env)) []
      and Clos (b2, env2) = krivmc (Clos (v2, env)) []
      in (match b1, b2 with 
          | Bl b1, Bl b2 -> krivmc (Clos (Bl (b1||b2), env)) s
          | _ -> failwith "Cannot and non-booleans")
  | Clos (Or (v1, v2), env) -> 
      let Clos (b1, env1) = krivmc (Clos (v1, env)) []
      and Clos (b2, env2) = krivmc (Clos (v2, env)) []
      in (match b1, b2 with 
          | Bl b1, Bl b2 -> krivmc (Clos (Bl (b1&&b2), env)) s
          | _ -> failwith "Cannot or non-booleans")
  | Clos (Eq (v1, v2), env) -> 
      let Clos (n1, env1) = krivmc (Clos (v1, env)) []
      and Clos (n2, env2) = krivmc (Clos (v2, env)) []
      in krivmc (Clos (Bl (n1=n2), env)) s
  | Clos (Gt (v1, v2), env) -> 
      let Clos (n1, env1) = krivmc (Clos (v1, env)) []
      and Clos (n2, env2) = krivmc (Clos (v2, env)) []
      in (match n1, n2 with 
          | Num n1, Num n2 -> krivmc (Clos (Bl (n1>n2), env)) s
          | _ -> failwith "Cannot compare non-numbers")
  | Clos (Not v, env) -> 
      let Clos (b1, env1) = krivmc (Clos (v, env)) []
      in (match b1 with 
          | Bl b1 -> krivmc (Clos (Bl (not b1), env)) s
          | _ -> failwith "Cannot invert non-booleans")
  | Clos (Pair (v1, v2), env) -> c
  | Clos (Fst v, env) -> 
      let Clos (v1, env1) = krivmc (Clos (v, env)) []
      in (match v with 
          | Pair (v1, _) -> krivmc (Clos (v1, env)) s
          | _ -> failwith "Cannot project non-tuples")
  | Clos (Snd v, env) -> 
      let Clos (v1, env1) = krivmc (Clos (v, env)) []
      in (match v1 with 
          | Pair (_, v1) -> krivmc (Clos (v1, env)) s
          | _ -> failwith "Cannot project non-tuples")
  | Clos (IfTE (v0, v1, v2), env) -> 
      let Clos (b0, env0) = krivmc (Clos (v0, env)) [] 
      in (match b0 with 
          | Bl true -> krivmc (Clos (v1, env)) s
          | Bl false -> krivmc (Clos (v2, env)) s
          | _ -> failwith "Cannot use non-boolean as if-then-else condition") 
  | Clos (App (e1, e2), env) -> krivmc (Clos (e1, env)) ((Clos (e2, env))::s)
  | Clos (Abs (x, e), env) -> (match s with
      | cl::s' -> krivmc (Clos (e, (augment env (Table [{key = x; value = Cl cl}])))) s'
      | [] -> failwith "Cannot call function with empty argument")
  | Clos (Match (v, e1, e2), env) -> 
      let Clos (v1, env1) = krivmc (Clos (v, env)) []
      in let rec match_helper ca o = match ca, o with
          | [], _ -> failwith "Did not match any case"
          | (Var "_")::c', e'::o' -> krivmc (Clos (e', env)) []
          | cs::c', e'::o' -> 
              let Clos (v', env') = krivmc (Clos (cs, env)) [] 
              in if v1 = v' then krivmc (Clos (e', env)) [] else match_helper c' o'
      in match_helper e1 e2
  | Clos (Let (d, e), env) -> let env' = elabmc env d
      in krivmc (Clos (e, (augment env env'))) s 
  | _ -> raise (Stuck (c, s))
and elabmc env d = match d with
  | Def (s, e) -> Table [{key = s; value = Cl (Clos (e, env))}]
  | DefRec (s, e) -> let rec augenv = Table [{key = s; value = Cl (Clos (e, augenv))}] in Table [{key = s; value = Cl (Clos (e, augenv))}]
  | Seq (d1, d2) -> 
      let env1 = elabmc env d1 in
      let env2 = elabmc (augment env env1) d2
      in (augment env1 env2)
  | Par (d1, d2) ->
      let env1 = elabmc env d1
      and env2 = elabmc env d2
      in (augment env1 env2)
  | Local (d1, d2) -> 
      let env1 = elabmc env d1 in
      let env2 = elabmc (augment env env1) d2
      in env2 
;;

(*
let unpack (c: clos) = match c with 
  | Clos (Pair(e1, e2), env) -> 
      let v1 = krivmc (Clos (e1, env)) []
      and v2 = krivmc (Clos (e2, env)) []
      in (match v1, v2 with 
          | Clos (v1, _), Clos(v2, _) -> Pair(v1, v2)
          | _ -> failwith "Error unpacking pair")
  | Clos (v, env) -> v
;; 
*)

let unpack c = match c with 
  | Clos (v, env) -> v
;;

let eval e = unpack (krivmc (Clos (e, Table [])) []) ;; 
(* let def x = 4 in x * 8 ni *)
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
let x = 5 in
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
(* ((fun y -> y) (let def y = 5 in y + 3 ni) ) *)
(* 
let y = 3 in 
let def add3 x = x + y in 
let def y = 6 in 
add3 ( ( fun y -> y * 3 ) (1 + y) )
*)
(* let x = 1 + 2 in if (x = 3) then 1 else 0 *)
(* let x = (-1) + 2 in if (x = 1) then 1 else 0 *)
(* Fst of Pair ( Pair ( 3 * 6, false), omega) *)
(* Pair (4 + 6, false) *)
(* If ((fun x -> Fst of Pair (true, omega)) omega) then 1 else omega*)

let omega = App ( Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x")));;

let codes = [
  Let ( Def ("x", Num 4), Times (Var "x", Num 8) );
  Let (Def ("x", Num 5), Let(Local (Def ("x", Num 4), Def ("y", Plus(Num 2, Var "x"))), Times(Var "x", Var "y") ));
  Let ( Seq ( Def ("x", Plus (Num 6, Num 3) ), Def ( "z", Var "x" ) ), Plus (Var "z", Num 4) );
  Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Par ( Def ("x", Var "y" ), Def ("y", Var "x" ) ) , Plus (Var "x", Var "y") ) ) );
  Let ( Def ("x", Num 5), Let ( Def ("y", Num 8), Let ( Local ( Def ("x", Num 4 ), Def ("y", Plus (Num 2, Var "x") ) ) , Times (Var "x", Var "y") ) ) );
  Let ( Par ( Local(Def ("x", Times(Num 4, Plus(Num 3, Num 5))), Def ("y", Plus(Num 2, Var "x"))) , Local(Def ("x", Plus(Num 2, Num 8)), Def("z", Plus(Times(Num 9, Var "x"), Num 1))) ) , Gt(Times(Num 7, Num 9), Plus(Var "y", Var "z")) );
  App ( Abs("x", Times(Var "x", Num 2)) , Num 3);
  App (Abs ("y" , Var "y"),Let ( Def ("y", Num 5),Plus (Num 5, Num 3) ) );
  Let (Def ("y", Num 3), Let (Def ("add3", Abs("x", Plus(Var "x", Var "y"))), Let (Def ("y", Num 6), App (Var "add3", App ( Abs ("y", Times (Num 3, Var "y") ),  Plus (Num 1, Var "y"))  ) ) ) );
  Let (Def ("x", Plus (Num 1, Num 2)), IfTE (Eq (Var "x", Num 3), Num 1, Num 0));
  Let (Def ("x", Plus (Neg (Num 1), Num 2)), IfTE (Eq (Var "x", Num 1), Plus(Var "x", Num 1), Var "x"));
  Fst (Pair (Pair( Times (Num 3, Num 6), Bl false), omega));
  Pair (Plus (Num 4, Num 6), Bl false);
  IfTE (App ( Abs ("x", Snd (Pair (omega, Bl true))), omega) ,Num 1,omega);
  Let ( Def("x", Plus (Num 3, Num 5)), Match (Var "x", [Num 7; Num 3; Var "_"], [Num 1; Num 2; Num 3] ) );
  Let ( Def("x", Plus (Num 3, Num 5)), Match (Var "x", [Times (Num 2, Num 4); omega; Num 8], [Num 1; Num 2; Num 3] ) );
  Let ( DefRec ( "fact", Abs ("x", IfTE( Eq(Var "x", Num 0), Num 1 , Times( Var "x", App ( Var "fact", Plus (Var "x", Neg (Num 1) ) ) ) ) )), App (Var "fact" , Num 10 ) );
  Let ( DefRec ( "add", Abs ("x", IfTE( Eq(Var "x", Num 0), Num 1 , Plus( Num 1, App ( Var "add", Plus (Var "x", Neg (Num 1) ) ) ) ) )), App (Var "add" , Num 10 ) );
];;

let _ = List.map eval codes 
