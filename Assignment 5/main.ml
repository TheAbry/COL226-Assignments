open List;;

type symbol = string * int;;
type signature = symbol list;;


let check_sig s = 
  let rec check_sig_helper sy acc = match sy with 
    | [] -> true
    | (sym, arity)::rest -> 
      if ((List.exists (fun (r, p) -> r = sym) acc) || (arity < 0))
      then false
      else check_sig_helper rest ((sym, arity) :: acc)
  in check_sig_helper s []
;; 

let sig1 = [("0", 0); ("1", 0); ("2", 0); ("+", 2); ("*", 2); ("-", 1); ("+4", 4)];;
let nsig1 = [("0", 0); ("0", 1); ("+", 2); ("*", 2); ("-", 1)];;
let nsig2 = [("0", 0); ("1", -1); ("+", 2); ("*", 2); ("-", 1)];;

check_sig sig1;;
check_sig nsig1;;
check_sig nsig2;;


type tree = 
  | Empty
  | V of string 
  | C of {node: symbol; children: tree list};; 

let x = V "x";;
let y = V "y";;
let z = V "z";;
let b = V "b";;

let zero = C {node = ("0", 0); children = []};;
let one = C {node = ("1", 0); children = []};;
let two = C {node = ("2", 0); children = []};;
let plus_zero_one = C {node = ("+", 2); children = [zero; one] };;
let times_one_x = C {node = ("*", 2); children = [one; x] };;
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };;
let plus_x_z = C {node = ("+", 2); children = [x; z] };;
let plus_timesonex_pluszeroy =
  C {node = ("+", 2);
      children = [times_one_x; plus_zero_y ] };;
let plus_timesonex_z =
  C {node = ("+", 2);
      children = [times_one_x; z ] };; 
let plus_timesonex_z_y =
  C {node = ("+", 2);
      children = [plus_timesonex_z; y ] };; 
let plus_y_plus_x_z = 
  C {node = ("+", 2); 
      children = [y; plus_x_z ] };;
let plus_two_plus_y_plus_x_z =
  C {node = ("+", 2);
      children = [two; plus_y_plus_x_z ] };;
let plus_plus_y_plus_x_z_two =
  C {node = ("+", 2);
      children = [plus_y_plus_x_z; two ] };;
let plus_plus_y_plus_x_z_zero =
  C {node = ("+", 2);
      children = [plus_y_plus_x_z; zero  ] };;
let plus4_two_x_y_z = 
  C {node = ("+4", 4);
      children = [two; x; y; z; ] };;
let plus4_x_y_z_two = 
  C {node = ("+4", 4);
      children = [x; y; z; two ] };;
let plus4_x_y_z_zero = 
  C {node = ("+4", 4);
      children = [x; y; z; zero ] };;
let plus4_b_x_y_z = 
  C {node = ("+4", 4);
      children = [b; x; y; z; ] };;
let plus4_x_y_z_b = 
  C {node = ("+4", 4);
      children = [x; y; z; b ] };;

let rec wftree s t = 
  let is_consistent_children r = match r with 
    | C {node = (_, i); children = lt} -> ((length lt) = i)
    | _ -> false
  in let rec is_valid_node rc sg = match rc, sg with 
        _, [] -> false
      | C {node = (st1, i); children=_}, (st2, j)::sg' -> if ((st1 = st2) && (i = j)) then true else is_valid_node rc sg'
      | _, _ -> false
  in match t with 
    V _ -> true
  | C {node = (label, arity); children= []} as re -> (is_consistent_children re) && (is_valid_node re s)
  | C {node = (label, arity); children= c} as re -> (is_consistent_children re) && (is_valid_node re s) && (fold_left (&&) true (map (wftree s) c))
;;

wftree sig1 zero;; 
wftree sig1 one;;
wftree sig1 times_one_x;;
wftree sig1 plus_timesonex_z;;
wftree sig1 plus_timesonex_pluszeroy;;
wftree sig1 plus_timesonex_z_y;; 

let sig2 = [("0", 0); ("1", 0); ("+", 3); ("*", 2); ("-", 1)];; 

let splus_timesonex_z =
  C {node = ("+", 3);
      children = [times_one_x; z; y ] };;

wftree sig1 splus_timesonex_z;;
wftree sig2 splus_timesonex_z;;

wftree sig2 zero;; 
wftree sig2 one;;
wftree sig2 times_one_x;;
wftree sig2 plus_timesonex_z;;
wftree sig2 plus_timesonex_pluszeroy;;
wftree sig2 plus_timesonex_z_y;; 


let rec ht t = match t with 
  | V _ -> 0
  | C r -> 
      let (s, n) = r.node
      in (if n = 0
          then 0 
          else 1 + (fold_left max 0 (map ht r.children)))
;;

ht zero;; 
ht one;;
ht times_one_x;;
ht plus_timesonex_z;;
ht plus_timesonex_pluszeroy;;
ht plus_timesonex_z_y;; 


let rec size t = match t with
  | V _ -> 1
  | C r -> (fold_left (+) 1 (map size r.children))
;;

size zero;; 
size one;;
size times_one_x;;
size plus_timesonex_z;;
size plus_timesonex_pluszeroy;;
size plus_timesonex_z_y;; 


let rec comb l acc = match l with
  | [] -> acc 
  | p::rest -> 
      let rec merge m n = match m with
        | [] -> n
        | c::rem -> 
            if not (exists ((=) c) n) 
            then merge rem (c::n) 
            else merge rem n
      in comb rest (merge p acc)
;; 

let rec vars t = match t with
  | V s -> [s]
  | C r -> comb (map vars r.children) []
;;

vars zero;; 
vars one;;
vars times_one_x;;
vars plus_timesonex_z;;
vars plus_timesonex_pluszeroy;;
vars plus_timesonex_z_y;; 


let rec mirror t = match t with 
  | V _ -> t
  | C r -> C {node = r.node; children = List.rev (List.map mirror r.children)}
;;

mirror times_one_x;;
mirror plus_timesonex_z;;
mirror plus_timesonex_pluszeroy;;
mirror plus_timesonex_z_y;;


type subst_pt = tree * tree;;
type substitution = subst_pt list;;

let sub1 = [(z, times_one_x)];;
let sub2 = [(y, plus_timesonex_z); (b, plus_timesonex_pluszeroy)];;
let sub3 = [(y, plus_timesonex_pluszeroy); (x, plus_timesonex_pluszeroy)];;
let nsub1 = [(x, one); (x, zero)];;
let nsub2 = [(y, x); (x, one); (y, one)];;


let check_subst s = 
  let rec check_subst_helper v acc = match v with 
    | [] -> true
    | (V st, t)::rest -> 
      if (List.exists (fun r -> r = st) acc)
      then false
      else 
        check_subst_helper rest (st :: acc)
    | _ -> false
  in check_subst_helper s []
;;

check_subst sub1;;
check_subst sub2;;
check_subst sub3;;
check_subst nsub1;;
check_subst nsub2;;

exception VariableNotFound of substitution * tree;;

let get_subst s v =
  let tup = List.find_opt (fun (var, tree) -> var = v) s in 
  match tup with 
    | Some (var, tree) -> tree
    | None -> raise (VariableNotFound (s, v))
;;

get_subst sub1 (z);;
get_subst sub2 (y);;
get_subst sub2 (b);;
get_subst sub3 (y);;
get_subst sub3 (x);;


let rec subst s t = match t with 
  | V _ -> if (List.exists (fun (x, y) -> x = t) s) then (get_subst s t) else t
  | C r -> C {node = r.node; children = (map (subst s) r.children)}
;;

subst sub1 plus_timesonex_z;;
subst sub1 plus_timesonex_z_y;;
subst sub2 plus_zero_y;;
subst sub2 plus_timesonex_z_y;;
subst sub3 plus_timesonex_z_y;;
subst sub1 one;;
subst sub2 one;;
subst sub3 plus_zero_one;;

let compose_subst s1 s2 =
  let rec compose_subst_helper sub2 acc = match sub2 with
    | [] -> acc
    | (var, t)::rest -> compose_subst_helper rest (if (List.exists (fun (x, y) -> x = var) s1) then acc else (var, t)::acc)
  and res = map (fun (x, y) -> (x, (subst s2 y))) s1
  in compose_subst_helper s2 res
;;

compose_subst sub1 sub2;;
compose_subst sub3 sub2;;
compose_subst sub2 sub3;;

(* 
z -> 1*x
then
y -> (1*x) + z
and 
b -> (1*x) + (0+y)
*)

(*
y -> 1*x + 0+y
and
x -> 1*x + 0+y
then
y -> 1*x + z
and
b -> 1*x + 0+y
---
y -> 1*x + 0+(1*x + z)
b -> 1*x + 0+y
x -> 1*x + 0+(1*x + z)
*)

(*
y -> 1*x + z
and
b -> 1*x + 0+y
then
y -> 1*x + 0+y
and
x -> 1*x + 0+y
---
y -> 1*(1*x + 0+y) + z
b -> 1*(1*x + 0+y) + 0+(1*x + 0+y)
x -> 1*x + 0+y
*)

exception NOT_UNIFIABLE of tree * tree;;

let rec mgu t1 t2 = match t1, t2 with
  | V x, V y -> if (x = y) then [] else [(t1, t2)]
  | V x, C r -> if (List.mem x (vars t2)) then raise (NOT_UNIFIABLE (t1, t2)) else [(t1, t2)]
  | C r, V x -> if (List.mem x (vars t1)) then raise (NOT_UNIFIABLE (t1, t2)) else [(t2, t1)]
  | C r1, C r2 -> 
    if (r1.node = r2.node) 
    then fold_left2 (fun sub x y -> compose_subst sub (mgu (subst sub x) (subst sub y))) [] r1.children r2.children
    else 
      raise (NOT_UNIFIABLE (t1, t2))
;; 

mgu x y;;
mgu y x;;
mgu zero x;;
mgu z plus_zero_y;;
mgu z plus_timesonex_pluszeroy;;
mgu plus_timesonex_z plus_timesonex_pluszeroy;;
mgu plus4_two_x_y_z plus4_x_y_z_two;;
mgu plus4_b_x_y_z plus4_x_y_z_b;;

(*
(* NON UNIFIABLE CASES *)
mgu plus_zero_y y;;
mgu y plus_zero_y;;
mgu plus_zero_one one;;
mgu plus_timesonex_z plus_timesonex_z_y;;
mgu plus4_two_x_y_z plus4_x_y_z_zero;;
*)