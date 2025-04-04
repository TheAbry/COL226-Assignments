type trie = 
          | Ce of {chr: char; node: trie} 
          | Cn of {node: tree; children: trie list}
;;

let empty = Cn {node = Empty; children = []};;
let edge c = Ce {chr = c; node = empty};;

let rec insert_trie t k v = match t, k with
  | Ce r, _ -> Ce {chr = r.chr; node = (insert_trie r.node k v)}
  | Cn r, [] -> Cn {node = v; children = r.children}
  | Cn r, c::rest ->
      let i = match List.find_index (fun (Ce e) -> e.chr = c) r.children with 
        | Some index -> index 
        | None -> -1
      in  
      if (i = -1)
      then 
        let new_child = (insert_trie (edge c) rest v) 
        in Cn {node = r.node; children = new_child :: r.children}
      else 
        let new_child = insert_trie (List.nth r.children i) rest v
        and other_children = filter (fun (Ce e) -> e.chr <> c) r.children
        in Cn {node = r.node; children = new_child :: other_children}
;;
 
let rec in_trie t v = match t, v with
  | Ce r, _ -> in_trie r.node v
  | Cn r, [] -> r.node <> Empty
  | Cn r, c::rest -> 
      let i = match List.find_index (fun (Ce e) -> e.chr = c) r.children with 
        | Some index -> index 
        | None -> -1
      in  
      if (i = -1)
      then false
      else in_trie (List.nth r.children i) rest
;;

let rec list_trie s t = match t with 
  | Ce r -> list_trie (s @ r.chr) r.node
  | Cn r -> 
    let inc_trie = (fold_right (::) [] (map (list_trie s) r.children)) 
    in 
      if r.node <> Empty 
      then (s, r.node) :: inc_trie
      else inc_trie 
;;
 
exception ElementNotFound of trie * char list;;
 
let rec get_trie t v = match t, v with
  | Ce r, _ -> get_trie r.node v
  | Cn r, [] -> 
    if (r.node = Empty)
    then raise (ElementNotFound (t, v))
    else r.node
  | Cn r, c::rest -> 
      let i = match List.find_index (fun (Ce e) -> e.chr = c) r.children with 
        | Some index -> index 
        | None -> -1
      in 
        if (i = -1)
        then raise (ElementNotFound (t, v))
        else get_trie (List.nth r.children i) rest
;;
 
let stoc (s : string) : char list =
  s |> String.to_seq |> List.of_seq

let sub1 = [(z, times_one_x)];;
let sub2 = [(y, plus_timesonex_z); (b, plus_timesonex_pluszeroy)];;
let sub3 = [(y, plus_timesonex_pluszeroy); (x, plus_timesonex_pluszeroy)];;
let nsub1 = [(x, one); (x, zero)];;
let nsub2 = [(y, x); (x, one); (y, one)];;
  
let check_subst s = 
  let rec check_subst_helper v acc = match v with 
    | [] -> true
    | c :: rest -> 
      if (List.exists (fun r -> r = c.chr) acc)
      then false
      else check_subst_helper rest (c.chr :: acc)
  in match s with 
    | Ce r -> check_subst r.node
    | Cn r -> fold_left (and) (check_subst_helper r.children []) (map check_subst r.children)
;;

check_subst sub1;;
check_subst sub2;;
check_subst sub3;;
check_subst nsub1;;
check_subst nsub2;;


let get_subst s v = match v with 
  V x -> get_trie s (stoc x)
;;

get_subst sub1 (z);;
get_subst sub2 (y);;
get_subst sub2 (b);;
get_subst sub3 (y);;
get_subst sub3 (x);;


let rec subst s t = match t with 
  | V x -> if (in_trie s (stoc x)) then (get_subst s t) else t
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
  let rec compose_subst_helper acc st sub2 = match sub2 with
    | Ce r -> compose_subst_helper acc (r.chr::st) r.node
    | Cn r -> 
      if (r.node <> Empty) 
      then 
        if (in_trie acc st)
        then Cn {node = r.node; children = List.map (compose_subst_helper acc st) r.children}
        else (insert_trie acc st r.node) 
  and let res = map (fun (x, y) -> (x, (subst s2 y))) s1
  in compose_subst_helper s2 res
;;

let rec fold_left f acc lst =
  match lst with
  | [] -> acc
  | hd::tl -> fold_left f (f acc hd) tl

let compose_subst s1 s2 = 
  

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

let rec occurs v t1 = match t1 with
  | V x -> V x = v
  | C r -> fold_left (||) false (map (occurs v) r.children)
;;

occurs x one;;
occurs x times_one_x;;
occurs y plus_timesonex_z;;
occurs y plus_timesonex_pluszeroy;;

exception NOT_UNIFIABLE of tree * tree;;

let rec mgu t1 t2 = match t1, t2 with
  | V x, V y -> if (x = y) then empty else insert_trie empty (stoc x) t2
  | V x, C r -> if (occurs t1 t2) then raise (NOT_UNIFIABLE (t1, t2)) else insert_trie empty (stoc x) t2
  | C r, V x -> if (occurs t2 t1) then raise (NOT_UNIFIABLE (t1, t2)) else insert_trie empty (stoc x) t1
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
