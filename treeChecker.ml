(* This is an OCaml editor.
Enter your program here and send it to the toplevel using the "Eval code"
    button or [Ctrl-e]. *)
open List;;

type symbol = string * int;;
type signature = symbol list;;

type tree = 
  | V of string 
  | C of {node: symbol; children: tree list};; 

let x = V "x";;
let y = V "y";;
let z = V "z";;

let sig1 = [("0", 0); ("1", 0); ("+", 2); ("*", 2); ("-", 1)];;
let nsig1 = [("0", 0); ("0", 1); ("+", 2); ("*", 2); ("-", 1)];;
let nsig2 = [("0", 0); ("1", -1); ("+", 2); ("*", 2); ("-", 1)];;

let rec check_sig s = 
  let check_sig_helper sy acc = match sy with 
    | [] -> true
    | (sym, arity)::rest -> 
      if ((List.exists sym acc)|| (arity < 0)) 
      then false 
      else check_sig rest (sym :: acc)
    ;;
  in check_sig_helper s []
;;  

check_sig sig1;;
check_sig nsig1;;
check_sig nsig2;;

let zero = C {node = ("0", 0); children = []};;
let one = C {node = ("1", 0); children = []};;
let plus_zero_one = C {node = ("+", 2); children = [zero; one] };;
let times_one_x = C {node = ("*", 2); children = [one; x] };;
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };;
let plus_timesonex_pluszeroy =
  C {node = ("+", 2);
     children = [times_one_x; plus_zero_y ] };;
let plus_timesonex_z =
  C {node = ("+", 2);
     children = [times_one_x; z ] };; 
let plus_timesonex_z_y =
  C {node = ("+", 2);
     children = [plus_timesonex_z; y ] };; 

let rec ht t = match t with 
  | V _ -> 0
  | C r -> 
      let (s, n) = r.node
      in (if n = 0
          then 0 
          else 1 + (fold_left max 0 (map ht r.children))
         )
;;

let rec size t = match t with
  | V _ -> 1
  | C r -> (fold_left (+) 1 (map size r.children))
;;

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

let rec wftree s t = 
  let is_consistent_children r = match r with 
      C {node = (_, i); children = lt} -> ((length lt) = i)
  in let rec is_valid_node rc sg = match rc, sg with 
        _, [] -> false
      | C {node= (st1, i); children=_}, (st2, j)::sg' -> if ((st1 = st2) && (i = j)) then true else is_valid_node rc sg'
  in match t with 
    V _ -> true
  | C {node= (label, arity); children= []} as re -> (is_consistent_children re) && (is_valid_node re s)
  | C {node= (label, arity); children= c} as re -> (is_consistent_children re) && (is_valid_node re s) && (fold_left (&&) true (map (is_tree_valid s) c))
;;

wftree sig1 zero;; 
wftree sig1 one;;
wftree sig1 times_one_x;;
wftree sig1 plus_timesonex_z;;
wftree sig1 plus_timesonex_pluszeroy;;
wftree sig1 plus_timesonex_z_y;; 

vars plus_timesonex_z;;
vars plus_timesonex_pluszeroy;;


let sig2 = [("0", 0); ("1", 0); ("+", 3); ("+", 2); ("*", 2); ("-", 1)];; 

let splus_timesonex_z =
  C {node = ("+", 3);
     children = [times_one_x; z; y ] };;

is_tree_valid sig1 splus_timesonex_z;;
is_tree_valid sig2 splus_timesonex_z;;

is_tree_valid sig2 zero;; 
is_tree_valid sig2 one;;
is_tree_valid sig2 times_one_x;;
is_tree_valid sig2 plus_timesonex_z;;
is_tree_valid sig2 plus_timesonex_pluszeroy;;
is_tree_valid sig2 plus_timesonex_z_y;; 
