(* Task : Associative_array/Merging *)

(* 
    Note that, given OCaml's strong typing,
    we must declare a type here for the 3-type data.
    In general, would need the specific data type for the task,
    or use a PPX rewriter (effectively a compiler middleware)
    that can rewrite code based on dynamic type extensions.
 *)

(*** Helpers ***)

type ty = 
    | TFloat of float
    | TInt of int
    | TString of string

type key = string
type assoc = string * ty

let string_of_ty : ty -> string = function
    | TFloat x -> string_of_float x
    | TInt i -> string_of_int i
    | TString s -> s

let print_pair key el =
    Printf.printf "%s: %s\n" key (string_of_ty el)
;;

(*** Associative list ***)

let l1 : assoc list = [
    ("name", TString "Rocket Skates");
    ("price", TFloat 12.75);
    ("color", TString "yellow")
] ;;

let l2 : assoc list = [
    ("price", TFloat 15.25);
    ("color", TString "red");
    ("year",  TInt 1974)
] ;;

let rec merge_assoc_list (base_list : assoc list) (add_list : assoc list) : assoc list = 
    List.fold_left
        (fun l (key, val_) -> 
            (key, val_) :: (List.remove_assoc key l)
        )
        base_list
        add_list
;;

let l' = merge_assoc_list l1 l2 ;;

(*** Map functor based ("binary tree"), since 4.03 for union function ***)

module StringMap = Map.Make(String) ;;

let print_map = StringMap.iter print_pair ;;

let map_merge (base : ty StringMap.t) (add : ty StringMap.t) : ty StringMap.t = 
    StringMap.union (fun key v1 v2 -> Some v2) base add
;;

let m1 = StringMap.(
    empty 
    |> add "name" (TString "Rocket Skates")
    |> add "price" (TFloat 12.75)
    |> add "color" (TString "yellow")
) ;;

let m2 = StringMap.(
    empty 
    |> add "price" (TFloat 15.25)
    |> add "color" (TString "red")
    |> add "year" (TInt 1974)
) ;;

let m' = map_merge m1 m2 ;;

print_map m' ;;

(*** Hash table ***)

(* Updates the base table with the bindings from add *)
let hash_merge (base : (string, ty) Hashtbl.t) (add : (string, ty) Hashtbl.t) : unit = 
    Hashtbl.iter (Hashtbl.replace base) add

let print_hashtbl t =
    Hashtbl.iter print_pair t

let h1 : (string, ty) Hashtbl.t = Hashtbl.create 10 ;;
Hashtbl.add h1 "name" (TString "Rocket Skates") ;;
Hashtbl.add h1 "price" (TFloat 12.75) ;;
Hashtbl.add h1 "color" (TString "yellow") ;;

let h2 : (string, ty) Hashtbl.t = Hashtbl.create 10 ;;
Hashtbl.add h2 "price" (TFloat 15.25) ;;
Hashtbl.add h2 "color" (TString "red") ;;
Hashtbl.add h2 "year" (TInt 1974) ;;

hash_merge h1 h2 ;;

print_hashtbl h1 ;;