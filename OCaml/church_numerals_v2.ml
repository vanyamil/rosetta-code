(*  Church Numerals task for OCaml 

    Church Numerals are numbers represented as functions. 
    A numeral corresponding to a number n is a function that receives 2 arguments
    - A function f 
    - An input x of some type
    and outputs the function f applied n times to x: f(f(...(f(x))))
*)

(*  Using type as suggested in https://stackoverflow.com/questions/43426709/does-ocamls-type-system-prevent-it-from-modeling-church-numerals 
    This is an explicitely polymorphic type : it says that f must be of type ('a -> 'a) -> 'a -> 'a for any possible a "at same time".
*)
type church_num = {f : 'a. ('a -> 'a) -> 'a -> 'a } ;;

(* Zero means apply f 0 times to x, aka return x *)
let ch_zero : church_num = 
    let f = fun f x -> x
    in {f}

(* The next numeral of a church numeral would apply f one more time *)
let ch_succ (n : church_num) : church_num =
    let f = fun f x -> f (n.f f x)
    in {f}

(* This is just a different way to represent natural numbers - so we can still add/mul/exp them *)

(* Adding m and n is applying f m times and then also n times *)
let ch_add  (m : church_num) (n : church_num) : church_num = 
    let f = fun f x -> n.f f (m.f f x)
    in {f}

(* Multiplying is repeated addition : add n, m times *)
let ch_mul  (m : church_num) (n : church_num) : church_num = 
    let f = fun f x -> m.f (n.f f) x
    in {f}

(*  Exp is repeated multiplication : multiply by base, exp times.
    However, Church numeral n is in some sense the n'th power of a function f applied to x
    So exp base = apply function base to the exp'th power = base^exp.
    Some shenanigans to typecheck though.
 *)
let ch_exp (base : church_num) (exp : church_num) : church_num = 
    let custom_f f x = (exp.f base.f) f x
    in {f = custom_f}

(* Convert a number to a church_num via recursion *)
let church_of_int (n : int) : church_num = 
    if n < 0 
    then raise (Invalid_argument (string_of_int n ^ " is not a natural number"))
    else 
    (* Tail-recursed helper *)
    let rec helper n acc = 
        if n = 0 
        then acc
        else helper (n-1) (ch_succ acc)
    in
    helper n ch_zero

(*  Convert a church_num to an int is rather easy! Just +1 n times. *)
let int_of_church (n : church_num) : int = 
    n.f succ 0
;;

(* Now the tasks at hand: *)

(* Derive Church numerals three and four in terms of Church zero and a Church successor function *)

let ch_three = ch_zero |> ch_succ |> ch_succ |> ch_succ
let ch_four = ch_three |> ch_succ

(* Use Church numeral arithmetic to obtain the the sum and the product of Church 3 and Church 4 *)
let ch_7 = ch_add ch_three ch_four
let ch_12 = ch_mul ch_three ch_four

(* Similarly obtain 4^3 and 3^4 in terms of Church numerals, using a Church numeral exponentiation function *)
let ch_64 = ch_exp ch_four ch_three
let ch_81 = ch_exp ch_three ch_four
;;

(* Convert each result back to an integer, and return it or print it to the console *)
List.map 
    int_of_church 
    [ch_three; ch_four; ch_7; ch_12; ch_64; ch_81] 
;;