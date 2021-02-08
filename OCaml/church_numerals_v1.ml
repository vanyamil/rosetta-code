(* Church Numerals task for OCaml 

	Church Numerals are numbers represented as functions. 
	A numeral corresponding to a number n is a function that receives 2 arguments
	- A function f 
	- An input x of some type
	and outputs the function f applied n times to x: f(f(...(f(x))))
*)

type 'a church_num = ('a -> 'a) -> 'a -> 'a ;;

(* Zero means apply f 0 times to x, aka return x *)
let ch_zero : 'a church_num = 
	fun f x -> x

(* The next numeral of a church numeral would apply f one more time *)
let ch_succ (n : 'a church_num) : 'a church_num =
	fun f x -> f (n f x)

(* This is just a different way to represent natural numbers - so we can still add/mul/exp them *)

(* Adding m and n is applying f m times and then also n times *)
let ch_add  (m : 'a church_num) (n : 'a church_num) : 'a church_num = 
	fun f x -> n f (m f x)

(* Multiplying is repeated addition : add n, m times *)
let ch_mul  (m : 'a church_num) (n : 'a church_num) : 'a church_num = 
	fun f x -> m (n f) x

(* 	Exp is repeated multiplication : multiply by base, exp times, starting at 1.
	Due to the strict type system, the (nicer) definition "fun base exp -> exp base" does not work here.
	In fact, everything fails here. V1 ends here.
 *)
let ch_exp (base : 'a church_num) (exp : 'a church_num) : 'a church_num = 
	ch_zero

(* Convert a number to a church_num via recursion *)
let church_of_int (n : int) : 'a church_num = 
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

(*  Convert a church_num to an int is rather easy! Just +1 n times. 
	Note that despite the type of int_of_church, everything still works
	(because church numerals themselves don't have type constraints)

	Some languages can avoid this by using reference cells, but they still
	need to pass some default value; and we can't pass a default x that
	does not constrain the type of church_num.
*)
let int_of_church (n : int church_num) : int = 
	n succ 0
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

(* Convert each result back to an integer, and return it or print it to the console *)
List.map 
	int_of_church 
	[ch_three; ch_four; ch_7; ch_12; ch_64; ch_81] 
;;