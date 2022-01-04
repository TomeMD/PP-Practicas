(*4. Ejercicio opcional. *)

let e1 = (function x -> x *. (x +. 1.)) (2. *. asin 1.);;
(*val e1 : float = 13.0111970546791511*)

let e2 = (function x -> log x/. log 2.) (float(1024 * 1024));;
(*val e2 : float = 20.*)

let e3 = function r -> 4. *. asin 1. *. r;;
(*val e3 : float -> float = <fun>*)

let e4 = function r -> 2. *. asin 1. *. r *. r;;
(*val e4 : float -> float = <fun>*)

