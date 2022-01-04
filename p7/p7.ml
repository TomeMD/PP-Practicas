let suml l = let rec aux acum = function [] -> acum | h::t -> aux (acum + h) t in aux 0 l;;

let rec maxl = function
	[] -> raise (Failure "maxl") 
	| h::[] -> h
	| h1::h2::[] -> max h1 h2
	| h1::h2::t -> maxl ((max h1 h2)::t);;

let to0from n = let rec aux acum x = if x < 0 then List.rev acum else aux (x::acum) (x-1) in aux [] n;;

let fromto a b = let rec aux acum x = if x > b then List.rev acum else aux (x::acum) (x+1) in aux [] a;;

let from1to n = fromto 1 n;; 

let append l1 l2 = List.rev_append (List.rev l1) l2;;

let map f l = let rec aux acum = function f -> (function [] -> acum | h::t -> aux (append acum [(f h)]) f t) in aux [] f l;;

let power x y = if y < 0 then raise(Invalid_argument "power") 
		else let rec innerpower pow x y = 
			if y = 0 then pow 
			else innerpower (pow*x) x (y-1) in innerpower 1 x y;; 

(*fold_right ahora es recursivo terminal por tanto incseg tambien lo es*)
let rec fold_right f l x = match (List.rev l, x) with ([], x) -> x | (h::t, x) -> fold_right f (List.rev t) (f h x);;

let incseg l = let rec aux acum = function [] -> acum | h::t -> aux ((fold_right (+) (h::t) 0)::acum) t in aux [] (List.rev l);;

let remove x l = let rec aux acum = function x -> (function [] -> List.rev acum | h::t -> if x = h then List.rev_append acum t else aux (h::acum) x t) in aux [] x l;;

let insert x l = let rec aux acum = function x -> (function [] -> append acum [x] | h::t -> if x <= h then append (append acum [x]) (h::t) else aux (append acum [h]) x t) in aux [] x l;;

let insert x l = if l = [] then [x] else let rec aux acum = function x -> (function [] -> append acum [x] | h::t -> if x <= h then append (append acum [x]) (h::t) else aux (append acum [h]) x t) in aux [] x l;;

let insert_gen f x l = let rec aux acum = function f -> function x -> (function [] -> append acum [x] | h::t -> if (f x h) then append (append acum [x]) (h::t) else aux (append acum [h]) f x t) in aux [] f x l;;






