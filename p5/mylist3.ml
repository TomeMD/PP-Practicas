let rec remove a = function [] -> [] | h::t -> if h = a then t else h::(remove a t);;

let rec remove_all a = function [] -> [] | h::t -> if h = a then remove_all a t else h::(remove_all a t);;

let rec ldif = function [] -> (function l2 -> []) | l1 -> (function [] -> l1 | h2::t2 -> remove_all h2 (ldif l1 t2));;

let rec lprod l1 l2 = match l1 with [] -> [] | h::t -> (List.map(function x -> (h,x)) l2) @ (lprod t l2);;

let divide lst = let rec aux l1 l2 i = function [] -> (List.rev l1, List.rev l2) | h::t -> if (i mod 2) = 0 then aux l1 (h::l2) (i+1) t else aux (h::l1) l2 (i+1) t in aux [] [] 1 lst;; 
	



