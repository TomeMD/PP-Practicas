(*Parte 1*)

let hd = function [] -> raise(Failure "hd") | h::_ -> h;;

let tl = function [] -> raise(Failure "tl") | _::t -> t;;

let rec length = function [] -> 0 | _::t -> 1 + length t;;

let rec compare_lengths = function [] -> (function [] -> 0 | h::t -> -1) | h1::t1 -> (function [] -> 1 | h2::t2 -> compare_lengths t1 t2);;

let rec nth = function [] -> raise(Failure "nth") | h::t -> (function 0 -> h | x -> nth t (x-1));;

let rec append = function [] -> (function l -> l) | h::t  -> function l -> h::(append t l);;


(*Parte 2*)

let init n f = let rec aux i n f = if i >= n then [] else (f i) :: (aux (i+1) n f) in aux 0 n f;;

let rev l = let rec aux acum = function [] -> acum | h::t -> aux (h::acum) t in aux [] l;;

let rec rev_append = function l1 -> function l2 -> append (rev l1) l2;;

let rec concat = function[] -> []| h::t -> append h (concat t);;

let flatten = concat;;

let rec map = function f -> function [] -> [] | h::t -> (f h)::(map f t);;

let rec rev_map = function f -> function l -> map f (rev l);; 

let rec map2 = function f -> function [] -> (function [] -> [] | l -> raise(Invalid_argument "map2")) | h1::t1 -> (function [] -> raise(Invalid_argument "map2") | h2::t2 -> (f h1 h2)::(map2 f t1 t2));;

let rec fold_left f x = function [] -> x | h::t -> fold_left f (f x h) t;;

let rec fold_right f l x = match (rev l, x) with ([], x) -> x | (h::t, x) -> fold_right f (rev t) (f h x);;


(*Parte 3*)

let rec find f = function [] -> raise(Not_found) | h::t -> if (f h) = true then h else find f t;;

let rec for_all f = function [] -> true | h::t -> if (f h) = true then for_all f t else false;;

let rec exists f = function [] -> false | h::t -> if (f h) = true then true else exists f t;;

let rec mem a = function [] -> false | h::t -> if a = h then true else mem a t;;

let rec filter f = function [] -> [] | h::t -> if (f h) = true then h::(filter f t) else filter f t;;

let find_all = filter;;

let partition f l1 = let rec aux l2 l3 = function [] -> (rev l2, rev l3) | h::t -> if (f h) = true then aux (h::l2) l3 t else aux l2 (h::l3) t in aux [] [] l1;;

let split l1 = let rec aux l2 l3 = function [] -> (rev l2, rev l3) | h::t -> aux (fst h::l2) (snd h::l3) t in aux [] [] l1;;

let combine l1 l2 = let rec aux l3 = function [] -> (function [] -> rev l3 | h2::t2 -> raise(Invalid_argument "combine")) | h1::t1 -> (function [] -> raise(Invalid_argument "combine") | h2::t2 -> aux ((h1, h2)::l3) t1 t2) in aux [] l1 l2;;

 



















