let split l1 = let aux f g = function [] -> ([], []) | h::t -> (List.map f l1, List.map g l1) in aux fst snd l1;;

let combine l1 l2 = let aux f = function [] -> (function [] -> [] | h2::t2 -> raise(Invalid_argument "combine")) | h1::t1 -> (function [] -> raise(Invalid_argument "combine") | h2::t2 -> List.map2 f l1 l2) in aux (function x -> function y -> (x, y)) l1 l2;;


let length l = let aux f i = function [] -> 0 | h::t -> List.fold_left f i l in aux (function x -> function y -> x + 1) 0 l;;

let append = function [] -> (function l2 -> l2) | l1 -> (function [] -> l1 | l2 -> List.fold_right (function x -> function y -> x::y) l1 l2);;

let rev = function [] -> [] | l -> List.fold_left (function x -> function y -> y::x) [] l;;


(*concat con fold_left*)
let concat = function [] -> [] | l -> List.fold_left (function x -> function y -> x @ y) [] l;;

(*concat con fold_right*)
let concat = function [] -> [] | l -> List.fold_right (function x -> function y -> x @ y) l [];;

(*La complejidad es la misma independientemente de si usamos fold_right o fold_left, las 
  dos funciones realizan n uniones siendo n el numero de elementos de la lista de listas.
  La unica diferencia es que fold_left comenzara creando la lista concatenada por el primer 
  elemento de la lista de listas y fold_right por el ultimo*)


let partition = function f -> function [] -> ([], []) | l -> (List.filter f l, List.filter (function x -> not (f x)) l);;

let remove_all = function a -> function [] -> [] | l -> List.filter (function x -> if x = a then false else true) l;; 

let ldif = function [] -> (function l2 -> []) | l1 -> (function [] -> l1 | l2 -> List.fold_right remove_all l2 l1);;

let lprod l1 l2 = List.concat (List.map (function h -> List.map (function x -> (h,x)) l2) l1);;

let comp = function f -> function x -> function y -> f (x y);;

let multicomp = function [] -> raise(Invalid_argument "multicomp") | h::t -> List.fold_left comp h t;;















