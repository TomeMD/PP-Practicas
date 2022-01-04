open Bintree;;
open Gtree;;

let cod_as_bin gt = let rec aux t hm = match t, hm with
	Gt(r, []),[] -> Node(r, Empty, Empty)
	| Gt(r, h::t),[] -> Node(r, (aux h t), Empty)
	| Gt(r, []),h::t -> Node(r, Empty, aux h t)
	| Gt(r, h1::t1),h2::t2 -> Node(r, aux h1 t1, aux h2 t2)
		in aux gt [];;

let decod_from_bin bt = let rec aux t = match t with
	Empty -> raise (Invalid_argument "decod_from_bin")
	| Node(r, Empty, Empty) -> [Gt (r, [])]
	| Node(r, i, Empty) -> [Gt (r, aux i)]
	| Node(r, Empty, d) -> [Gt (r, [])]@(aux d)
	| Node(r, i, d) -> [Gt (r, aux i)]@(aux d)
		in List.hd (aux bt);;
