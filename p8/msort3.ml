open List;;

let split_firsts n l = let n1 = (n+1) / 2 and n2 = n / 2 in
	let rec cut i l =
		if i > 0 then cut (i-1) (tl l)
		else l
	in (n1, l, n2, cut n1 l);;

let merge_t ord l1 l2 = 
	let rec aux acum l3 l4 = match l3,l4 with
		[],l | l,[] -> rev_append acum l
		| h3::t3, h4::t4 -> if ord h3 h4 then  aux (h3::acum) t3 l4
		else aux (h4::acum) l3 t4 in
			aux [] l1 l2;;

let rec msort3 ord l = match l with
	[] | _::[] -> l
	| _ -> let n1, l, n2, l1 = split_firsts (length l) l
	       and n1', l', n2', l2 = split_firsts (length l - 1) (rev l) in
		merge_t ord (msort3 ord l1) (msort3 ord l2);;

(*

# let aleatorio  = rlist 300 50000;;     
val aleatorio : int list = [...]

# let time2 = time msort2 (<) aleatorio;;
val time2 : float = 0.0941819999999999879

# let time3 = time msort3 (<) aleatorio;;
val time3 : float = 0.115645999999999916

msort3 no obtiene una mejora respecto a msort2, msort2 es un 22.7% (0.1156/0.0942) mas rapido que msort3.
*)
