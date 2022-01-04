type 'a bin_tree = 
	Empty 
	| Node of 'a * 'a bin_tree * 'a bin_tree;;

let breadth_first bt = 
	let rec aux cola acum = 
		if cola = [] then List.rev acum
		else match List.hd cola with
		Empty -> aux (List.tl cola) acum
		| Node (r, i, d) -> aux ((List.tl cola)@[i; d]) (r::acum)
			in aux [bt] [];;

