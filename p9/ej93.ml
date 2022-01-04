open Bintree;;

let rec strict bt = match bt with
	Empty -> raise (Invalid_argument "strict")
	| Node (r, i, d) -> 
		if (i = Empty)&&(d = Empty) then true
		else if ((i = Empty)&&(d != Empty))
			||((i != Empty)&&(d = Empty)) then false
		else (strict i)&&(strict d);;

(*funcion auxiliar para saber la altura de un nodo*)
let rec altura = function
	Empty -> 0
	|Node (_, i, d) -> 1 + max (altura i) (altura d);;

let rec perfect bt = match bt with
	Empty -> true
	| Node (r, i, d) -> 
		if (i = Empty)&&(d = Empty) then true
		else ((altura i) = (altura d))&&(perfect i)&&(perfect d);;

let rec complete bt = match bt with
	Empty -> true
	| Node (r, i, d) -> 
		if (i = Empty)&&(d = Empty) then true
		else let n = altura i- altura d in
		     ((n = 0)||(n = 1))&&(complete i)&&(complete d);;


