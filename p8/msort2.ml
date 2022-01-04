open List;;

(*Funciones auxiliares para la medicion de tiempos de ejecucion*)

(**************************************************************************************************)
let time ord f x = 
	let t = Sys.time() in 
	   let _ = ord f x in
		Sys.time() -. t;;

let rlist r l = 
	let rec aux acum = function
		0 -> acum
		|n -> aux ((Random.int r)::acum) (n-1)
	in aux [] l;;

let desc n = 
	let rec ord n l c = 
	    if (n == 0) then l
	    else ord (n-1) (c::l) (c+1)
	in ord n [] 1;;
let asc n = 
	let rec ord n l c = 
	    if (n == 0) then l
	    else ord (n-1) (c::l) (c-1)
	in ord n [] n;;
(**************************************************************************************************)


let rec split = function
	[] -> [], []
	| h::[] -> [h],[]
	| h1::h2::t -> let t1,t2 = split t in (h1::t1, h2::t2);;

let rec merge ord l1 l2 = match l1,l2 with
	[],l | l,[] -> l
	| h1::t1, h2::t2 -> if ord h1 h2 then h1::merge ord t1 l2
	else h2::merge ord l1 t2;;

let rec msort1 ord l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = split l in
		merge ord (msort1 ord l1) (msort1 ord l2);;

let l2 = rlist 300 88000;;

(*
¿Puede provocar algun problema la no terminalidad de split o merge?

Si, para tamaños mayores de 88000 elementos (aproximadamente) hay un desbordamiento en la pila.

# let l2 = rlist 300 88000;;
val l2 : int list = [...]

# time msort1 (<) l2;;       
Stack overflow during evaluation (looping recursion?).
*)

let split_t l = let rec aux acum1 acum2 = function
	[] -> rev acum1, rev acum2
	| h::[] -> aux (h::acum1) acum2 []
	| h1::h2::t -> aux (h1::acum1) (h2::acum2) t in
		aux [] [] l;;


let merge_t ord l1 l2 = 
	let rec aux acum l3 l4 = match l3,l4 with
		[],l | l,[] -> rev_append acum l
		| h3::t3, h4::t4 -> if ord h3 h4 then  aux (h3::acum) t3 l4
		else aux (h4::acum) l3 t4 in
			aux [] l1 l2;;

let rec msort2 ord l = match l with
	[] | _::[] -> l
	| _ -> let l1, l2 = split_t l in
		merge_t ord (msort2 ord l1) (msort2 ord l2);;

(*
# let aleatorio  = rlist 300 50000;;     
val aleatorio : int list = [...]

# let time1 = time msort1 (<) aleatorio;;
val time1 : float = 0.0705700000000000216

# let time2 = time msort2 (<) aleatorio;;
val time2 : float = 0.0941819999999999879

# let time3 = time qsort3 (<) aleatorio;;
val time3 : float = 0.271041000000000309


msort2 es un 33.6% (0.0942/0.0705) mas lento que msort1 y un 287.6% (0.271/0.0942) mas rapido que qsort3.
*)

