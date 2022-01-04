open List;;

type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec breadth_first = function
	Gt (x, []) -> [x]
	| Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

(*Version terminal breadth_first*)

let breadth_first_t gt = let rec aux acum = function
	Gt (x, []) -> rev (x::acum)
	| Gt (x, (Gt (y, t2))::t1) -> aux (x::acum) (Gt (y, rev_append (rev t1) t2)) in
		aux [] gt;;

let rec tlist r n = 
	if n <= 0 then Gt (Random.int r, [])
	else Gt (Random.int r, [tlist r (n-1)]);;

let t = tlist 300 95500;;

(*

let time f x = 
	let t = Sys.time() in 
	   let _ = f x in
		Sys.time() -. t;;

Defino una funcion para generar un int g_tree de gran tamaño con facilidad.
 
let rec tlist r n = 
	if n <= 0 then Gt (Random.int r, [])
	else Gt (Random.int r, [tlist r (n-1)]);; 

let t = tlist 300 95500;;
val t : int g_tree =
  Gt (37, [...])

# let time = time breadth_first t;;
Stack overflow during evaluation (looping recursion?).

# let time_t = time breadth_first_t t;;
val time_t : float = 0.0120860000000000412

A partir de 95500 elementos (aproximadamente) breadth_first provoca un desbordamiento en la pila mientras que breadth_first_t funciona correctamente.

*)
