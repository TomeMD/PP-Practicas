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


let rec qsort1 ord = function
	[] -> []
	| h::t -> let after, before = partition (ord h) t in
		qsort1 ord before @ h :: qsort1 ord after;;

(*
¿En que casos no sera bueno el rendimiento de esta implementacion?

# let aleatoria = rlist 300 10000;;
val aleatoria : int list = [...]    

# let ordenada = asc 10000;;
val ordenada : int list = [1..10000]

# let desordenada = desc 10000;;
val desordenada : int list = [10000..1]

# let tiempo = time qsort1 (<) aleatoria;;  
val tiempo : float = 0.0266079999999995209

# let tiempo = time qsort1 (<) ordenada;;
val tiempo : float = 5.107513

# let tiempo = time qsort1 (<) desordenada;;
val tiempo : float = 6.351585

Ordenacion               Tiempos de ejecucion

Aleatoria    ---> 	     0.0266079999999995209s
Ordenada     --->	     5.107513s
Desordenada  --->	     6.351585s

Esta implementacion no tendra un buen rendimiento en los casos en los que la lista esté completamente desordenada o completamente
ordenada, el rendimiento bajara muchisimo. En caso de que la lista este ordenada qsort1 es (5.107513/0.026608 = 19000% aprox.) 190 veces mas lenta que cuando esta en orden aleatorio y cuando esta desordenada (6.3515850/0.026608 = 24000% aprox.) 240 veces mas lenta que cuando esta en orden aleatorio.

*)

let rec qsort2 ord = function
	[] -> []
	| h::t -> let after, before = partition (ord h) t in
		rev_append (rev (qsort2 ord before)) (h :: qsort2 ord after);;

let l1 = rlist 300 300000;;

(*
¿Tiene qsort2 alguna ventaja sobre qsort1?

Si, qsort2 utiliza recursividad terminal, lo cual evita posibles debordamientos en la pila con ordenaciones de gran tamaño.


¿Permite qsort2 ordenar listas que no podrian ordenarse con qsort1?

Si, a partir de listas con 300000 elementos o mas (aproximadamente), qsort1 falla por un desbordamiento en la pila debido a su no terminalidad mientras que qsort2 no.

(n = 300000)
# let l1 = rlist 300 300000;;       
val l1 : int list = [...]    

# time qsort1 (<) l1;;
Stack overflow during evaluation (looping recursion?).

# time qsort2 (<) l1;;
- : float = 17.0037869999999884

¿Tiene qsort2 alguna desventaja sobre qsort1?

Si, qsort2 es mas lento que qsort1.

# let aleatorio = rlist 300 150000;;
val aleatorio : int list = [...]

# let tiempo1 = time qsort1 (<) aleatorio;;
val tiempo1 : float = 3.77758699999999692

# let tiempo2 = time qsort2 (<) aleatorio;;
val tiempo2 : float = 4.18302899999999411

Esto se debe a la funcion rev_append y rev, en cada iteracion recursiva estamos haciendo un rev de un rev para dejar la lista en su estado original y dicha operacion nos hace perder un tiempo considerable. 

qsort2 es un (4.18/3.77 = 0.108) 10.8% más lento que qsort1.

*)

let qsort3 ord l =
	let rec sort_asc = function
		[] -> []
		| h::t -> let after, before = partition (ord h) t in
			rev_append (sort_des before) (h :: sort_asc after)
	and sort_des = function
		[] -> []
		| h::t -> let after, before = partition (ord h) t in
			rev_append (sort_asc after) (h :: sort_des before)
	in sort_asc l;;

(*
¿Puede apreciarse un cambio de rendimiento en tiempo de ejecucion de qsort3 respecto a
qsort2? Si es asi, estime en que porcentaje. ¿Y respecto a qsort1?

# let tiempo3 = time qsort3 (<) aleatorio;;
val tiempo3 : float = 2.89747299999999797

Si, se aprecia un cambio de rendimiento respecto a ambos. qsort3 es aproximadamente, un 44.6% (4.18/2.89) mas rapido que qsort2 y un 30.4% (3.77/2.89) mas rapido que qsort1.
*)
