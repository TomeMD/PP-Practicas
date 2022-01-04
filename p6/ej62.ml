let f = function x -> x;;

let g = function x -> (x, x);;

let h = function x, y -> x;; (*let h = fst*)

let i = function x, y -> y;; (*let i = snd*)

let j = function x -> [x; x; x];;

(*
En las primeras 4 funciones solo podremos definir una funciones sin operadores o con operadores polimorficos, ya que en el momento que usas un operador que tiene un tipo concreto como parametro de entrada OcamL reconoce que lo que pases como parametro a ese operador es de dicho tipo. Sin embargo, en la ultima funcion si que tendremos infinitas variantes ya que las listas en OcamL son polimorficas y podemos realizar operaciones sobre ellas dentro de una funcion y que esta siga siendo polimorfica.
*)

let k = function x -> raise (Failure "x");;

let l = function x -> [raise (Failure "x")];;

(*
Solo podremos escribir funciones con funciones que lancen excepciones ya que otras funciones haran que los tipos dejen de ser polimorficos (p.e. char_of_int, int_of_char, floor...). La funciones con tipo a -> b, como hemos mencionado previamente, tienen en comun que unicamente lanzaran excepciones.
*)

(*
No, no es posible debido a que el compilador leera en primer lugar f [1; 2; 3] y asignara f a una funcion de tipo (int list -> 'a), a continuacion leera f ['a', 'b'] y dara un error de compilacion porque ['a', 'b'] no es de tipo int list sino char list.

# let fun_123_ab f = (f [1; 2; 3]) + (f ['a'; 'b']);;            
Error: This expression has type char but an expression was expected of type int
*)
