type symbol = Symbol of char | Range of char * char;; 
type regexp = 	Empty 
		| EmptyString
		| Single of symbol
		| Except of symbol
		| Any
		| Concat of regexp * regexp
		| Repeat of regexp
		| Or of regexp * regexp 
		| And of regexp * regexp;;

let symbol_of_char a = Symbol a;;

let symbol_of_range a b = Range (a, b);;

let empty = Empty;;

let empty_string = EmptyString;;

let single s = Single s;;
	
let except s = Except s;;

let any = Any;;

let concat r1 r2 = Concat (r1, r2);;

let repeat r = Repeat r;;

let alt r1 r2 = Or (r1, r2);;

let all r1 r2 = And (r1, r2);;




