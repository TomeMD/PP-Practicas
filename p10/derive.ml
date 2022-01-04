open Regexp;;

let regexp_of_string s = Regexp_parser.main Regexp_lex.token
	(Lexing.from_string s);;

let rec nullable = function 
	Empty -> Empty
	| EmptyString -> EmptyString
	| Single Symbol s -> Empty | Single Range (a, b) -> Empty
	| Except Symbol s -> Empty | Except Range (a, b) -> Empty
	| Any -> Empty
	| Concat (r1, r2) -> if (nullable r1 = EmptyString)&&(nullable r2 = EmptyString) then EmptyString else Empty
	| Repeat r -> EmptyString
	| Or (r1, r2) -> if (nullable r1 = EmptyString)||(nullable r2 = EmptyString) then EmptyString else Empty
	| And (r1, r2) -> if (nullable r1 = EmptyString)&&(nullable r2 = EmptyString) then EmptyString else Empty;;

let rec derive c = function 
	Empty -> Empty
	| EmptyString -> Empty
	| Single Symbol a -> if c = a then EmptyString else Empty 
	| Except Symbol a -> if c <> a then EmptyString else Empty
	| Single Range (a, b) -> if (c>=a)&&(c<=b) then EmptyString else Empty 
	| Except Range (a, b) -> if (c<a)||(c>b) then EmptyString else Empty
	| Any -> EmptyString
	| Concat (r1, r2) -> alt (concat (derive c r1) r2) (concat (nullable r1) (derive c r2))
	| Repeat r -> concat (derive c r) (repeat r)
	| Or (r1, r2) -> alt (derive c r1) (derive c r2)
	| And (r1, r2) -> all (derive c r1) (derive c r2);;

let rec simplify = function
	Concat (Empty, r) -> Empty | Concat (r, Empty) -> Empty
	| Concat (EmptyString, r) -> simplify r | Concat (r, EmptyString) -> simplify r
	| Repeat EmptyString -> EmptyString
	| Repeat Empty -> Empty
	| Or (Empty, r) -> simplify r | Or (r, Empty) -> simplify r
	| And (Empty, r) -> Empty | And (r, Empty) -> Empty
	| Empty -> Empty
	| EmptyString -> EmptyString
	| Single s -> Single s 
	| Except s -> Except s
	| Any -> Any
	| Concat (r1, r2) -> Concat (simplify r1, simplify r2)
	| Repeat r -> Repeat (simplify r) 
	| Or (r1, r2) -> Or (simplify r1, simplify r2)
	| And (r1, r2) -> And (simplify r1, simplify r2);;

let matches_regexp s r = let l = String.length s in
	let rec aux acum i = 
		if i = l then (nullable acum) = EmptyString
		else aux (simplify (derive s.[i] acum)) (i+1)
			in aux (simplify r) 0;;

let matches s1 s2 = matches_regexp s1 (regexp_of_string s2);;








