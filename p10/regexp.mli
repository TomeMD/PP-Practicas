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

val symbol_of_char  : char -> symbol
val symbol_of_range : char -> char -> symbol

val empty        : regexp
val empty_string : regexp
val single       : symbol -> regexp
val except       : symbol -> regexp
val any          : regexp
val concat       : regexp -> regexp -> regexp
val repeat       : regexp -> regexp
val alt          : regexp -> regexp -> regexp
val all          : regexp -> regexp -> regexp

