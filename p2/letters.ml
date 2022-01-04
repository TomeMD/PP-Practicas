(*Función lowercase*)

let lowercase = function x -> char_of_int (int_of_char x + 32);;
(*val lower : char -> char = <fun>*)

(*Función uppercase*)

let uppercase = function x -> char_of_int (int_of_char x - 32);;
(*val upper : char -> char = <fun>*)

