let rec hanoi (o, a, d) n = 
	if n < 1 then raise (Invalid_argument "Hanoi")
	else if n = 1 then [(o, d)]
	else (hanoi (o, d, a) (n-1))@((o, d)::(hanoi (a, o, d) (n-1)));;
