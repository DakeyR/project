(* Append 2 lists  ==> @*)

let rec append l1 l2 = 
	match l1 with
		|[] -> l2
		|h::t -> h:: append t l2;;


(* reversing a list *)
let rec reverse l =
	let rec aux ac l = match l with
		|[] -> ac
		|h::t -> aux (h::ac) t
	in
	aux [] l;;


(* set an int to the power of n *)

let rec power x = function
	|0 -> 1
	|n -> x * power x (n-1);;
 

(* convert_from base 10 *)

let rec ten_to_base n b =
	match (n mod b) with
		|0 when n/b = 0 -> [] (*end of conversion *)
		|r -> append(ten_to_base (n/b) b) [r];;


(* convert into base 10 *)

let base_to_ten l b =
	let rec aux n = function 
		|[] -> 0
		|h::t -> h * (power b n) + (aux (n+1) t)  
	in 
	aux 0 (reverse l) ;;

