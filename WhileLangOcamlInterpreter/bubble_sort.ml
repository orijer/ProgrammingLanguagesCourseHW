open Ast
open Nos
open Semantics

(* A script that creates a bubble sort entirely in the While language *)

let rec create_variables_list n =
	if n = 0 then []
	else create_variables_list (n - 1) @ ["" ^ string_of_int n];;

let rec create_variables variables_list = match variables_list with
	| [] -> update "temp" (Num 0) (default_state)
	| h :: t -> update h (Num 1) (create_variables t);;

let rec initial_state state variables_list values_list = match variables_list with
	| [] -> state
	| h :: t -> match values_list with
		| [] -> state
	  	| v_h :: v_t -> update h (Num v_h) (initial_state state t v_t);;

let rec print_variables state variables_list = match variables_list with
	| [] -> ()
	| h :: t -> 
    	print_int (state h);
		print_endline "";
		print_variables state t;;

let run_bubble_sort n =
	let rec create_compares i n = match i with
		| v when v >= n -> Ass("r", Add(Var "r", Num 1))
		| v -> Comp(If(Gte(Var ("" ^ string_of_int v), Var ("" ^ string_of_int (v+1))), 
						Comp(Ass("temp", Var ("" ^ string_of_int v)), 
						Comp(Ass("" ^ string_of_int v, Var ("" ^ string_of_int (v+1))), 
						Ass("" ^ string_of_int (v+1), Var "temp"))), Skip), 
			create_compares (i+1) n)
	in (* The actual sort code: *)
		Comp(Ass("r", Num 0), 
		Comp(Ass("stop", Mult (Var "n", Var "n")), 
		While(Neg (Aeq (Var "r", Var "stop")), (create_compares 1 n))))

let () = 
	let n = 7 in  (* Declare your n first *)
	let variables_list = create_variables_list n in
	let initialized_state = update "n" (Num n) (initial_state (create_variables variables_list) variables_list [3; 5; 1; -2; 100; 0; 2]) in (* Change the assignments as needed *)

	(* Print the initialized variables *)
	print_endline "";
	print_string "Before Sort:";
	print_endline "";
	print_variables initialized_state variables_list;
	print_endline "";

	(* Call the run_bubble_sort function with your n, and run the resulting statement*)
	let sort_code = run_bubble_sort n in
	let new_state = nos (sort_code, initialized_state) in

	(* Print the ordered values *)
	print_string "After Sort:";
	print_endline "";
	print_variables new_state variables_list
