(* Reducers (interpreters) for lambda-calculus *)

open Utils
open Parser

exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))

(* Given a term, returns a new variable that isn't used in the term (or an error if not possible) *)
let fresh_var used_vars : string = 
	if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
	then raise (OutOfVariablesError)
	else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)

(* Returns a list of strings of all the free variables inside a term *)
let rec fv t = match t with
	| Variable x -> Utils.string_set_from_list [x]
	|	Abstraction (x, t2) -> Utils.StringSet.remove x (fv t2)
	| Application (t1, t2) -> Utils.StringSet.union (fv t1) (fv t2);;

(* Substitutes all appearances of a var inside a term with another term *)
let rec substitute var t1 t2 = match t2 with
	| Variable x -> if x = var then t1 else t2
	| Abstraction (x, t2_) -> if x = var then t2 
			else 
				if (Utils.StringSet.mem x (fv t1)) then 
					let z = fresh_var (StringSet.union (StringSet.union (fv t1) (fv t2)) (fv (Variable x))) in 
					substitute var t1 (Abstraction (z, substitute x (Variable z) t2_))
				else Abstraction (x, substitute var t1 t2_) 
	| Application (t21, t22) -> Application ((substitute var t1 t21), (substitute var t1 t22));;

(* Call By Value reduction *)
let rec reduce_cbv (t : term) : term option = match t with
	| Variable x -> None
	| Abstraction (x, tt) -> None
	| Application (t1, t2) -> match t1 with
		| Variable y -> None
		| Abstraction (x, tt1) -> (match t2 with
			| Variable y -> None
			| Abstraction (y, tt2) -> Some (substitute x t2 tt1)
			| Application (t21, t22) -> let simplify_t2 = (reduce_cbv t2) in match simplify_t2 with
			  | None -> None
				| Some new_t2 -> Some (Application (t1, new_t2)))
		| Application (t11, t12) -> (let simplify_t1 = (reduce_cbv t1) in match simplify_t1 with
			| None -> None
			| Some new_t1 -> Some (Application (new_t1, t2)));;

(* Call By Name reduction *)
let rec reduce_cbn (t : term) : term option = match t with
	| Variable x -> None
	| Abstraction (x, tt) -> None
	| Application (t1, t2) -> match t1 with
		| Variable x -> None
		| Abstraction (x, tt1) -> Some (substitute x t2 tt1)
		| Application (t11, t12) -> let simplify_t1 = (reduce_cbn t1) in match simplify_t1 with
		  | None -> None
			| Some new_t1 -> Some (Application (new_t1, t2));;