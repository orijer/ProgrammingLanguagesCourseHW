open Ast;;
open Semantics;;

[@@@ocaml.warning "-8"];;

(* Natural Operational Semantics of the While language *)
let rec nos c =
    let (command, state) = c in
    match command with
    	| Ass (variable, expression) -> update variable expression state
  	    | Skip -> state
  		| Comp (com1, com2) -> nos (com2, nos (com1, state))
  		| If (condition, com1, com2) -> if (solve_b condition state) = "tt" then nos (com1, state) else nos (com2, state)
  		| While (condition, com) -> if (solve_b condition state) = "tt" then nos(While (condition, com), nos (com, state)) else state
  		| Repeat (com, condition) -> let comState = nos (com, state) in 
									 if (solve_b condition comState) = "tt" then comState else nos (Repeat(com, condition), comState);;

(* tests *) 
print_string "x = ";;
print_int (let new_state = nos (Ast.test1, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test2, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test3, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "y");;
print_endline "";;

print_string "a = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "a");;
print_endline "";;

print_string "b = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "b");;
print_endline "";;

print_string "c = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "c");;
print_endline "";;