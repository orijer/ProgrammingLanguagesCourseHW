(* Tests for the lambda calculus parser and reducers *)

open Utils
open Parser
open Reducer

let recognize_church_bool term =
  match term with
  | Abstraction(t, Abstraction(f, Variable v)) when v = t ->
    " =/=>\ntru"  (* λt.λf.t *)
  | Abstraction(t, Abstraction(f, Variable v)) when v = f ->
    " =/=>\nfls"  (* λt.λf.f *)
  | _ -> 
    " =/=>\n" ^ format_term_conv term

    let rec evaluate ~verbose reduce t =
      let max_same_steps = 5 in  (* Maximum number of identical reductions before declaring infinite loop *)
      
      let rec evaluate_with_limit same_steps prev_term t =
        if same_steps >= max_same_steps then (
          if verbose then 
            print_string "\nInfinite reduction detected (same term repeated 5 times)\n\n";
          t
        ) else (
          if verbose then 
            (print_string (format_term t))
          else 
            ();
    
          match reduce t with
          | None -> 
              if verbose then (
                print_string (recognize_church_bool t);
                print_string "\n\n"
              ) else ();
              t
          | Some t' -> 
              if verbose then 
                (print_string " ==>\n")
              else 
                ();
              (* Check if the term changed *)
              let new_same_steps = 
                if format_term t' = format_term prev_term
                then same_steps + 1
                else 0
              in
              evaluate_with_limit new_same_steps t t'
        )
      in
      evaluate_with_limit 0 t t

(* Boolean logic examples *)
let s1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
"

(* Simple reduction examples *)
let s3 = "((\\x.(\\y.x)) (\\z.z))"  (* Simple abstraction *)
let s4 = "((\\x.x) ((\\y.y) z))"    (* Identity function *)
let s5 = "((\\x.x) (\\y.y))"        (* Identity applied to identity *)
let omega = "((\\x.(x x)) (\\x.(x x)))"  (* Omega - infinite reduction *)

let () =
  (* Test boolean logic *)
  printf "\nTesting boolean AND with (tru tru), (cbn):\n";
  ignore (evaluate true reduce_cbn (parse s1));
  
  printf "\nTesting boolean AND with (fls tru), (cbv):\n";
  ignore (evaluate true reduce_cbv (parse s2));

  (* Test simple reductions *)
  printf "\nTesting simple abstraction (cbv):\n";
  ignore (evaluate true reduce_cbv (parse s3));
  
  (* Test identity function applications *)
  printf "\nTesting identity function (cbv):\n";
  ignore (evaluate true reduce_cbv (parse s4));
  printf "\nTesting identity function (cbn):\n";
  ignore (evaluate true reduce_cbn (parse s4));

  (* Test identity composition *)
  printf "\nTesting identity composition: (cbv)\n";
  ignore (evaluate true reduce_cbv (parse s5));

  (* Test omega expression *)
  printf "\nTesting Omega expression (infinite reduction): (cbv)\n";
  ignore (evaluate true reduce_cbv (parse omega));
  
  (* Format tests *)
  printf "\nFormat Term Conv Tests:\n";
  printf "%s\n" (format_term_conv(parse("(\\t.(\\f.t))")));
  printf "%s\n" (format_term_conv(parse("((\\x.x) (\\y.y))")));
  printf "%s\n" (format_term_conv(parse(s1)))