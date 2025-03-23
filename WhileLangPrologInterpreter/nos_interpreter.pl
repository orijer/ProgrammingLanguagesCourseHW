% Abstract Syntax Tree definitions
% Arithmetic expressions
aexp(num(N)) :- number(N).
aexp(var(X)) :- atom(X).
aexp(add(E1, E2)) :- aexp(E1), aexp(E2).
aexp(mult(E1, E2)) :- aexp(E1), aexp(E2).
aexp(sub(E1, E2)) :- aexp(E1), aexp(E2).
aexp(iand(E1, E2)) :- aexp(E1), aexp(E2).

% Boolean expressions
bexp(true).
bexp(false).
bexp(aeq(E1, E2)) :- aexp(E1), aexp(E2).
bexp(beq(E1, E2)) :- bexp(E1), bexp(E2).
bexp(gte(E1, E2)) :- aexp(E1), aexp(E2).
bexp(neg(E)) :- bexp(E).
bexp(and(E1, E2)) :- bexp(E1), bexp(E2).

% Statements
stm(skip).
stm(assign(X, E)) :- atom(X), aexp(E).
stm(comp(S1, S2)) :- stm(S1), stm(S2).
stm(if(E1, E2, E3)) :- bexp(E1), stm(E2), stm(E3).
stm(while(E1, E2)) :- bexp(E1), stm(E2).
stm(do_while(S, B)) :- stm(S), bexp(B).


% State operations
lookup([(X, V)|_], X, V).
lookup([(Y, _)|Rest], X, V) :- X \= Y, lookup(Rest, X, V).

update(X, V, [], [(X, V)]).
update(X, V, [(X, _)|Rest], [(X, V)|Rest]).
update(X, V, [(Y, W)|Rest], [(Y, W)|NewRest]) :-
    X \= Y,
    update(X, V, Rest, NewRest).

% Arithmetic expression evaluation
eval_aexp(num(N), _, N).
eval_aexp(var(X), State, V) :- lookup(State, X, V).
eval_aexp(add(E1, E2), State, Result) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), Result is V1 + V2.
eval_aexp(sub(E1, E2), State, Result) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), Result is V1 - V2.
eval_aexp(mult(E1, E2), State, Result) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), Result is V1 * V2.
eval_aexp(iand(E1, E2), State, Result) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), Result is V1 /\ V2.

% Boolean expression evaluation
eval_bexp(true, _, true).
eval_bexp(false, _, false).
eval_bexp(neg(E), State, true) :- eval_bexp(E, State, false).
eval_bexp(neg(E), State, false) :- eval_bexp(E, State, true).
eval_bexp(and(E1, E2), State, true) :- eval_bexp(E1, State, true), eval_bexp(E2, State, true).
eval_bexp(and(E1, _), State, false) :- eval_bexp(E1, State, false).
eval_bexp(and(_, E2), State, false) :- eval_bexp(E2, State, false).
eval_bexp(aeq(E1, E2), State, true) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), V1 is V2.
eval_bexp(aeq(E1, E2), State, false) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), V1 \= V2.
eval_bexp(gte(E1, E2), State, true) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), V1 >= V2.
eval_bexp(gte(E1, E2), State, false) :- eval_aexp(E1, State, V1), eval_aexp(E2, State, V2), V1 < V2.


% Natural semantics for statements
nos(skip, State, State).
nos(assign(X, E), State, NewState) :-
    eval_aexp(E, State, V),
    update(X, V, State, NewState).
nos(comp(S1, S2), State, NewState) :- nos(S1, State, State1), nos(S2, State1, NewState).
nos(if(B, S1, _), State, NewState) :- eval_bexp(B, State, true), nos(S1, State, NewState).
nos(if(B, _, S2), State, NewState) :- eval_bexp(B, State, false), nos(S2, State, NewState).
nos(while(B, S), State, NewState) :- eval_bexp(B, State, true), nos(S, State, State1), nos(while(B, S), State1, NewState).
nos(while(B, _), State, State) :- eval_bexp(B, State, false).
nos(do_while(S, B), State, NewState) :- nos(S, State, State1), eval_bexp(B, State, true), nos(do_while(S, B), State1, NewState).
nos(do_while(S, B), State, NewState) :- nos(S, State, NewState), eval_bexp(B, State, false).


% test cases
test1 :- 
    Program = assign(x, num(5)),
    nos(Program, [], State),
    lookup(State, x, Value),
    write('x = '), write(Value), nl.

test2 :-
    Program = comp(assign(x, num(3)),
                  assign(x, add(var(x), num(1)))),
    nos(Program, [], State),
    lookup(State, x, Value),
    write('x = '), write(Value), nl.

% Test 3: Factorial calculation (5!)
test3 :-
    Program = comp(assign(result, num(1)),
              comp(assign(n, num(5)),
                   while(neg(aeq(var(n), num(0))),
                         comp(assign(result, mult(var(result), var(n))),
                              assign(n, sub(var(n), num(1))))))),
    nos(Program, [], State),
    lookup(State, result, Value),
    write('5! = '), write(Value), nl.

% Test 4: Sum of numbers from 1 to 10
test4 :-
    Program = comp(assign(sum, num(0)),
              comp(assign(i, num(1)),
                   while(gte(num(10), var(i)),
                         comp(assign(sum, add(var(sum), var(i))),
                              assign(i, add(var(i), num(1))))))),
    nos(Program, [], State),
    lookup(State, sum, Value),
    write('Sum 1 to 10 = '), write(Value), nl.

% Test 5: GCD calculation using do-while
test5 :-
    Program = comp(assign(a, num(48)),
              comp(assign(b, num(36)),
                   do_while(
                       if(gte(var(a), var(b)),
                          assign(a, sub(var(a), var(b))),
                          assign(b, sub(var(b), var(a)))),
                       neg(aeq(var(a), num(0)))))),
    nos(Program, [], State),
    lookup(State, b, Value),
    write('GCD of 48 and 36 = '), write(Value), nl.