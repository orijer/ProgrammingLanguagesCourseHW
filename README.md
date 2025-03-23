# ProgrammingLanguagesCourseHW
A repository that holds the interesting parts of the homework assignments from the principles of programming languages course I took during my studies.

In it, we learned a lot of concepts, including but not limited to: Natural&Axiomatic Semantics of programming languages, Lambda Calculus (including Simply Typed Lambda Calculus), The basics of functional programming in Ocaml, and Logical programming in Prolog.

The homemwork assignments were partly theoretical and partly programming. In this repository I include the programing parts.

## Lambda Calculus Interpreter in Ocaml
In this assignment we needed to create a simple Lambda Calculus interpreter in Ocaml. In order to see how to use it you can go to `tests.ml` and see the syntax.

Compile: `ocamlc -o tests utils.ml lexer.ml parser.ml reducer.ml tests.ml` and then use: `./tests`.

You can of course create a script yourself and run it instead of tests.

## While Language Interpter in Ocaml
In class we learned about Natural&Axiomatic semantics through a small language that we called the 'While Language', which is a basic language that allows assigning, computing, loops and if statements.

In this assignment we were tasked with creating an interpreter for it written in Ocaml. To show that this language is a real programming language we were asked to implement bubble sort itten entirely in the While Language, which you can find in `bubble_sort.ml`.

Compile: `ocamlc -o bubble_sort semantics.ml nos.ml bubble_sort.ml` n then use `./bubble_sort`.

You can of course create a script yourself and run it instead of bubble_sort.

## While Language Interpreter in Prolog
In the last part of the course we learned about Logical programming through Prolog, so in the last assignment we ha to create another interpreter for the While Language, but now in Prolog. The nice thing about Prolog is that it is very readable and manageable, so the code for it is much shorter and simpler then the Ocaml version.

In order to run it I used swipl: `swipl` -> `consult('nos_interpreter.pl).` -> `test5.` (and of course you can write your own code and run it instead of test5).
