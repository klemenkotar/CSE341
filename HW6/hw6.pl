/* Klemen KOtar CSE 341 Spring 2018, Section AA, Homework 6 
CSE 341 - starter code for family tree and path through a maze questions */

:- use_module(library(clpr)).
:- use_module(library(clpfd)).

/* QUESTION 1 */
/* average computes the average of a list of numbers that is grounded */
average(Xs, A) :-
    sum(Xs, Sum),
    length(Xs, Length),
    A is Sum/Length.
/* helper function that computes the sum of a list of numbers */
sum([X], X).
sum([X|Xs], Sum) :-
    sum(Xs, Rest),
    Sum is X + Rest.

/* QUESTION 2 */
/* enqueue puts an element at the end of a queue */
enqueue(Q1,X,Q2) :- append(Q1,[X],Q2).
/* dequeue removes the first element from a queue */
dequeue([Q|Qs],Q,Qs).
/* head finds the head of the queue */
head([Q|_],Q).
/* empty checks if the a queue is empty */
empty([]).


/* QUESTION 3 */
/* relates person and their grandmother */
grandmother(Grandmother, Grandchild) :-
    parent(Parent, Grandchild),
    parent(Grandmother, Parent),
    woman(Grandmother).

/* relates person and their son */
son(Son, Parent) :-
    parent(Parent, Son),
    man(Son).

/* relates person and their ancestor */
ancestor(Ancestor, Person) :-
    parent(Ancestor, Person).
ancestor(Ancestor, Person) :-
    parent(Parent, Person),
    ancestor(Ancestor, Parent).

/* Norwegian royal family facts */
man('Haakon VII').
man('Olav V').
man('Harald V').
man('Haakon').
woman('Martha').
woman('Mette-Marit').
woman('Maud').
woman('Sonja').
parent('Haakon VII','Olav V').
parent('Maud','Olav V').
parent('Olav V','Harald V').
parent('Martha','Harald V').
parent('Harald V','Haakon').
parent('Sonja','Haakon').

/* QUESTION 4 */
/* Path finds paths from one location to another through defined edges,
   records the stops and computes the path cost */
path(A, A, [A], 0).
path(Start, Finish, [Start|Stops], Cost) :-
    edge(Start, Stop, StopCost),
    path(Stop, Finish, Stops, PathCost),
    Cost is StopCost + PathCost.

/* maze facts */
edge(allen_basement, atrium, 5).
edge(atrium, hub, 10).
edge(hub, odegaard, 140).
edge(hub, red_square, 130).
edge(red_square, odegaard, 20).
edge(red_square, ave, 50).
edge(odegaard, ave, 45).
edge(allen_basement, ave, 20).

/* QUESTION 5 */
/* Symbolic differentiation in Prolog */

/* Find the derivative of expression A with respect to variable X.
   Use cut to prune away any alternate solutions. */

/* The expression and variable should be ground and the result a variable.
   Prolog has mode declarations (just comments for SWI Prolog though):
      deriv(+Expr,+Var,-Result)
   See http://www.swi-prolog.org/pldoc/man?section=preddesc
*/

deriv(A,X,C) :- basic_deriv(A,X,B), simplify(B,C), !.

basic_deriv(N,_,0) :- number(N).
basic_deriv(X,X,1).
basic_deriv(Y,X,0) :- atom(Y), Y\==X.

basic_deriv(A+B,X,A1+B1) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

/* basic_deriv rule for subtraction */
basic_deriv(A-B,X,A1-B1) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

basic_deriv(A*B,X,A*B1+A1*B) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

/* basic_deriv rule for sin */
basic_deriv(sin(A),X,A1*cos(A)) :- basic_deriv(A,X,A1).

/* basic_deriv rule for cos */
basic_deriv(cos(A),X,(-1*A1)*sin(A)) :- basic_deriv(A,X,A1).

/* basic_deriv rule for exp */
basic_deriv(A^B,X,B*A^(B-1)*A1) :- basic_deriv(A,X,A1).

simplify(X,X) :- atom(X).
simplify(N,N) :- number(N).

simplify(A+B,C) :- 
   simplify(A,A1),
   simplify(B,B1),
   simplify_sum(A1+B1,C).

/* simplify rule for subtraction */
simplify(A-B,C) :-
    simplify(A,A1),
    simplify(B,B1),
    simplify_difference(A1-B1,C).

simplify(A*B,C) :- 
   simplify(A,A1),
   simplify(B,B1),
   simplify_product(A1*B1,C).

/* simplify rule for sin */
simplify(sin(A),C) :-
    simplify(A,A1),
    simplify_sin(sin(A1),C).

/* simplify rule for cos */
simplify(cos(A),C) :-
    simplify(A,A1),
    simplify_cos(cos(A1),C).

/* simplify rule for exponents */
simplify(A^B, C) :-
    simplify(A,A1),
    simplify(B,B1),
    simplify_exp(A1^B1,C).


simplify_sum(0+A,A).
simplify_sum(A+0,A).
simplify_sum(A+B,C) :- number(A), number(B), C is A+B.
simplify_sum(A+B,A+B).

/* simplify_difference simplifies a subtraction expression as much as possible */
simplify_difference(0-0,0).
simplify_difference(0-A,-A).
simplify_difference(A-0,A).
simplify_difference(A-B,C) :- number(A), number(B), C is A-B.
simplify_difference(A-B,A-B).

simplify_product(0*_,0).
simplify_product(_*0,0).
simplify_product(1*A,A).
simplify_product(-1*A,-A). /* simplifying expressions multiplied by -1 */
simplify_product(A*1,A).
simplify_product(A*B,C) :- number(A), number(B), C is A*B.
simplify_product(A*B,A*B).

/* simplify_difference simplifies a sin expression as much as possible */
simplify_sin(sin(A),C) :- number(A), C is sin(A).
simplify_sin(sin(A),sin(A)).

/* simplify_difference simplifies a cos expression as much as possible */
simplify_cos(cos(A),C) :- number(A), C is cos(A).
simplify_cos(cos(A),cos(A)).

/* simplify_difference simplifies an exponential expression as much as possible */
simplify_exp(_^0,1).
simplify_exp(A^1,A).
simplify_exp(A^B,C) :- number(A), number(B), C is A^B.
simplify_exp(A^B,A^B).


/* QUESTION 6 */
/* average computes the average of a list using the clpr library */
better_average(Xs, A) :-
    length(Xs, Length),
    better_sum(Xs, Sum),
    {A = Sum/Length}.
/* helper function that computes the sum of a list of numbers using clpr*/
better_sum([X], X).
better_sum([X|Xs], Sum) :-
    better_sum(Xs, Rest),
    {Sum = X + Rest}.

/* QUESTION 7 */
/* puzzle solves the B,O,B] * [B,O,B] = [M,A,R,L,E,Y] puzzle using a brute force
   approach with clpfd */
puzzle([B,O,B] * [B,O,B] = [M,A,R,L,E,Y]) :-
        Vars = [B,O,M,A,R,L,E,Y],
        Vars ins 0..9,
        all_different(Vars),
                                     (B*100 + O*10 + B) * 
                                     (B*100 + O*10 + B) #=
        M*100000 + A*10000 + R*1000 + L*100 + E*10 + Y,
        M #\= 0, B #\= 0.

/* running puzzle(As*As=Cs), label(As). solves the riddle and produces the 2 results:
    
    As = [3, 5, 3],
    Cs = [1, 2, 4, 6, 0, 9] ;
    (B=3, O=5, M=1, A=2, R=4, L=6, E=0, Y=9)

    As = [9, 2, 9],
    Cs = [8, 6, 3, 0, 4, 1] .
    (B=9, O=2, M=8, A=6, R=3, L=0, E=4, Y=1)

*/





