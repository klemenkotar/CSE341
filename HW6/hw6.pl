/* Klemen KOtar CSE 341 Spring 2018, Section AA, Homework 6 
CSE 341 - starter code for family tree and path through a maze questions */

/* PROBELM 1 */
/* average computes the average of a list of numbers that is grounded */
average([X|Xs], A) :-
    sum([X|Xs], S),
    length([X|Xs], L),
    A is S/L.
sum([], 0).
sum([X|Xs], S) :-
    sum(Xs, Rest),
    S is X + Rest.

/* PROBLEM 2 */
/* enqueue puts an element at the end of a queue */
enqueue(Q1,X,Q2) :- append(Q1,[X],Q2).
/* dequeue removes the first element from a queue */
dequeue([Q|Qs],Q,Qs).
/* head finds the head of the queue */
head([Q|_],Q).
/* empty checks if the a queue is empty */
empty([]).


/* PROBLEM 3 */
grandmother(Grandmother, Grandchild) :-
    parent(Parent, Grandchild),
    parent(Grandmother, Parent),
    woman(Grandmother).

son(Son, Parent) :-
    parent(Parent, Son),
    man(Son).

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

/* PROBLEM 4 */
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
