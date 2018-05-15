/* Klemen KOtar CSE 341 Spring 2018, Section AA, Homework 6 
CSE 341 - starter code for family tree and path through a maze questions */

/* PROBELM 1 */
/* average computes the average of a list of numbers that is grounded */
average([X], X).
average([X|Xs], A, L) :-
    average(Xs, B, L),
    A is X/L + B.
average(Xs, A) :-
    length(Xs, L),
    average(Xs, A, L).
    

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

/* maze facts */
edge(allen_basement, atrium, 5).
edge(atrium, hub, 10).
edge(hub, odegaard, 140).
edge(hub, red_square, 130).
edge(red_square, odegaard, 20).
edge(red_square, ave, 50).
edge(odegaard, ave, 45).
edge(allen_basement, ave, 20).
