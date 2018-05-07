likes(fred, beer).
likes(fred, cheap_cigars).
likes(fred, pro_wrestling).

likes(sue, jogging).
likes(sue, kale).
likes(sue, bicycling).

likes(mary, jogging).
likes(mary, kale).
likes(mary, bicycling).
likes(mary, rush_limbaugh).

health_freak(X) :-
    likes(X, kale),
    likes(X, jogging).

left_wing(X) :-
    likes(X, rush_limbaugh).

low_life(X) :-
    likes(X, cheap_cigars).

prerequisite(cse142,cse143).
prerequisite(cse143,cse311).
prerequisite(cse311,cse312).
prerequisite(cse143,cse331).
prerequisite(cse143,cse341).

/* take_before(A,B) suceeds if you must take A before B */
take_before(X,Z) :- prerequisite(X,Z).
take_before(X,Z) :- prerequisite(X,Y),
		    take_before(Y,Z).

myappend([],Ys,Ys).
myappend([X|Xs],Ys,Answer) :- Answer=[X|Zs], myappend(Xs,Ys,Zs).

factorial(0,1).
factorial(N,F) :-
    N>0,
    N1 is N-1,
    factorial(N1,F1),
    F is N * F1.


insert(X,L,[X|L]).

insert(X,[H|T],[H|U]) :-
    insert(X,T,U).

permute([],[]).

permute([H|T],L) :-
  permute(T,U),
  insert(H,U,L).

sorted([]).
sorted([_]).
sorted([A,B|R]) :-
    A=<B,
    sorted([B|R]).

quicksort([],[]).
quicksort([X|Xs],Sorted) :-
    partition(X,Xs,Smalls,Bigs),
    quicksort(Smalls,SortedSmalls),
    quicksort(Bigs,SortedBigs),
    myappend(SortedSmalls,[X|SortedBigs],Sorted).

partition(_,[],[],[]).
partition(Pivot,[X|Xs],[X|Ys],Zs) :-
    X =< Pivot,
    partition(Pivot,Xs,Ys,Zs).
partition(Pivot,[X|Xs],Ys,[X|Zs]) :-
    X > Pivot,
    partition(Pivot,Xs,Ys,Zs).

twins([A,B,C],[A,A,B,B,C,C]).

reverse([], []).
reverse([X|Xs], R) :-
    reverse(Xs,Rs),
    append(Rs, [X], R).

sum_list([],0).
sum_list([X|Xs], Sum) :-
    sum_list(Xs, Rest),
    Sum is X + Rest.
