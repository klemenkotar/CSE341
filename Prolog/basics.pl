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
