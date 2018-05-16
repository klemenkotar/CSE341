:- use_module(library(clpr)).

distance([X1, Y1], [X2, Y2], D) :-
    {D^2 = (X2-X1)^2 + (Y2-Y1)^2}.
    
distanceX([X1,_], [X2,_], D) :-
    {D = (X2 - X1)}.

distanceY([_,Y1], [_,Y2], D) :-
    {D = (Y2 - Y1)}.

onLine([X1,Y1], [X2,Y2], [XP,YP]) :-
    {M = (Y2-Y1)/(X2-X1)},
    {(YP-Y1) = M*(XP-X1)}.

greaterThan([X,Y], [MinX, MinY]) :-
    {X > MinX},
    {Y > MinY}.

lessThank([X,Y], [MaxX,MaxY]) :-
    {X < MaxX},
    {Y < MaxY}.

fin(F1, F2, F3) :-
    V1 = [0.0,0.0],
    V2 = [70.0, 20.0],
    V3 = [100.0, 0.0],
    C = [70.0, 0.0],
    distance(F1,F2,D1),
    distanceY(F1,C,D1),
    distance(F2,F3,D2),
    distanceY(F3,C,D2),
    onLine(V3,V2,F1),
    onLine(V2,V1,F3),
    distance(F1,F2,D3),
    distance(F2,F3,D3).

fin2(F1) :-
    V1 = [0.0,0.0],
    V2 = [70.0, 20.0],
    C = [70.0, 0.0],
    distance(F1,V2,D1),
    distanceY(F1,C,D1),
    onLine(V1,V2,F1).
