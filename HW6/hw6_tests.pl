/* Klemen Kotar CSE 341 AA Spring 2018, Homework 6 
Starter file for tests for HW 6, Prolog Project, CSE 341, Spring 2018
You'll need to add some other tests, as described in the assignment.

Hint: SWI Prolog allows nested comments - this may be useful if you
want to comment out most of the tests initially and then gradually
move the boundary as you get more things working.
*/

/* read in a file hw6.pl (so you should call your homework this!) */
:- consult(hw6).

:- begin_tests(question1).
test(average) :- average([1,2,3,4,5],3).
test(average,  [fail]) :- average([],_).
:- end_tests(question1).


:- begin_tests(question2).
test(enqueue) :- enqueue([1,2,3,4],5,[1,2,3,4,5]).
test(enqueue) :- enqueue([],"Octopus",["Octopus"]).
test(enqueue) :- enqueue(["List"],["One", "Two"],["List",["One", "Two"]]).
test(enqueue) :- enqueue([a,b],c,[a,b,c]).
test(enqueue, [fail]) :- enqueue("Word", ["List","Of","Words"],_).
test(dequeue) :- dequeue([1,2,3,4],1,[2,3,4]).
test(dequeue) :- dequeue([squid, tree, bird], squid, [tree, bird]).
test(dequeue) :- dequeue([42],42,[]).
test(dequeue, [fail]) :- dequeue([],_,_).
test(dequeue, [fail]) :- dequeue(12,_,_).
test(head) :- head([1,2,3],1).
test(head) :- head([a],a).
test(head, [fail]) :- head([],_).
test(head, [fail]) :- head(12,_).
test(empty) :- empty([]).
test(empty) :- A = [], empty[A].
test(empty, [fail]) :- empty[12].
test(dequeue) :-
    empty(Q1),
    enqueue(Q1,squid,Q2),
    enqueue(Q2,clam,Q3),
    dequeue(Q3,X,Q4),
    Q1 = [],
    Q2 = [squid],
    Q3 = [squid, clam],
    Q4 = [clam],
    X = squid.
:- end_tests(question2).

:- begin_tests(question4).
/* First a nondeterministic test of one path: */
test(maze, [nondet]) :- path(allen_basement, ave, 
     [allen_basement, atrium, hub, odegaard, ave], 200).
/* A test for all of the costs for the 4 possible paths 
   (ignoring the route).  We use 'set' rather than 'all' so that 
   it doesn't matter what order the results come back. */
test(maze, set(C==[20, 195, 200, 210])) :- path(allen_basement, ave, _, C).
/* Finally an exhaustive test of all 4 paths, including both route and costs.
  To make this work with Prolog's unit test framework, we bundle the route
  and cost together into a 'solution', and have a list of solutions.  (The
  name 'solution' is arbitrary -- this could be any name and would 
  still work.) */
test(maze, set(Soln==[ 
   solution([allen_basement, ave],20),
   solution([allen_basement, atrium, hub, odegaard, ave],200),
   solution([allen_basement, atrium, hub, red_square, ave],195),
   solution([allen_basement, atrium, hub, red_square, odegaard, ave],210) 
   ])) :-
   path(allen_basement, ave, S, C), Soln = solution(S,C).
:- end_tests(question4).

:- begin_tests(question6).
/* to avoid roundoff errors, these tests check whether the actual answer is 
   within 0.01 of the expected answer */

/* the expected answer to the following test is A=3 */
test(better_average, [nondet]) :- better_average([1,2,3,4,5],A), {A>2.99}, {A<3.01}.

/* the expected answer to the following test is X=5 */
test(better_average, [nondet]) :- better_average([1,X,3],3), {X>4.99}, {X<5.01}.

/* the expected answer to the following test is X=10 */
test(better_average, [nondet]) :- better_average(Xs,10), Xs=[X], {X>9.99}, {X<10.01}.

/* the expected answer to the following test is Xs=[5,15] and B=15 */
test(better_average, [nondet]) :- better_average(Xs,10), Xs=[5,B], {B>14.99}, {B<15.01}.

test(better_average,  [fail]) :- better_average([],_).

:- end_tests(question6).

:- run_tests.
