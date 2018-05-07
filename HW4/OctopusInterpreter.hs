{- Klemen Kotar, UW NETID: klemen, Section AA -}

{- Starter file for CSE 341, Spring 2018, Haskell Project:
   the Octopus Interpreter -}
 

module OctopusInterpreter
    where

import OctoParser
import Data.Char
import Data.Maybe
import Test.HUnit
import Control.Monad

{- The heart of the interpreter is the eval function, which takes an
OctoValue and evaluates it in the given environment.  As in Racket,
expressions are represented as lists.  The eval function is defined by
cases.

The environment is a list of (name,value) pairs.  The type
'Environment' is a synonym for [(OctoValue,OctoValue)], and is defined in
OctoParser.  To look up a name the interpreter searches the environment
starting from the front, so that one variable can shadow another. -}

eval :: OctoValue -> Environment -> OctoValue

-- integers and booleans evaluate to themselves
eval (OctoInt i) env = OctoInt i
eval (OctoBool b) env = OctoBool b

{- To evaluate a symbol, look it up in the current environment and 
return the value found; or raise an exception if it's not found. -}
eval s@(OctoSymbol v) env = case lookup s env of
    Nothing -> error ("name not found: " ++ v)
    Just r -> r

-- A quoted expression evaluates to that expression.
eval (OctoList [OctoSymbol "quote", x]) env = x

{- An expression starting with (lambda ...) evaluates to a closure,
where a closure consists of a list of variable names (OctoSymbols),
the environment of definition, and the body. -}
eval (OctoList [OctoSymbol "lambda"]) env = 
    error "Lambda takes 2 arguments - a list of variables and the expression body - 0 passed"
eval (OctoList [OctoSymbol "lambda", _]) env = 
    error "Lambda takes 2 arguments - a list of variables and the expression body - 1 passed"
eval (OctoList [OctoSymbol "lambda", OctoList vars, body]) env = 
    OctoClosure vars env body
eval (OctoList [OctoSymbol "lambda", _, _]) env = 
    error "Lambda takes 2 arguments - a list of variables and the expression body - incorrect types passed"

{- An expression starting with if evaluates its first argument
  as the predicate and then returns the evaluation of its second 
  argument if the predicate was True, else it returns the evaluation of 
  its third argument. In Racket/Octopus any value that is not explicitly
  #f is treated as true so we check if the predicate is false in 
  our if statement and if so return the false case, else we return 
  the true case -}
eval (OctoList [OctoSymbol "if"]) env =
    error "if takes 3 arguments - a predicate, a true clause and a false clause - 0 passed"
eval (OctoList [OctoSymbol "if", _]) env =
    error "if takes 3 arguments - a predicate, a true clause and a false clause - 1 passed"
eval (OctoList [OctoSymbol "if",_ ,_]) env =
    error "if takes 3 arguments - a predicate, a true clause and a false clause - 2 passed"
eval (OctoList [OctoSymbol "if", pred, tCase, fCase]) env = 
    if (evalFor pred == OctoBool False)
      then evalFor fCase 
      else evalFor tCase
        where evalFor exp = eval exp env

-- A conds statmenet is converted to a nested octopus if statmenet and then evaluated
eval (OctoList [OctoSymbol "cond"]) env = error "cond needs at least one case, 0 passed"
eval (OctoList (OctoSymbol "cond":cases)) env = 
    eval (parse (cond_to_if cases)) env

{- A let expression evaluates its bindings, adds them to its
   envirnoment and then evaluates its body in the new envirnoment -}
eval (OctoList [OctoSymbol "let"]) env =
    error "let takes 2 arguments - a list of bindings and a body - 0 passed"
eval (OctoList [OctoSymbol "let", _]) env =
    error "let takes 2 arguments - a list of bindings and a body - 1 passed"
eval (OctoList [OctoSymbol "let", OctoList vars, body]) env =
    eval body ext_env
    where ext_env = (map (eval_let_bindings env) vars) ++ env
eval (OctoList [OctoSymbol "let", _, _]) env =
    error "let takes 2 arguments - a list of bindings and a body - incorrect types passed"

{- A letrec expression works like a regular let, except that it
   evaluates its bindings within the extended environment, as
   it builds up the extended environment -}
eval (OctoList [OctoSymbol "letrec"]) env =
    error "letrec takes 2 arguments - a list of bindings and a body - 0 passed"
eval (OctoList [OctoSymbol "letrec", _]) env =
    error "letrec takes 2 arguments - a list of bindings and a body - 1 passed"
eval (OctoList [OctoSymbol "letrec", OctoList vars, body]) env =
    eval body ext_env
    where ext_env = (map (eval_let_bindings ext_env) vars) ++ env
eval (OctoList [OctoSymbol "letrec", _, _]) env =
    error "letrec takes 2 arguments - a list of bindings and a body - incorrect types passed"

eval (OctoList [OctoSymbol "define", var, def]) env = 
    OctoEnvironment $ [(var, eval def env)] ++ env

eval (OctoList [OctoSymbol "load", OctoSymbol s]) eval = (OctoFile s)

{- If we don't match any of the special cases, the first thing in the
list should evaluate to a function.  Apply it to its arguments.  There
are two cases: either the function is a user-defined function, or a
primitive.  These are handled separately.  In either case, the
arguments are found by evaluating each of the expressions after the
function name in the current environment. -}
eval (OctoList (f:xs)) env = case eval f env of
    c@(OctoClosure _ _ _) -> apply c args
    (OctoPrimitive p) -> fromJust (lookup p primitives) $ args
    where  args = map (\x -> eval x env) xs

-- Helper function that evaluates the bindings of a let statement
eval_let_bindings :: Environment -> OctoValue -> (OctoValue, OctoValue)
eval_let_bindings env (OctoList [name, value]) = (name, eval value env)

-- Helper function that conferts an Octopus cond statement to an Octopus
-- if statement
cond_to_if :: [OctoValue] -> String
cond_to_if [OctoList [_, body]] = octoshow body
cond_to_if ((OctoList [pred, body]):xs) = 
    unwords ["(if ", octoshow pred, octoshow body, cond_to_if xs, ")"]

{- Apply a user-defined function to the given arguments.  The user-defined
function has already been evaluated to get an OctoClosure, and the arguments
have already been evaluated as well in the calling environment.
Then make a new environment by extending the environment of definition
of the lambda (which is part of the closure).  In the extended
environment, the actual args are bound to the respective formal names,
evaluate the body of the function in this new environment, and return
the result. -}
-- merge args and vals together and add them to the end of f_env -> after eval body with new f_env
-- function that applies a user defined function Octopus to the given arguments
apply (OctoClosure vars f_env body) args = eval body $ (zip vars args) ++ f_env


-- list of primitive functions and their definitions in Haskell
-- for the starter, we only have +
-- you need to add various other functions
primitives = [("+",octoplus), ("-",octominus), ("*",octomulti), ("cons",octocons), ("car",octocar), ("cdr",octocdr), ("equal?",octoeq), ("eval",octoeval)]

-- helper function for arithmetic functions (if we defined OctoInt using
-- record syntax we wouldn't need this, but I didn't feel like cluttering
-- everything else up)
getint (OctoInt i) = i
-- The octoplus function takes a list of OctoInts and adds them.
octoplus = OctoInt . sum . map getint
-- The octominus function takes lists of OctoInts and subtracts them according to Racket rules
-- Unlike with (+), Racket/Octopus throw an error when (-) is called with no arguments
-- If a single OctoInt is passed to (-), it returns the negative of the OctoInt
octominus [] = error "(-) takes at least 1 argument, 0 given"
octominus [(OctoInt x)] = OctoInt (-x)
-- If multiple OctoInt's are passed to (-) it subtracts them all from the first OctoInt
octominus xs = OctoInt . foldl1 (-) $ map getint xs
-- The Octopus function that takes a list of OctoInts and multiplies them
octomulti = OctoInt . product . map getint
-- The Octopus function that takes an OctoValue and an OctoList and conses them
octocons [] = error "cons takes two arguments, 0 given"
octocons [x] = error "cons takes two arguments, only 1 given"
octocons [x, (OctoList xs)] = OctoList (x:xs)
octocons [x, y] = error "cons takes an atom and a list, incorrect data types given"
-- The Octopus function that returns the first element of an OctoList
octocar ([OctoList []]) = error "You can not take the car of an empty list" 
octocar (OctoList x:_) = head x
-- The Octopus function that returns the tail of an OctoList
octocdr ([OctoList []]) = error "You can not take the cdr of an empty list" 
octocdr (OctoList x:_) = OctoList (tail x)
-- The Octopus function that checks for equality
octoeq [] = error "You need two elements to check them for equality, 0 given"
octoeq [a] = error "You need two elements to check them for equality, only 1 given"
octoeq [a,b] = OctoBool (a == b)
-- The Octopus function that evaluates a quotes Octopus expression
octoeval [OctoList []] = error "You can not evaluate an empty list"
octoeval [body] = eval body global_env

-- the global enviroment has null?, and the primitives 
-- (and 'not' after you add it) 
global_env = [
  (OctoSymbol "null?", eval (parse "(lambda (x) (equal? x '()))") global_env),
  -- not returns #t if given an argument that is false, else #t
  (OctoSymbol "not", eval (parse "(lambda (x) (if x #f #t))") global_env)]
   ++ map (\(name,fn) -> (OctoSymbol name, OctoPrimitive name)) primitives
{- null? is defined by evaluating the result of parsing the lambda.
Notice that the environment in which it is evaluated is the global
environment, which we are defining.  This may look circular, but it
works fine in Haskell due to lazy evaluation.  You won't be able to
actually use null? until you have lambda working, but again because
Haskell is lazy it's fine to put the definition in from the start. -}

-- for the unit tests, make a test environment by extending the global env
testenv = [(OctoSymbol "k", OctoInt 5) , (OctoSymbol "s" , OctoSymbol "x")]
   ++ global_env

evparse s = eval (parse s) testenv

-- Function that turns any data of type OctoValue into a string
octoshow :: OctoValue -> String
octoshow (OctoInt i) = show i
octoshow (OctoBool b) = if b then "#t" else "#f"
octoshow (OctoSymbol s) = s
octoshow (OctoList [OctoSymbol "quote",xs]) = "'" ++ (octoshow xs)
octoshow (OctoList xs) = "(" ++ unwords (map octoshow xs) ++ ")"
octoshow (OctoFile f) = f
octoshow (OctoEnvironment env) = "<environment"
octoshow (OctoPrimitive p) = "<primitive " ++ p ++ ">"
octoshow (OctoClosure _ _ _) = "<closure>"

{- REPL Monda -}

-- Read Eval Print loop that enables interactive execution of Octopus code

isEnv (OctoEnvironment _) = True
isEnv _ = False

isFile (OctoFile _) = True
isFile _ = False

getEnv (OctoEnvironment env) = env


repl env = do
  putStr ">> "
  command <- getLine
  replAct env command
  
replAct env val = do
  let value = eval (parse val) env
  if isEnv value then
    repl $ getEnv value
  else if isFile value then do
    contents <- readFile $ octoshow value 
    let commands = lines contents
    mapM_ (replAct env) commands
  else do
    putStrLn (octoshow value)
    repl env

-- Main Monad for running the REPL from command line
--main :: IO String
main = do
  repl global_env

{- unit tests for the interpreter -}
{- the tests that don't work yet with the starter program are commented out -}

-- Define a helper function to make test cases so that they aren't so
-- ridiculously verbose
testeval expr val = TestLabel 
  ("evaluating " ++ expr ++ " should give " ++ show val)
  (TestCase (assertEqual "an OctoTest" val (evparse expr)))
 
-- note that we need to get the white space right for the input strings
-- for this, since we are testing for string equality on the output
show_test_cases = ["7", "#f", "(1 2 3)", "(+ 1 (* 2 3))", "'squid", "'(1 2 3)"]

-- some expressions to test let
shadowing_let1 = " \
\  (let ((k (+ 3 k))  \
\        (j k))       \
\    (+ j k))"

nested_let1 = " \
\  (let ((x 3)       \
\        (y 4))      \
\    (let ((x 100))  \
\      (+ x y)))"

nested_let2 = " \
\  (let ((x 3)       \
\        (y 4))      \
\    (let ((x (+ x y)))  \
\      (+ x y)))"

nested_let3 = " \
\  (let ((n 10))                     \
\    (let ((f (lambda (x) (+ x n)))  \
\          (n 3))                    \
\      (+ (f 100) n)))"

{- Expr to test that lambda is closing over its environment of
definition.  Here, n is defined in the let but not where f is used --
so we would get an error if the body of the lambda were evaluated in
the wrong environment.-}
let_test_closure = "                         \
\ (let ((y 10)                               \
\       (f (let ((n 50))                     \
\            (lambda (x) (+ x (* 2 n))))))   \
\   (f y))"

-- the factorial function
letrec_fact =  " \
\ (letrec       \
\    ((fact (lambda (n) (if (equal? 0 n) 1 (* n (fact (- n 1)))))))   \
\  (fact 4))"

-- the range function returns a list of integers from n down to 0
letrec_range =  " \
\ (letrec       \
\    ((range (lambda (n) (if (equal? 0 n) '() (cons n (range (- n 1)))))))   \
\  (range 4))"

letrec_map =  " \
\ (letrec \
\    ((map (lambda (f s) \
\        (if (null? s) '() (cons (f (car s)) (map f (cdr s)))))))  \
\  (map (lambda (n) (* n 2)) '(10 20 30)))"

-- Curried version of map. Here map itself takes one argument (a function), 
-- and returns a new function that maps over a list.
letrec_curried_map =  "                                                     \
\ (letrec                                                                   \
\    ((map (lambda (f)                                                      \
\            (lambda (s)                                                    \
\              (if (null? s) '() (cons (f (car s)) ((map f) (cdr s))))))))  \
\  ((map (lambda (n) (* n 2))) '(10 20 30)))"

-- test mutual recursion in letrec with a ridiculously slow version of
-- the mod2 function
letrec_mod2 = "                                                 \
\ (letrec                                                       \
\     ((zero? (lambda (x) (equal? x 0)))                        \
\      (even? (lambda (x) (if (zero? x) #t (odd? (- x 1)))))    \
\      (odd?  (lambda (x) (if (zero? x) #f (even? (- x 1)))))   \
\      (mod2 (lambda (x) (if (even? x) 0 1))))                   \
\   (cons (mod2 5) (cons (mod2 6) '())))"

-- overcommented expression to test handling Racket comments
complicated_comment_test = "  ; this is a comment on its own line \n \
\  (+ 3  ; another comment \n  \
\  4  )  \n      \
\ ; and a final comment \n"

tests = TestList [
  testeval "#t" (OctoBool True),
  testeval "#f" (OctoBool False),
  testeval "3" (OctoInt 3),
  testeval "'x" (OctoSymbol "x"),
  testeval "'(x 5)" (OctoList [OctoSymbol "x", OctoInt 5]),
  testeval "k" (OctoInt 5),
  testeval "s" (OctoSymbol "x"),
  -- test + for 0, 1, 2, and 4 args
  testeval "(+)" (OctoInt 0),
  testeval "(+ 3)" (OctoInt 3),
  testeval "(+ 3 4)" (OctoInt 7),
  testeval "(+ 3 4 10 20)" (OctoInt 37),
  --test (-) for 1, 2, 4 arguments (omit 0 args test as it crashes in Racket/Octopus)
  testeval "(- 3)" (OctoInt (-3)),
  testeval "(- 16 4)" (OctoInt 12),
  testeval "(- 4 16)" (OctoInt (-12)),
  testeval "(- 120 80 10 200)" (OctoInt (-170)),
  --test * for 0, 1, 2, 4 arguments, including negatives
  testeval "(*)" (OctoInt 1),
  testeval "(* 2)" (OctoInt 2),
  testeval "(* -3)" (OctoInt (-3)),
  testeval "(* 16 4)" (OctoInt 64),
  testeval "(* -2 2)" (OctoInt (-4)),
  testeval "(* -120 80 -10 200)" (OctoInt 19200000),
  --test cons
  testeval "(cons 1 '())" (OctoList [OctoInt 1]),
  testeval "(cons 1 '(1 2))" (OctoList [OctoInt 1, OctoInt 1, OctoInt 2]),
  testeval "(cons 1 '(21 -2 3 1))" (OctoList [OctoInt 1, OctoInt 21, OctoInt (-2), OctoInt 3, OctoInt 1]),
  --test car
  testeval "(car '(1 3 2))" (OctoInt 1),
  testeval "(car '(squid clam starfish))" (OctoSymbol "squid"),
  testeval "(car '(#t #f))" (OctoBool True),
  testeval "(car '( '(1 2) 3 4))" (OctoList [OctoSymbol "quote", (OctoList [OctoInt 1, OctoInt 2])]),
  --test cdr
  testeval "(cdr '(1 2 3))" (OctoList [OctoInt 2, OctoInt 3]),
  testeval "(cdr '(deer bear rabbit))" (OctoList [OctoSymbol "bear", OctoSymbol "rabbit"]),
  testeval "(cdr '(#f #t))" (OctoList [OctoBool True]),
  testeval "(cdr '(1 2 '(3 4)))" (OctoList [OctoInt 2, OctoList [OctoSymbol "quote", OctoList [OctoInt 3, OctoInt 4]]]),
  --test equal?
  testeval "(equal? 1 1)" (OctoBool True),
  testeval "(equal? 'a 'a)" (OctoBool True),
  testeval "(equal? '(squid shark) '(squid shark))" (OctoBool True),
  testeval "(equal? #t #f)" (OctoBool False),
  testeval "(equal? '(a) 'a)" (OctoBool False),
  -- can't use the shortcut for these -- testing octoshow
  TestLabel "octoshow" (TestCase (assertEqual "test octoshow" 
    show_test_cases (map (octoshow . parse) show_test_cases))),
  TestLabel "octoshow primitive" (TestCase (assertEqual "test octoshow" 
    "<primitive *>" (octoshow $ evparse "*"))),
  testeval "( (lambda (x) x) 7)" (OctoInt 7),
  testeval "((lambda (x y) (+ x (+ y 10))) 3 4)" (OctoInt 17),
  -- the inner lambda's y should shadow the outer one, so we get 11 
  -- rather than 3
  testeval "( (lambda (x y) ((lambda (y) (+ x y)) 10)) 1 2)" (OctoInt 11),
  testeval "(null? '())" (OctoBool True),
  testeval "(null? '(squid car))" (OctoBool False),
  testeval "(null? '(lambda (x y) (* x y)) 2 4)" (OctoBool False),
  testeval "(let ((x 3)) (+ x 4))" (OctoInt 7),
  testeval "(let ((x 3) (y 4)) (+ x y))" (OctoInt 7),
  testeval shadowing_let1 (OctoInt 13),
  testeval nested_let1 (OctoInt 104),
  testeval nested_let2 (OctoInt 11),
  testeval nested_let3 (OctoInt 113),
  testeval let_test_closure (OctoInt 110),
  -- The first two if cases have a nonexistant variable on the branch not 
  -- taken. If we evaluated it we would get an error, so if this works it
  -- means 'if' is not evaluating the branch not taken.
  -- The third checks that anything other than #f counts as true.
  -- The fourth makes sure the test is evaluated (it evaluates to #f).
  testeval "(if #t 3 bad)" (OctoInt 3),
  testeval "(if #f bad (+ 2 3))" (OctoInt 5),
  testeval "(if 2 3 5)" (OctoInt 3),
  testeval "(if (equal? k 10) (+ 2 3) (+ 10 20))" (OctoInt 30),
  -- Test not
  testeval "(not 12)" (OctoBool False),
  testeval "(not #f)" (OctoBool True),
  testeval "(not (+ 2 3))" (OctoBool False),
  -- cond
  testeval "(cond (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond (#t (+ 10 10)) (else (+ 2 3)))" (OctoInt 20),
  testeval "(cond (#f bad) (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond ((equal? 1 2) bad) (#f bad) (else (+ 2 3)))" (OctoInt 5),
  testeval "(cond (#f bad) (#t 88) (else (+ 2 3)))" (OctoInt 88),
  -- bind a new name to a primitive and try it
  testeval "(let ((f +)) (f 3 4))" (OctoInt 7),
  -- rebind * (!!!).  This is a very weird thing to do, but it should work
  testeval "(let ((* +)) (* 3 4))" (OctoInt 7),
  testeval "(eval '(+ 2 3))" (OctoInt 5),
  -- more complex eval example -- make sure the argument to eval is evaluated
  -- in the current environment (here with x bound to 10)
  testeval "(let ((x 10)) (eval (cons '+ (cons x (cons 5 '())))))" 
    (OctoInt 15),
  -- another complex eval example -- make sure eval evaluates its expression
  -- in the global environment.  To do this, (yuck) rebind * and make sure the 
  -- expression still uses the global * 
  -- (If you don't believe this is legal, try pasting the part between 
  -- the " marks into racket and evaluating it.)
  testeval "(let ((* null?)) (eval (cons '* (cons 3 (cons 5 '())))))"
      (OctoInt 15),
  -- Recursive function tests
  testeval letrec_fact (OctoInt 24),
  testeval letrec_range (evparse "'(4 3 2 1)"),
  testeval letrec_map (evparse "'(20 40 60)"),
  testeval letrec_curried_map (evparse "'(20 40 60)"),
  testeval letrec_mod2 (evparse "'(1 0)"),
  -- tests handling Racket comments
  testeval "(+ 3 4)  ; a comment at the end" (OctoInt 7),
  testeval complicated_comment_test (OctoInt 7),
  -- tests for the string extra credit question
--  testeval "(string-append)" (OctoString ""),
--  testeval "(string-append \"fish \" \"clam \" \"squid\" )" 
--    (OctoString "fish clam squid"),
  -- a final dummy test to avoid problems with the last comma!
  testeval "3" (OctoInt 3)
  ]

run = runTestTT tests 
