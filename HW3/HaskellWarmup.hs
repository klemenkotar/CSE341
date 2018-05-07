-- Klemen Kotar
-- CSE 341 Homework 3

import Data.Char
import Test.HUnit

-- Takes radius and height of a cone and computes its volume
cone_volume :: Double -> Double -> Double
cone_volume radius height = (pi/3) * height * radius^2

-- Recursively checks if a list of integers is in struct ascending order
ascending :: [Integer] -> Bool
ascending x = (null x || length x == 1) || (head x < head (tail x) && ascending (tail x))

-- Takes list of integers nd returns a list of squares of those integers
squares :: [Integer] -> [Integer]
squares = map (^ 2)

-- Calculates the total resistence of a number of resistors connected in parallel

-- If the input list only contains 0's, the result will be 0. The function first
-- computes 1/0 to be Infinity (in the list comprehension) and then divides 1 by
-- the sum of one or several Infinities (depending on how many 0's are inputed).
-- Either way the computation results to 0. Simmilarly if the input is an empty list
-- the result will be 0, since the list comprehension has no elements to compute with
-- and the function simply evaluates 1/0, which Haskell defines as Infinity.
parallel_resistors :: [Double] -> Double
parallel_resistors resistors  = 1 / sum [ 1/x | x <- resistors]

-- Calculates the total resistence of a number of resistors connected in parallel
-- Pointfree style
pointfree_parallel_resistors :: [Double] -> Double
pointfree_parallel_resistors = (1 /) . sum . map (1 /)

-- Evalueates if a string is a palindrome (non-alphabet characters are ignored)
palindrome :: [Char] -> Bool
palindrome = all equalCouple . revZip . map toLower . filter isLetter

-- Helper function that takes a list and zips it with its reverse
revZip :: [Char] -> [(Char, Char)]
revZip x = zip x (reverse x)   

-- Predicate that tests if two Chars in a couple are equal
equalCouple :: (Char, Char) -> Bool
equalCouple letters = fst letters == snd letters


-- UNIT TESTS

-- testing function provided in homework
{- test whether a number is within epsilon of to another (for unit tests on
   Doubles, to accomodate floating point roundoff errors).  Note that this doesn’t
   work for testing numbers that should be exactly 0 -- for that you should specify
   your own test with an appropriate epsilon -}
is_close x y = abs (x-y) < abs x * epsilon
    where epsilon = 1.0e-6


test1 = TestCase (assertEqual "0 arguments" 0 (cone_volume 0 0))
test2 = TestCase (assertEqual "0 and 1 arguments" 0 (cone_volume 0 1))
test3 = TestCase (assertEqual "1 and 0 arguments" 0 (cone_volume 0 1))
test4 = TestCase (assertBool  "radius 1 and height 1" (is_close 1.047198 (cone_volume 1 1)))
test5 = TestCase (assertBool "radius 5 height 10" (is_close 261.799388 (cone_volume 5 10)))
test6 = TestCase (assertBool "radius -5 height -10" (is_close (- 261.799388) (cone_volume (-5) (-10))))
test7 = TestCase (assertEqual "empty list" True (ascending []))
test8 = TestCase (assertEqual "list with 1 entry" True (ascending [12]))
test9 = TestCase (assertEqual "short ascending list" True (ascending [1,2,3]))
test10 = TestCase (assertEqual "complex ascending list" True (ascending [-12,-9,-3,0,1,4,7,19,23,90,103]))
test11 = TestCase (assertEqual "short descending list" False (ascending [3,2,1]))
test12 = TestCase (assertEqual "complex descending list" False (ascending [-22,-12,0,1,3,2,4,99]))
test13 = TestCase (assertEqual "empty list" [] (squares []))
test14 = TestCase (assertEqual "list with 1 entry" [1] (squares [1]))
test15 = TestCase (assertEqual "simple list" [1, 4, 9] (squares [1, 2, 3]))
test16 = TestCase (assertEqual "complex list" [16, 1, 0, 9, 25] (squares [-4, -1, 0, 3, 5]))
test17 = TestCase (assertEqual "list with 0" 0.0 (parallel_resistors [0]))
test18 = TestCase (assertEqual "list with 1 entry" 1.0 (parallel_resistors [1]))
test19 = TestCase (assertBool "simple list" (is_close 0.5454545454545455 (parallel_resistors [1, 2, 3])))
test20 = TestCase (assertBool "complex list" (is_close 3.5294117647058822 (parallel_resistors [10, 10, 20, 30])))
test21 = TestCase (assertEqual "list with 0" 0.0 (pointfree_parallel_resistors [0]))
test22 = TestCase (assertEqual "list with 1 entry" 1.0 (pointfree_parallel_resistors [1]))
test23 = TestCase (assertBool "simple list" (is_close 0.5454545454545455 (pointfree_parallel_resistors [1, 2, 3])))
test24 = TestCase (assertBool "complex list" (is_close 3.5294117647058822 (pointfree_parallel_resistors [10, 10, 20, 30])))
test25 = TestCase (assertBool "empty palindrome" (palindrome ""))
test26 = TestCase (assertBool "abba" (palindrome "abba"))
test27 = TestCase (assertBool "banana palindrome" (palindrome "Yo! Banana Boy!"))
test28 = TestCase (assertBool "carrot palindrome" (not (palindrome "Yo! Carrot Girl!")))
test29 = TestCase (assertBool "date palindrome" (palindrome "01/02/2010"))


tests = TestList [TestLabel "Problem 1 0 argument test" test1,
                  TestLabel "Problem 1 0 and 1 argument test" test2,
                  TestLabel "Problem 1 1 and 0 argument test" test3,
                  TestLabel "Problem 1 width 1 and height 1 test" test4,
                  TestLabel "Problem 1 width 5 and height 10 test" test5,
                  TestLabel "Problem 1 negative values test" test6,
                  TestLabel "Problem 2 empty list" test7,
                  TestLabel "Problem 2 list with 1 entry" test8,
                  TestLabel "Problem 2 short ascending list" test9,
                  TestLabel "Problem 2 complex ascending list" test10,
                  TestLabel "Probelm 2 short descending list" test11,
                  TestLabel "Probelm 2 complex descending list" test12,
                  TestLabel "Problem 3 empty list" test13,
                  TestLabel "Problem 3 list with 1 entry"  test14,
                  TestLabel "Probelm 3 simple list" test15,
                  TestLabel "Probelm 3 complex list" test16,
                  TestLabel "Problem 4 empty list" test17,
                  TestLabel "Problem 4 list with 1 entry"  test18,
                  TestLabel "Probelm 4 simple list" test19,
                  TestLabel "Probelm 4 complex list" test20,
                  TestLabel "Problem 5 empty list" test21,
                  TestLabel "Problem 5 list with 1 entry"  test22,
                  TestLabel "Probelm 5 simple list" test23,
                  TestLabel "Probelm 5 complex list" test24,
                  TestLabel "Problem 6 empty palindrome" test25,
                  TestLabel "Problem 6 abba"  test26,
                  TestLabel "Probelm 6 banana palindrome" test27,
                  TestLabel "Probelm 6 carrot palindrome" test28,
                  TestLabel "Probelm 6 date palindrome"test29]
run = runTestTT tests
