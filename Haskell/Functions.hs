lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVector :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVector (x1, y1) (x2, y2) = (x1 + x2, x2 + y2)

head' :: [a] -> a
head' [] = error "Lists of size 0 do not have a head!"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal"
  | weight / height ^ 2 <= 30.0 = "You're fat!"
  | otherwise =                   "You're a whale, congratulations!"

max' :: Ord a => a -> a -> a
max' a b
  | a > b      = a
  | otherwise  = b

myCompare :: Ord a => a -> a -> Ordering
myCompare a b
  | a < b      = LT
  | a == b     = EQ
  | otherwise  = GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmi :: (RealFloat a) => [(a, a)] -> [a]
calcBmi xs = [ bmi | (w, h) <- xs, let bmi = w / h ^2] 

descriptionList :: [a] -> String
descriptionList xs = "The list is " ++ case xs of [] -> "Empty."
                                                  x:[] -> "A singletonlist."
                                                  xs -> "A longer list."
                                                 
