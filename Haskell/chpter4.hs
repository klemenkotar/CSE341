maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can not get maximum of an empty list"
maximum' [x] = x
maximum'(x:xs)
  | x > maxTail = x
  | otherwise   = maxTail
  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n a
  | n <= 0    = []
  | otherwise = a:replicate' (n-1) a

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _
  | i <= 0     = []
take' _ []     = []
take' i (x:xs) = x:take' (i-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat' a 

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' [a | a <- xs, a <= x] ++ [x] ++ quicksort' [a | a <- xs, a > x]
