fun :: (Num a) => a -> [a] -> a
fun x y = case x:y of
  (x:y:xs) -> x + y
  [x] -> x
  [] -> 0
  
  
