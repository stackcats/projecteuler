divisors :: Int -> [Int]
divisors n = helper n 2
  where
    helper n m
      | n < 2 = []
      | n `mod` m == 0 = m : helper (n `div` m) m
      | otherwise = helper n (m + 1)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
  | x == y = x : merge xs ys
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

smallestMultiple :: [Int] -> Int
smallestMultiple = product . foldr (merge . divisors) []

smallestMultiple [1..20]
