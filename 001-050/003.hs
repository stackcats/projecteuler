largestPrimeFactor :: Int -> Int -> Int
largestPrimeFactor n m
  | n == m = n
  | n `mod` m == 0 = largestPrimeFactor (n `div` m) m
  | otherwise = largestPrimeFactor n (m + 1)

largestPrimeFactor 600851475143 2
