fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

evenFibs :: Int -> Int
evenFibs n = sum $ filter even $ takeWhile (< n) fibs
