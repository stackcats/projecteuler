largestPalindromeProduct :: Int
largestPalindromeProduct = maximum [x * y | x <- [999,998..100], y <- [999,998..100], isPalindrome $ x * y]

isPalindrome :: Int -> Bool
isPalindrome n = s == reverse s where s = show n
