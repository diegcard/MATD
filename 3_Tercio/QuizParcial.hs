isPerfect :: [Int] -> Bool
isPerfect xs = all isPerfectNumber (map maximum (init (inits xs)))

isPerfectNumber :: Int -> Bool
isPerfectNumber n = n == sum (filter (\x -> n `mod` x == 0) [1..n-1])


-------------------------------------------------------------------------------------



isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

isPrimita :: [Int] -> Bool
isPrimita xs = all isPrime (map minimum (tail (tails xs)))


-------------------------------------------------------------------------------------



subPalin :: Eq a => [a] -> [a]
subPalin xs = maximumBy (comparing length) [sublist | sublist <- subsequences xs, isPalindrome sublist]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs


-------------------------------------------------------------------------------------



isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0

imparin :: Int -> Bool
imparin n = n == product (filter isOdd [1,3..n-1])

imparinList :: [Int] -> [Int]
imparinList xs = filter imparin xs


-------------------------------------------------------------------------------------



isPrimeNumber :: Int -> Bool
isPrimeNumber n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

isTruncable :: [Int] -> Bool
isTruncable xs = all isPrimeNumber (map listToNumber (tail (inits xs)))

listToNumber :: [Int] -> Int
listToNumber = foldl (\acc x -> acc * 10 + x) 0


-------------------------------------------------------------------------------------


ordinary :: Int -> Bool
ordinary n = n `mod` last (digits n) == 0

compadres :: [Int] -> [(Int, Int)]
compadres xs = [(x, y) | x <- xs, y <- xs, x /= y, gcd x y == 1, ordinary x, ordinary y]

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]