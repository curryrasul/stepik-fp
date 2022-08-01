import Data.Char

-- 1.2
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x > 0 then 1 else if x < 0 then (-1) else 0

-- 1.3
x |-| y = abs (x - y)

-- 1.4
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x) && (isDigit y) then (digitToInt x) * 10 + (digitToInt y) else 100


dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- 1.5
doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 0 = 1
doubleFact n = n * doubleFact(n - 2)


fibonacci :: Integer -> Integer
fibonacci n
    | n == 0    = 0
    | n == 1    = 1
    | n > 0     = fibonacci (n - 1) + fibonacci (n - 2)
    | otherwise = (-1) ^ (abs n + 1) * fibonacci (abs n)


optFibonacci :: Integer -> Integer
optFibonacci = helper 0 1
helper a b n
    | n > 0 = helper b (a + b) (n - 1)
    | n < 0 = helper (b - a) a (n + 1)
    | otherwise = a


-- 1.6
seqA :: Integer -> Integer
seqA n
    | n < 0 = undefined
    | otherwise = let
        helper a b c n 
            | n == 0 = a
            | n == 1 = b
            | n == 2 = c
            | otherwise = helper b c (b + c - 2 * a) (n - 1) 
    in helper 1 2 3 n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x 
    | x == 0 = (0, 1)
    | otherwise = let
        helper (curSum, curDigits) x
            | x == 0 = (curSum, curDigits)
            | otherwise = helper (curSum + rem x 10, curDigits + 1) (div x 10)
        in helper (0, 0) (abs x)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + helper 0 (a + h) (n - 1))
    where
        helper curSum curPoint n
            | n == 0    = curSum
            | otherwise = helper (curSum + f curPoint) (curPoint + h) (n - 1)
        n = 1000
        h = (b - a) / n
