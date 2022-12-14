import Data.Char
import Data.ByteString (foldl')

-- 3.1
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b l = a : b : l

nTimes:: a -> Int -> [a]
nTimes a n = helper [] n
    where 
        helper curList n
            | n == 0 = curList
            | otherwise = helper (a : curList) (n - 1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
    | odd x = x : oddsOnly xs
    | otherwise = oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l

-- Sum
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- Product
myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- Maximum
myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "empty list"
myMaximum [x] = x
myMaximum (x : xs) = max x $ myMaximum xs

-- Minimum
myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "empty list"
myMinimum [x] = x
myMinimum (x : xs) = min x $ myMinimum xs

--
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = sum2 x $ sum2 y z
    where
        sum2 x [] = x
        sum2 [] y = y
        sum2 (x : xs) (y : ys) = (x + y) : sum2 xs ys


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = ys : groupElems zs where
    (ys, zs) = span (== head xs) xs

-- 3.2
readDigits :: String -> (String, String)
readDigits = span isDigit 


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = []
filterDisj p1 p2 (x : xs)
    | p1 x || p2 x = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p : xs) = qsort lt ++ [p] ++ qsort gt
    where
        lt = filter (< p) xs
        gt = filter (>= p) xs


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x : xs) = concatMap (insertElem x) (perms xs) 
    where
        insertElem x [] = [[x]]
        insertElem x yss@(y : ys) = (x : yss) : map (y : ) (insertElem x ys)


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> a `max` b `max` c)


-- 3.3
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


repeatHelper = id


data Odd = Odd Integer 
    deriving (Eq, Show)

instance Enum Odd where
    succ (Odd x) = Odd $ x + 2
    pred (Odd x) = Odd $ x - 2
    toEnum x = Odd $ toInteger x * 2 + 1
    fromEnum (Odd x) = quot (fromInteger x - 1) 2
    enumFrom = iterate succ
    enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
    enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
    enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]


coins = []
change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]

-- 3.4
concatList :: [[a]] -> [a]
concatList = foldr (++) []


lengthList :: [a] -> Int
lengthList = foldr (const succ) 0


sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0


-- 3.5
meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, len) -> (s + x, len + 1)) (0, 0)


evenOnly :: [a] -> [a]
evenOnly = reverse . fst . foldl (\(l, curLen) x -> if odd curLen then (x : l, curLen + 1) else (l, curLen + 1)) ([], 0)

evenOnlyInf :: [a] -> [a]
evenOnlyInf = snd . foldr (\x ~(xs, ys) -> (x : ys, xs)) ([], [])


-- 3.6
lastElem :: [a] -> a
lastElem = foldl1 (flip const)


unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f ini = 
    helper (f ini)
    where
        helper Nothing = []
        helper (Just (x, ini')) = x : unfoldr f ini'
        
revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(a,b) -> if b < a then Nothing else Just (b,(a, pred b)))