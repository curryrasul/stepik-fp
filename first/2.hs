import Data.Function

-- 2.1
getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y


func :: a -> a -> b -> a -> a
func x y z q = x
{-
    func x y z q = y
    func x y z q = q
    3
-}


multSecond = g `on` h
g = (*)
h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


-- 2.2
-- doItYourself = f . g . h
-- f = logBase 2
-- g = (^3)
-- h = max 42


-- Answer 2.2.7 = (,)
-- Answer 2.2.8 = snd
-- Answer 2.2.9 = uncurry,flip,(,)

-- 2.3
-- Answer 2.3.3 = Num Bool

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString a
        | a         = "true"
        | otherwise = "false"
        
instance Printable () where
    toString a = "unit type"


instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"


-- 2.4
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
        | doesEnrageGork a && doesEnrageMork a = stomp (stab a)
        | doesEnrageMork a = stomp a
        | doesEnrageGork a = stab a
        | otherwise = a

-- 2.4.5
{-
a = 127.2
b = 24.1
c = 20.1
d = 2
-}

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a
        | a == maxBound = minBound
        | otherwise = succ a 

    spred :: a -> a
    spred a
        | a == minBound = maxBound
        | otherwise = pred a


avg :: Int -> Int -> Int -> Double
avg a b c = (/ 3) $ sum $ map fromIntegral [a, b, c]
