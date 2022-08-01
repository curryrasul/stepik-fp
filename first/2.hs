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
doItYourself = f . g . h
f = logBase 2
g = (^3)
h = max 42


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