-- 4.1
data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"


charToInt :: Char -> Int
charToInt '0' = 0 
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9 


stringToColor :: String -> Color
stringToColor "Red"   = Red
stringToColor "Green" = Green
stringToColor "Blue"  = Blue


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp x y = compare (ord x)  (ord y) where
   ord Error = 3
   ord Warning = 2
   ord Info = 1



data Result = Fail | Success
data SomeData

-- Just for tests
doSomeWork :: SomeData -> (Result,Int)
doSomeWork = doSomeWork
-- Wrong definition of doSomeWork

processData :: SomeData -> String
processData s = 
    case doSomeWork s of
        (Fail, n) -> "Fail: " ++ show n
        (Success, _) -> "Success"
