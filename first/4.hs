import Data.Time.Clock
import Data.Time.Format
-- import System.Locale

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


-- data LogLevel = Error | Warning | Info

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


-- 4.2
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2


data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


data Result' = Fail' Int | Success'

instance Show Result' where
    show (Fail' n)  = "Fail: " ++ show n
    show Success' = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' s = case doSomeWork s of
    (Fail, n) -> Fail' n
    (Success, _) -> Success'


square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False


-- 4.3
timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info
    deriving (Show)

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString le = timeToString (timestamp le) ++ ": "
    ++ logLevelToString (logLevel le) ++ ": " ++ message le


data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName person1 person2 = person2 { lastName = lastName person1 }


-- data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p @ Person { firstName = (x : _ : _) } = p { firstName = x : "." }
abbrFirstName p = p


