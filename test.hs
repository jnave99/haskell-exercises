{- This is all miscellaneous excersises and some stuff I was playing around with -}

sayHello :: String -> IO ()
sayHello x = 
    putStrLn ("Hello " ++ x ++ "!")

half :: Float -> Float
half x = x / 2

square :: Int -> Int
square x = x * x

circArea :: Float -> Float
circArea r = pi * (r * r)

getCharIn :: String -> Int -> Char
getCharIn s i = s !! (i - 1)

rvrs :: [a] -> [a]
rvrs [] = []
rvrs li = rvrs (tail li) ++ [head li]

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _ = Woot

myAbs :: Integer -> Integer 
myAbs x = 
    if x >= 0 then 
        x
    else 
        x * (-1)


testF :: (a, b) -> (c, d) -> ((b, d), (a, c))
testF a b = ((snd a, snd b), (fst a, fst b))

addTen :: Integer -> Integer -> Integer
addTen x y = x + y + 10

applyInFunc :: Integer -> (Integer -> Integer) -> Integer
applyInFunc x func = func x


{- Type classes-}

data Trivial = Trivial'

instance Eq Trivial where 
    Trivial' == Trivial' = True


data TisAnInteger = 
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn int)
         (TisAn int') =
        int == int'

data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where 
    (==) (Two int1 int2)
         (Two int1' int2') = 
         int1 == int1' &&
         int2 == int2'

data StringOrInt = 
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where 
    (==) (TisAnInt int)
         (TisAnInt int') =
         int == int'
    (==) (TisAString str)
         (TisAString str') = 
         str == str'
    (==) _ _ = False


{- For Pair and Tuple, what's the preferred behavior when 
comparing different incompatible types? Throw error or just False?
Check with typeOf?
-}
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair v1 v2) 
         (Pair v1' v2') =
         v1 == v1' && 
         v2 == v2'

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple v1 v2)
         (Tuple v1' v2') = 
         v1 == v1' &&
         v2 == v2'

data DayOfWeek = 
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False
    
type Subject = String
type Verb = String 
type Object = String 

data Sentence = 
    Sentence Subject Verb Object
    deriving (Eq, Show)

data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu = 
    Papu Rocks Yeah
    deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

f :: (Num a, Fractional a) => a
f = 1.0

fl :: RealFrac a => a
fl = 1.0

freud :: Ord a => a -> a 
freud x = x
