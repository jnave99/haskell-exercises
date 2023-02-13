import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

xInt :: Maybe Int
xInt = elemIndex 3 [1, 2, 3, 4, 5]

yInt :: Maybe Int 
yInt = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max 

maxed :: Maybe Int 
maxed = max' <$> xInt <*> yInt

xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer 
x2 = lookup 3 $ zip xs ys 

y2 :: Maybe Integer 
y2 = lookup 2 $ zip xs ys 

summed :: Maybe Integer 
summed = fmap sum $ (,) <$> x2 <*> y2 


newtype Identity' a = Identity' a
    deriving (Eq, Ord, Show)

instance Functor Identity' where
    fmap f (Identity' a) = Identity' (f a)

instance Applicative Identity' where
    pure a = Identity' a
    (Identity' f) <*> (Identity' a) = Identity' (f a)

{- Maybe Applicative -}

validateLength :: Int
               -> String
               -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

newtype Name =
    Name String deriving (Eq, Show)
newtype Address =
    Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
    Person Name Address
    deriving (Eq, Show)

mkPerson :: String
         -> String
         -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
    | n >= 0 = Just n
    |   otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString name' age' weight' = Cow 
    <$> noEmpty name' 
    <*> noNegative age' 
    <*> noNegative weight'


{- List Applicative -}

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons a t) = Cons (f a) (fmap f t)  

instance Semigroup (List a) where 
    Nil <> t@(Cons _ _) = t
    (Cons a li) <> Nil = Cons a li
    (Cons a Nil) <> t@(Cons _ _) = Cons a t  
    (Cons a li) <> t@(Cons _ _) = Cons a (li <> t)

instance Applicative List where 
    pure a = Cons a (Nil)
    _ <*> Nil = Nil 
    Nil <*> _ = Nil
    (Cons f Nil) <*> (Cons a Nil) = Cons (f a) Nil 
    (Cons f Nil) <*> (Cons a li') = Cons (f a) (fmap f li')
    (Cons f li) <*> t@(Cons a Nil) = (Cons (f a) Nil) <> (li <*> t)
    (Cons f li) <*> t@(Cons a li') = (Cons (f a) (fmap f li')) <> (li <*> t)

data Errors =
    DividedByZero
    | StackOverflow
    | MooglesChewedWires
    deriving (Eq, Show)

data Validation e a = 
    Failure e
  | Success a 
  deriving (Eq, Show) 

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where 
    pure a = Success a 
    (Failure a) <*> (Failure a') = Failure (a <> a')
    (Failure a) <*> _ = Failure a 
    _ <*> (Failure a) = Failure a 
    (Success f) <*> (Success a) = Success (f a)
