data FixMePls a = 
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where 
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

replaceWithP = const 'p'

lms = [ave, n, w]
n = Nothing 
ave = Just "Ave"
w = Just "woohoo"

newtype Identity a = Identity a 

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a 

instance Functor Pair where 
  fmap f (Pair a a') = Pair (f a) (f a')

data Two a b = Two a b 

instance Functor (Two a) where 
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c 

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a 
  fmap f (Second b) = Second (f b)  

data Quant a b =
  Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where 
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a 
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a 

instance Functor (K a) where 
  fmap _ (K a) = K a