
splitWordsList :: [String] -> [[String]]
splitWordsList s = do 
    sentence <- s 
    [words sentence]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []


data Sum a b = 
    First a 
  | Second b 
  deriving (Eq, Show)

instance Functor (Sum a) where 
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a 

instance Applicative (Sum a) where  
    pure x = Second x 
    First f <*> First x = First x
    Second f <*> Second x = Second (f x)
    First f <*> Second x = First f
    Second f <*> First x = First x 

instance Monad (Sum a) where 
    return = pure
    Second x >>= f = f x 
    First x >>= _ = First x

data Nope a =
    NopeDotJpg

instance Functor Nope where 
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
    pure a = NopeDotJpg
    NopeDotJpg <*> _ = NopeDotJpg

instance Monad Nope where 
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg

data BahEither b a =
    PLeft a
    | PRight b

instance Functor (BahEither b) where 
    fmap f (PLeft a) = PLeft (f a)
    fmap _ (PRight b) = PRight b 

instance Applicative (BahEither b) where 
    pure a = PLeft a
    (PLeft f) <*> (PLeft a) = PLeft (f a)
    (PRight f) <*> (PRight a) = PRight a  
    _ <*> (PRight a) = PRight a 
    (PRight f) <*> _ = PRight f
    
instance Monad (BahEither b) where  
    return = pure 
    (PLeft a) >>= f = f a
    (PRight b) >>= _ = PRight b

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

data List a =
    Nil
    | Cons a (List a)

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

instance Monad List where 
    return = pure 
    (Cons a li) >>= f = (f a) <> (li >>= f)
    Nil >>= _ = Nil

