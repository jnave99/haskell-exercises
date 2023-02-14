
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

