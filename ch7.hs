
{- Writing functions as anonymous func-}

addOneIfOdd n = case odd n of
    True -> f n 
    False -> n 
    where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

{- User example -}

newtype Username = 
    Username String

newtype AccountNumber = 
    AccountNumber Integer

data User = 
    UnregisteredUser 
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"

printUser (RegisteredUser 
           (Username name)
           (AccountNumber acctNum)) = 
    putStrLn $ name ++ " " ++ show acctNum

{- Pattern matching function def -}
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

{- Case expressions -}

functionC x y = 
    case greaterThan of 
        True -> x
        False -> y
    where greaterThan = x > y

ifEvenAdd2 n = 
    case even n of 
        True -> (n + 2)
        False -> n

nums x = 
    case compare x 0 of 
        LT -> -1
        GT -> 1
        EQ -> 0