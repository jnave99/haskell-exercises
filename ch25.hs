{- # LANGUAGE InstanceSigs # -}

import Control.Applicative

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

-- instance (Applicative f, Applicative g) => Applicative (Compose f g) where
--     pure x = Compose (pure (pure x))
--     Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x
    
-- instance (Applicative f, Applicative g) => Applicative (Compose f g) where
--     pure :: a -> Compose f g a
--     pure a = Compose (pure (pure a))
--     (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
--     (Compose f) <*> (Compose a) = 
--         Compose $ liftA2 apply f a 
--         where 
--             apply :: (Applicative a, Applicative b, Applicative c) => a -> b -> c
--            apply a b = a <*> b 


class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where 
    bimap f f' (Deux a b) = Deux (f a) (f' b)

data Const' a b = Const' a deriving (Eq, Show)

instance Bifunctor Const' where 
    bimap f f' (Const' a) = Const' (f a)