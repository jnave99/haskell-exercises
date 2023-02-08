import Data.Monoid
import Control.Monad

{- Monoids -}

data Optional a =
    Nada
  | Only a 
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where 
    Nada <> Nada = Nada
    (Only a) <> Nada = Only a 
    Nada <> (Only a) = Only a 
    (Only a) <> (Only b) = Only (a <> b)

 {- Mad Libs -}

type Adjective = String 
type Adverb = String 
type Noun = String 
type Exclamation = String 

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = 
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said"
    , adv, " as he jumped into his car", noun 
    , " and drove off with his " 
    , adj, " wife"]

{- Monoid Instances -}

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (Identity a) <> (Identity b) = Identity (a <> b)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where 
    (BoolConj b) <> (BoolConj b') = BoolConj (b && b')

newtype BoolDisj = BoolDisj Bool  deriving (Eq, Show)

instance Semigroup BoolDisj where 
    (BoolDisj b) <> (BoolDisj b') = BoolDisj (b || b')

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where 
    Fst a <> Fst b = Fst b 
    Snd a <> _ = Snd a 
    Fst a <> Snd b = Snd b

instance Monoid BoolConj where 
    mempty = BoolConj True