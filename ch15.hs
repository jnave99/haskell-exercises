import Data.Monoid
import Control.Monad
import Test.QuickCheck

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

{- Quickcheck -}


