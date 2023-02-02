import Data.List 
import Data.Maybe

replaceThe :: String -> String
replaceThe str = intercalate " " $ map handleWord (words str)
    where 
        handleWord :: String -> String
        handleWord str = 
            fromMaybe "a" (notThe str)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

countTheBeforeVowel :: String -> Int 
countTheBeforeVowel str = fst (foldl handleFold (0, False) (words str))
    where 
        isVowel :: String -> Bool
        isVowel (x:xs) = elem x "aeiou"

        handleFold :: (Int, Bool) -> String -> (Int, Bool)
        handleFold (count, wasThe) str = 
            if wasThe && isVowel str then
                (count + 1, False)
            else if str == "the" then 
                (count, True)
            else 
                (count, False)

isVowel :: Char -> Bool 
isVowel ch = elem ch "aeiou"

countVowels :: String -> Integer 
countVowels str = foldl (\x y -> if isVowel y then x + 1 else x) 0 str 
        
newtype Word' =
    Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = if fst counts > snd counts then Nothing else Just (Word' str)
    where 
        counts = foldl (\x y -> if isVowel y then (fst x + 1, snd x) else (fst x, snd x + 1)) (0, 0) str :: (Int, Int)

--myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] --Why do we use maybe?
--myUnfoldr func start = [start] ++ (myUnfoldr func (func start)) 