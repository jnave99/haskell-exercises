import Data.Char 

{- take/drop while recursive splitting -}
myWords :: String -> [String]
myWords "" = []
myWords x = takeWhile (/=' ') x : myWords(drop 1 (dropWhile (/=' ') x)) 

myLines :: String -> [String]
myLines "" = []
myLines lines = takeWhile (/='\n') lines : myLines (drop 1 (dropWhile (/='\n') lines))

separateBy :: String -> Char -> [String]
separateBy "" _ = []
separateBy lines char = takeWhile (/= char) lines : separateBy (drop 1 (dropWhile (/= char) lines)) char 

{- List composition-}
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTup = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
numTuples = length myTup

filterThrees x = filter (\y -> y `rem` 3 == 0) x
listFiltered = filterThrees [1..30]

removeWords str = filter (\x -> not (elem x removedWords)) (words str)
    where removedWords = ["the", "a", "an"]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x: xs) (y: ys) = (x, y) : (myZip xs ys) 

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith fun (a: as) (b: bs) = fun a b : myZipWith fun as bs  

capitalizeFirst :: String -> String
capitalizeFirst (x: xs) = toUpper x : xs

cipher :: String -> Int -> String
cipher "" _ = ""
cipher (y: ys) x = (chr . (+x) . ord) y : cipher ys x 

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x: xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny func (x: xs) = func x || myAny func xs

squish :: [[a]] -> [a]
squish [] = []
squish (x: xs) = x ++ squish xs