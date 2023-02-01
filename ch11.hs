import Data.Char
import Data.List
import Data.Maybe

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car manu price) = True 
isCar _ = False 

isPlane :: Vehicle -> Bool 
isPlane (Plane airline) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars vehicles = map (\x -> isCar x) vehicles

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manu price) = Just manu
getManu _ = Nothing

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

type Gardener = String

data Garden = Gardenia Gardener 
    | Daisy Gardener
    | Rose Gardener
    | Lilac Gardener
    deriving Show

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
        , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [(Programmer {os = x, lang = y}) | x <- allOperatingSystems, y <- allLanguages]

{- Binary Tree stuff-}

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

insert' :: Ord a
    => a
    -> BinaryTree a
    -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree func (Node left val right) = Node (mapTree func left) (func val) (mapTree func right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left val right) = [val] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = (inorder left) ++ [val] ++ (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left val right) = (postorder left) ++ (postorder right) ++ [val]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b)
    -> b
    -> BinaryTree a 
    -> b
foldTree _ start Leaf = start
foldTree func start (Node left val right) = foldTree func (foldTree func (func val start) left) right

{- Cipher -}
 
cipherHelper :: String -> String -> Int -> (Int -> Int -> Int) -> String
cipherHelper "" _ _ _ = ""
cipherHelper (' ': xs) key index action = " " ++ cipherHelper xs key index action
cipherHelper (x : xs) key index action = [chr (zeroTo26Num + 65)] ++ cipherHelper xs key (index + 1) action
    where 
        keyIndex = mod index keyLength :: Int
        keyLength = length key :: Int
                
        zeroTo26Num :: Int
        zeroTo26Num = mod ((action (ord x - 65) (ord (head (drop keyIndex key)))) + 65) 26

encode :: String -> String -> String
encode val key = cipherHelper (map toUpper val) (map toUpper key) 0 (+)
        
decode :: String -> String -> String
decode val key = cipherHelper (map toUpper val) (map toUpper key) 0 (-)

{- As-patterns -}

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf pattern str@(x:xs) = take (length pattern) str == pattern || isSubseqOf pattern xs 

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map format (words str)
    where 
        format :: String -> (String, String)
        format t@(x:xs) = (t, (toUpper x) : xs)

{- Phone -}

type DaPhone = [(String, Char)]

phone = [("abc", '2'), ("def", '3'), ("ghi", '4'), ("jkl", '5'), ("mno", '6'), ("pqrs", '7'), ("tuv", '8'), ("wxyz", '9'), (" ", '0'), (".,", '#')] :: DaPhone

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = capitalPress ++ charPress 
     where 
        capitalPress :: [(Digit, Presses)]
        capitalPress = 
            if char == toLower char then 
                []
            else 
                [('*', 1)]
        
        charPress :: [(Digit, Presses)]
        charPress = charPressHelper phone (toLower char)

        charPressHelper :: DaPhone -> Char -> [(Digit, Presses)]
        charPressHelper [] _ = [] :: [(Digit, Presses)]
        charPressHelper (x:xs) char = 
            if index /= -1 then 
                [(snd x, index + 1)]
            else 
                charPressHelper xs char
            
            where 
                index = fromMaybe (-1) $ elemIndex char (fst x) 

type PressList = [(Digit, Presses)]

cellPhonesDead :: DaPhone -> String -> PressList
cellPhonesDead _ [] = []
cellPhonesDead phone (x:xs) = (reverseTaps phone x) ++ (cellPhonesDead phone xs)

fingerTaps :: PressList -> Presses
fingerTaps li = foldl (+) 0 (map (\(di, p) -> p) li)

mostPopularLetter :: String -> (Char, Int)
mostPopularLetter str = foldl (\x y -> if snd y > snd x then y else x) (' ', 0) (buildList str [])
    where
        buildList :: String -> PressList -> PressList
        buildList [] li = li
        buildList (x:xs) li = buildList xs (updateList li x)

        updateList :: PressList -> Char -> PressList
        updateList [] char = [(char, 1)]
        updateList (x:xs) char = 
            if fst x == char then 
                (fst x, snd x + 1) : xs
            else 
                x : (updateList xs char)

coolestLtr :: [String] -> Char
coolestLtr [] = ' '
coolestLtr li = fst $ foldl (\x y -> if snd x > snd y then x else y) (' ', 0) (coolestHelper li)
    where 
        coolestHelper :: [String] -> PressList
        coolestHelper [] = []
        coolestHelper (x:xs) = (mostPopularLetter x) : (coolestHelper xs)  