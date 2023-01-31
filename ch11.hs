import Data.Char

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

{- BInary Tree stuff-}

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
