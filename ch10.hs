import Data.Time

x = foldl (\b a -> (take 1 b ++ a)) "" ["Pizza", "Apple", "Banana"]

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 12
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    ]

filterNonDates :: DatabaseItem -> Bool
filterNonDates (DbDate _)= True
filterNonDates _ = False

extractDate :: DatabaseItem -> UTCTime
extractDate (DbDate time) = time

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = map extractDate (filter filterNonDates items)

foldDbHelper :: [UTCTime] -> DatabaseItem -> [UTCTime]
foldDbHelper arr (DbDate date') = arr ++ [date']
foldDbHelper arr _ = arr

foldDbDate :: [DatabaseItem] -> [UTCTime]
foldDbDate items = foldl foldDbHelper [] items

foldDbNumber :: [DatabaseItem] -> [Integer]
foldDbNumber items = foldr ((++) . foldHelper) [] items 
    where 
        foldHelper :: DatabaseItem -> [Integer]
        foldHelper item = case item of
            DbNumber n -> [n]
            _ -> []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr (\x y -> if x > y then x else y) (head filteredItems) (tail filteredItems) 
    where 
        filteredItems = ((foldDbDate) items)

sumDb :: [DatabaseItem] -> Integer 
sumDb items = foldr (\x y -> x + y) 0 (foldDbNumber items)

avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral (sumDb items) / fromIntegral (length $ foldDbNumber items)

myReverse :: [a] -> [a]
myReverse items = foldl (\x y -> flip (:) x y) [] items

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter func items = foldl (\x y -> x ++ filterVal y) [] items
    where 
        filterVal val = case func val of
            True -> [val]
            False -> []