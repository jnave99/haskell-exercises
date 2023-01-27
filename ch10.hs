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