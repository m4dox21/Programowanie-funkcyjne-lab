-- take 
-- drop

podziel :: Int -> [a] -> [[a]]  
podziel _ [] = []
podziel n lst = 
    let 
        -- (h, t) = splitAt n lst
        h = take n lst
        t = drop n lst 
    in 
        h:(podziel n t)

sumMaybe :: Num a => [Maybe a] -> a
-- sumMaybe [] = 0
-- sumMaybe  = addNumMaybe (sumMaybe t) h
sumMaybe lst = foldl addNumMaybe 0 lst


addNumMaybe :: Num a => a -> Maybe a -> a
addNumMaybe n Nothing = n
addNumMaybe n (Just x) = n + x


-- addMaybe :: Num a => Maybe a -> Maybe a -> a
-- addMaybe Nothing Nothing = 0
-- addMaybe Nothing (Just x) = x
-- addMaybe (Just x) Nothing  = x

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing

sumMaybe' :: Num a => [Maybe a] -> Maybe a
sumMaybe' lst = foldl addMaybe (Just 0) lst

sumMaybe'' :: Num a => [Maybe a] -> Maybe a
sumMaybe'' [] = Just 0
sumMaybe'' (Nothing:_) = Nothing
sumMaybe'' (h:t) = addMaybe h (sumMaybe'' t)

data Date = Date {
    year :: Int,
    month :: Int,
    day :: Int
} deriving Show

datesToYears :: [Date] -> [Int]
datesToYears dates = map year dates

createsDates :: Int -> [Int] -> [Int] -> [Date]
createsDates year months days = zipWith (Date year) months days

instance Eq Date where
    Date y1 m1 d1 == Date  y2 m2 d2= (y1, m1, d1) == (y2 ,m2 ,d2)

instance Ord Date where
    Date y1 m1 d1 <= Date y2 m2 d2 = 
        (y1 * 366) + (m1 * 31) + d1 <= ((y2 * 366) + (m2 * 2) + d2)