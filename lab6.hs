isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'z'

lowerUpperDiff :: Int
lowerUpperDiff = (fromEnum 'z') - (fromEnum 'Z')

toLowerC :: Char -> Char
toLowerC c
    |   isUpper c = toEnum $ (fromEnum c) + lowerUpperDiff
    |   otherwise = c

toUpperC :: Char -> Char
toUpperC c 
    |   isLower c = toEnum $ (fromEnum c) - lowerUpperDiff
    |   otherwise = c

toLower :: String -> String
-- toLower s = map toLowerC s
toLower = map toLowerC

toUpper :: String -> String
-- toUpper s = map toUpperC s
toUpper = map toUpperC

-- 
isDecDigit :: Char -> Bool
isDecDigit c = c >= '0' && c <= '9'
    
isDigit' :: Char -> Bool 
isDigit' c = c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'

digitValue :: Char -> Int
digitValue c
    | isDecDigit c  = (fromEnum c) - (fromEnum '0')
    | otherwise     = (fromEnum (toLowerC c)) - (fromEnum 'a') + 10


isDigit :: Int -> Char -> Bool
isDigit b n = (isDigit' n ) && (digitValue n) < b

toInt :: Int -> String -> Int
toInt b str 
    | all (isDigit b) str =
         foldl (\n c -> b * n + (digitValue c)) 0 str
    | otherwise         = error "bledny zapis"

-- 
type Macierz a= [[a]]

poprMacierz :: [[a]] -> Bool
poprMacierz m =
    not (null m) && not (null (head m)) && 
        all ((== (length (head m))) . length) (tail m)

sumaElem :: Num a => Macierz a -> a
sumaElem = sum . (map sum)

iloczynMacierz :: (Num a) => a -> Macierz a -> Macierz a
iloczynMacierz n m = map (map ( * n)) m

macierzStala :: Int -> Int -> a -> Macierz a
macierzStala w k wartosc = replicate w (replicate k wartosc)

-- macierzStala w k = (replicate w) . (replicate k)

-- macierzZerowa :: Int -> Int -> Macierz a
-- macierzZerowa w k = macierzStala w k 0

wiersz w k = (replicate w 0) ++ [1] ++ (replicate (k - w - 1) 0)

macierzJednostkowa n = map (\w -> wiersz w n ) [0..(n-1)]
-- macierzJednostkowa n = map ('wiersz' n) [0..(n-1)]