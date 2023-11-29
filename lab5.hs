usun_elementy_z_konca a [] = []
usun_elementy_z_konca a (h:t) =
    let
        ogon = usun_elementy_z_konca a t
    in
        if a == h then
            if ogon == [] then
                []
            else
                h:ogon
        else
            h:ogon


add_poly :: (Num a, Eq a) => [a] -> [a] -> [a]
add_poly p1 p2 = 
    usun_elementy_z_konca 0 $ add_poly_hlp p1 p2

add_poly_hlp [] lst = lst
add_poly_hlp lst [] = lst

add_poly_hlp (h1:t1) (h2:t2) = 
    (h1 + h2):(add_poly_hlp t1 t2)

mul_poly :: (Num a, Eq a) => [a] -> [a] -> [a]
mul_poly [] lst = []
mul_poly lst [] = []
-- mul_poly (h:t) p2 = 
    -- add_poly_help (0:(mul_poly t p2)) (map (* h) p2)


range1 end = [0..(end-1)]
range2 begin end = [begin..(end-1)]
range3 begin end step = 
    [begin, (begin+step)..(end-(signum step))]

-- 14.11.2023
-- show_poly :: (Show a, Num a, Eq a) => [a] -> String
-- show_poly [] = "0"

-- zipWith (\c i -> (c, i)) [0, 3, 0, 4, 5] [0..]
-- skrócny zapis 
-- zip [0, 3, 0, 4, 5] [0..]

show_monomial :: (Show a, Num a, Eq a) => (a, Int) -> String

show_monomial (1 , 1) = "x"
show_monomial (c, 0) = show c
show_monomial (c, 1) = show c ++ "x"
show_monomial (1, e) = "x^" ++ (show e)

show_monomial (c, e) = (show c) ++ "x^" ++ (show e)

-- https://wiki.haskell.org/Fold
-- https://zvon.org/other/haskell/Outputprelude/map_f.html
-- https://zvon.org/other/haskell/Outputprelude/zip_f.html
-- \x -> x /= 0

-- Skrócony zapis
show_poly' coeffs =
    foldl1  (\p m -> m ++ " + " ++ p ) $
    map show_monomial $ filter ((/=0) . fst) (zip coeffs [0..])

-- Zwykly zapis

coeffsToMonomials :: [a] -> [(a, Int)]
coeffsToMonomials coeffs = zip coeffs [0..]

removeZeroMonominals :: (Num a, Eq a) => [(a, Int)] -> [(a, Int)]  
removeZeroMonominals = filter ((/=0) . fst) 

monoList :: (Num a, Eq a) => [a] -> [(a, Int)]
monoList coeffs = removeZeroMonominals (coeffsToMonomials coeffs)

strMonoList :: (Show a, Num a, Eq a) => [a] -> [String]
strMonoList coeffs = map show_monomial $ monoList coeffs

concatStrMono :: String -> String -> String
concatStrMono ('-':m) p = p ++ " - " ++ m
concatStrMono p m = m ++ " + " ++ p

show_poly :: (Show a, Num a, Eq a) => [a] -> String
show_poly [] = "0"
show_poly coeffs =
    foldl1  concatStrMono (strMonoList coeffs)

-------------------------------------------------

startsWith :: String -> String -> Bool
startsWith []  _ = True
startsWith _ [] = False
startsWith (hp:tp) (hs:ts) =
    hp == hs && startsWith tp ts

endswith :: String -> String -> Bool
endswith "" _ = True
endswith _ "" = False
endswith suffix string =
    suffix == string || endswith suffix (tail string)

isSubstr :: String -> String -> Bool
isSubstr "" "" = True
isSubstr _ "" = False
isSubstr substr str =
    startsWith substr str || isSubstr substr ( tail str ) 


find :: String -> String -> Int
find "" "" = 0
find _ "" = -1
find substr str
    |startsWith substr str = 0
    |otherwise =
        let
            i = find substr (tail str)
        in
            if i == -1 then
                -1
        else
            i+1