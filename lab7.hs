type Row a = [a]
type Matrix a = [Row a]

-- wiersz -> kolumna
matrixFromFunction :: Int -> Int -> (Int -> Int -> a) -> Matrix a
matrixFromFunction rows cols func = 
    map (\row -> rowFromFunction row cols func) [0.. (rows - 1)] 

-- Index wiersza -> Liczba kolumn
rowFromFunction :: Int -> Int -> (Int -> Int -> a) -> Row a
rowFromFunction row_index cols func =
    map (func row_index) [0..(cols - 1)]

-- 
matrixFromValue :: Int -> Int -> a -> Matrix a
matrixFromValue rows cols a =
    matrixFromFunction rows cols (\_ _ -> a) 

idMatrix :: Num a => Int -> Matrix a
idMatrix size = matrixFromFunction size size (\row col -> if (row == col) then 1 else 0)

-- 
apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = f (apply (n - 1) f x)

-- 
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:(iterate' f(f x))

-- 
fib :: Int -> Integer
fib n = fst $ apply n (\(x, y) -> (y, x + y)) (0, 1)

-- 
fact n = product [1..n]

fact_list' = map fact [0..]

fact_list_hlp n fact_n 
    = fact_n : (fact_list_hlp (n+1) ((n+1) * fact_n))

fact_list'' = fact_list_hlp 0 1

fact_list''' = 1 : (zipWith (*) [1..] fact_list''')

fib_list = 0:1:(zipWith (+) fib_list (tail fib_list))

fib_list' = map fst $ iterate (\(x, y) -> (y, x+y)) (0, 1)

pierwsza 1 = False
pierwsza n = 
    all (\d -> mod n d /=0) (takeWhile (\d -> d * d <= n) [2..])