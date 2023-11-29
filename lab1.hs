f  :: Int -> Int
f x = x + 1

g :: Num a => a -> a
g x = x

add :: Num a => a -> ( a -> a )
add x y = x + y

add3 y = 3 + y

abs' x = 
    if x < 0 then
         (-x)
        else x

-- Zad 1
-- 1.1
kw_sum a b = a ^ 2 + b ^ 2
-- 1.2
sum_kw a b = (a + b)^2
-- 1.3
f1_3 a b c d = a * b + c * d
-- 1.4
f1_4 a b c d = (a - b )/( c + d)
-- 1.5
f1_5 a b d = (a/b)*d
-- 1.6
f1_6 a = a/4
-- 1.7
f1_7 a b = (a/b)^3
-- 1.8
f1_8 a b = sqrt(abs'(a-b))

-- Zad 2

zad2' cj cd =
    if cj < 2 || cj > 4 then
        "złotych"
    else
        if cd == 1 then
            "złotych"
        else
            "złote"

zad2 n =
    if n == 1 then
        "złoty"
    else
        zad2' (mod n 10) (div (mod n 100) 10)



-- Zad 3

zad3' gr =
    if gr == 1 then
        "grosz"
    else
        if gr >= 2 && gr <= 4 then
            "grosze"
        else
            "groszy"


    