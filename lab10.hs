data Fraction = Integer :/ Integer

infixl 7 :/

(//) :: Integer -> Integer -> Fraction
_ // 0 = error "Dzielenie przez zero"
0 // _ = 0 :/ 1
a // b
    | b < 0     = (-a) // (-b)
    | otherwise =
        let
            d = gcd a b
        in
            (div a d) :/ (div b d)


infixl 7 //

instance Show Fraction where
    show (a :/ b) = (show a) ++ "/" ++ (show b)

instance Eq Fraction where
    (a1 :/ b1) == (a2 :/ b2) = a1 * b2 == b1 * a2

instance Ord Fraction where
    (a1 :/ b1) <= (a2 :/ b2) =
        let
            p1 :/ q1 = a1 // b1
            p2 :/ q2 = a2 // b2
        in
            p1 * q2 <= p2 * q1

instance Num Fraction where
    (a1 :/ b1) + (a2 :/ b2) =
        (a1 * b2 + a2 * b1) // (b1 * b2)

    (a1 :/ b1) * (a2 :/ b2) =
        a1 * a2 // (b1 * b2)

    negate (a :/ b) = (-a) // b

    abs (a :/ b) = (abs a) // (abs b)

    signum f
        | f < 0      = -1
        | f == 0     = 0
        | otherwise  = 1

    fromInteger n = n :/ 1

-- 09.01.2024

data UlamekLancuchowy a = 
    Wartosc a | Suma a (UlamekLancuchowy a)
    deriving (Show)

wartoscUl :: UlamekLancuchowy Integer -> Fraction
wartoscUl (Wartosc n) = fromInteger n
wartoscUl (Suma n ul) =
     fromInteger n + odwrotnosc (wartoscUl ul)

wartoscDouble :: UlamekLancuchowy Integer -> Double
wartoscDouble (Wartosc n) = fromInteger n
wartoscDouble (Suma n ul) =
    fromInteger n + 1.0 + (wartoscDouble ul)
    
listaNaul :: [a] -> UlamekLancuchowy a
listaNaul [x] = Wartosc x
listaNaul (h:t) = Suma h (listaNaul t)

odwrotnosc :: Fraction -> Fraction
odwrotnosc (p :/ q) = q // p