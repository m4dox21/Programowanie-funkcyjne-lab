data Pozycja = Pozycja{
    wiersz :: Int,
    kolumna :: Int
} deriving (Show, Eq)

data Kierunek = Lewo | Prawo | Gora | Dol 
    deriving (Show, Eq)

data Pionek = Pionek{ 
    pozycja :: Pozycja,
    kierunek :: Kierunek 
} deriving Show

data Plansza = Plansza {
    pionek :: Pionek,
    zaznaczone :: [Pozycja]
} deriving Show

nowaPlansza :: Plansza
nowaPlansza = Plansza (Pionek (Pozycja 0 0) Prawo) []

naPlanszy :: Pozycja -> Bool
naPlanszy (Pozycja wiersz kolumna) = 
    wiersz >= 0 && kolumna >= 0

krok :: Plansza -> Char -> Plansza
krok pl@(Plansza pi@(Pionek pozycja kierunek) pola) 'm' 
    | naPlanszy pozycja = Plansza pi (pozycja:pola) 
    | otherwise         = pl

krok (Plansza(Pionek pozycja _) pola) 'n' = 
    (Plansza(Pionek pozycja Gora) pola)

krok (Plansza(Pionek pozycja _) pola) 's' = 
    (Plansza(Pionek pozycja Dol) pola)

krok (Plansza(Pionek pozycja _) pola) 'w' = 
    (Plansza(Pionek pozycja Lewo) pola)

krok (Plansza(Pionek pozycja _) pola) 'e' = 
    (Plansza(Pionek pozycja Prawo) pola)

krok (Plansza(Pionek (Pozycja wiersz kolumna) Gora) pola) 'f' = 
    (Plansza(Pionek (Pozycja (wiersz - 1) kolumna) Gora) pola)

krok (Plansza(Pionek (Pozycja wiersz kolumna) Dol) pola) 'f' = 
    (Plansza(Pionek (Pozycja (wiersz + 1) kolumna) Dol) pola)

krok (Plansza(Pionek (Pozycja wiersz kolumna) Lewo) pola) 'f' = 
    (Plansza(Pionek (Pozycja wiersz (kolumna - 1)) Lewo) pola)

krok (Plansza(Pionek (Pozycja wiersz kolumna) Prawo) pola) 'f' = 
    (Plansza(Pionek (Pozycja wiersz (kolumna + 1)) Prawo) pola)

krok plansza _ = plansza

kroki :: Plansza -> String -> Plansza
kroki plansza litery = foldl krok plansza litery

pokaz:: Plansza -> String
pokaz plansza = (pokazWspolrzedne plansza) ++ "\n" ++ (pokazPlansze plansza)

pokazWspolrzedne :: Plansza -> String
pokazWspolrzedne (Plansza (Pionek (Pozycja w k) _) _) =
    "(" ++ (show k) ++ ", " ++ (show w) ++ ")"

pokazPlansze :: Plansza -> String
pokazPlansze = unlines . wierszePlanszy

pokazKierunek :: Kierunek -> Char
pokazKierunek Gora = '^'
pokazKierunek Lewo = '<'
pokazKierunek Prawo = '>'
pokazKierunek Dol = 'v'

pokazPole :: Plansza -> Pozycja -> Char
pokazPole (Plansza (Pionek pozPionka k) z) pozPola = 
    if pozPionka == pozPola then
        pokazKierunek k
    else
        if naPlanszy pozPola then
            if (elem pozPola z) then '.' else '#'
        else
            '~'

pokazWiersz :: Plansza -> Int -> String
pokazWiersz plansza w =
    let 
        k = kolumna (pozycja (pionek plansza))
    in
        map (\kol -> 
            pokazPole plansza (Pozycja w kol)) [k - 10 .. k + 10]

wierszePlanszy :: Plansza -> [String]
wierszePlanszy plansza =
    let
        w = wiersz (pozycja (pionek plansza))
    in
        map (pokazWiersz plansza) [w - 5 .. w + 5]

main :: IO ()
main = mainZPlansza nowaPlansza

mainZPlansza :: Plansza -> IO ()
mainZPlansza plansza = do
    putStr (pokaz plansza)
    polecenia <- getLine
    if elem 'q' polecenia then
        return ()
    else
        mainZPlansza (kroki plansza polecenia)
