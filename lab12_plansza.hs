data Pozycja = Pozycja{
    wiersz :: Int,
    kolumna :: Int
} deriving Show

data Kierunek = Lewo | Prawo | Gora | Dol 
    deriving Show

data Pionek = Pionek{ 
    pozycja :: Pozycja,
    kierunek :: Kierunek 
} deriving Show

data Plansza = Plansza {
    pionek :: Pionek,
    zaznaczone :: [Pozycja]
}

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

krok (Plansza(Pionek (Pozycja wiersz kolmna) Gora) pola) 'f' = 
    (Plansza(Pionek (Pozycja (wiersz-1) kolumna) Gora) pola)

krok (Plansza(Pionek (Pozycja wiersz kolmna) Dol) pola) 'f' = 
    (Plansza(Pionek (Pozycja (wiersz+1) kolumna) Dol) pola)

krok (Plansza(Pionek (Pozycja wiersz kolmna) Lewo) pola) 'f' = 
    (Plansza(Pionek (Pozycja wiersz (kolumna-1)) Lewo) pola)

krok (Plansza(Pionek (Pozycja wiersz kolmna) Gora) pola) 'f' = 
    (Plansza(Pionek (Pozycja wiersz (kolumna+1)) Prawo) pola)

krok plansza _ = plansza

kroki :: Plansza -> String -> Plansza
kroki plansza litery = foldl krok plansza litery