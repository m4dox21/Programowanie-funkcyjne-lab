import System.IO
import System.Environment

zad1 :: IO()
zad1 = do
    hSetBuffering stdout NoBuffering
    putStr "nazwa pliku: "
    nazwaPliku <- getLine
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    putStr zawartosc
    hClose plik

----------------------------------------------

zad2 :: IO()
zad2 = do
    -- wypakowanie monady do 'argumenty'
    argumenty <- getArgs
    -- zapakowanie 'nazwaPliku' do monady
    let nazwaPliku = argumenty !! 0
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    putStr zawartosc
    hClose plik

----------------------------------------------
-- words zwraca liste napisow i ignoruje białe znaki - tabulacje, spacje, \n
-- lines zwraca liste napisow i ignoruje tylko spacje

sumaLiczb :: String -> Int
-- sumaLiczb napis = sum (map read (words napis))
sumaLiczb = sum . map read . words


zad3 :: IO()
zad3 = do
    -- wypakowanie monady do 'argumenty'
    argumenty <- getArgs
    -- zapakowanie 'nazwaPliku' do monady
    let nazwaPliku = argumenty !! 0
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    -- putStr zawartosc
    print $ sumaLiczb zawartosc
    hClose plik
    
----------------------------------------------
sumyLiczbZwierszy :: String -> String
sumyLiczbZwierszy = unlines . map show . map sumaLiczb . lines

zad4 :: IO()
zad4 = do
    argumenty <- getArgs
    let nazwaPliku = argumenty !! 0
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    putStr $ sumyLiczbZwierszy zawartosc
    hClose plik

----------------------------------------------
main :: IO()
main = do
    argumenty <- getArgs
    let nazwaPliku = argumenty !! 0
    let nazwaPlikuWy = argumenty !! 1
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    plikWy <- openFile nazwaPlikuWy WriteMode
    hPutStr plikWy $ sumyLiczbZwierszy zawartosc
    hClose plik
    hClose plikWy
