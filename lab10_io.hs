import System.IO
-- ghc nazwa_pliku.hs
-- ./nazwa_pliku
main :: IO()
-- main = putStrLn "Hello world"
-- main = getLine >>= (\line -> print (length line))
-- main = do 
--     line <- getLine
--     let lineLength = length line
--     print (lineLength)


main2 = do
    hSetBuffering stdout NoBuffering
    putStrLn "imię: "
    imie <- getLine
    putStrLn "nazwisko: "
    nazwisko <- getLine
    putStrLn $ "Witaj " ++ imie ++ " " ++ nazwisko ++ "!"


-- main = do
--     strNum <- getLine
--     let number = read strNum :: Int
--     print number


main3 = do
    hSetBuffering stdout NoBuffering
    putStrLn "number 1: "
    line1 <- getLine
    let numLine1 = read line1 :: Int
    putStrLn "number 2: "
    line2 <- getLine   
    let numLine2 = read line2 :: Int
    let suma = numLine1 + numLine2
    putStrLn $ "suma: " ++ (show suma)
    let iloczyn = numLine1 * numLine2
    putStrLn $ "iloczyn: " ++ (show iloczyn)

-- operowanie na plikach
wczytajZawartosc :: FilePath -> IO (String, Handle)
wczytajZawartosc nazwaPliku = do
    plik <- openFile nazwaPliku ReadMode
    zawartosc <- hGetContents plik
    hClose plik
    return (zawartosc, plik)

main = do
    hSetBuffering stdout NoBuffering
    putStr "nazwa pliku: "
    nazwaPliku <- getLine
    (zawartosc, plik) <- wczytajZawartosc nazwaPliku
    putStr zawartosc
    hClose plik