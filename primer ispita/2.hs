import System.Environment

-- Napisati program koji iz fajla čija je putanja data kao argument 
-- komandne linije, čita brojeve jedan po jedan, svaki u zasebnom redu.
-- Program ispisuje sve brojeve poravnate udesno u odnosu na najduži
-- broj, a zatim podvlaku i sumu svih brojeva
--
-- Na primer, za fajl:
--
-- 1020
-- 10
-- 3
-- 8000
-- 2000
-- 1
--
-- Treba ispisati:
--
--      1020
--        10
--         3
--      8000
--      2000
--         1
--     -----
--     11034

main :: IO ()
main = do 
    (arg:_) <- getArgs
    tekst <- readFile arg
    let zbir = process tekst 
    let n    = length zbir
    putStr   $ ispis tekst n
    putStrLn $ replicate n ' ' ++ replicate n '-'
    putStrLn $ replicate n ' ' ++ zbir

process :: String -> String
process tekst = show
                $ sum
                $ map (\x -> read x :: Int)
                $ lines tekst

ispis :: String -> Int -> String
ispis tekst n = unlines
                $ map (\x -> replicate n ' ' ++ replicate (n - length x) ' ' ++ x)
                $ lines tekst 
