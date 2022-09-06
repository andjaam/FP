import System.Environment
import Data.Char 

-- Imena fajlova se daju kao argumenti komandne linije i učitava se reč sa stdin-a.
-- Prebrojati pojavljivanje te reči u fajlovima.

main :: IO ()
main = do 
    word <- getLine
    args <- getArgs
    texts <- sequence $ map readFile args 
    putStrLn $ process word texts

process :: String -> [String] -> String
process word texts = show
                    $ length
                    $ filter (\x -> x == word)
                    $ words
                    $ filter (\x -> isLetter x || isSpace x)
                    $ map toLower
                    $ unlines texts
