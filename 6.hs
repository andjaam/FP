import qualified System.Environment as Env
import qualified Data.Char as Ch 
import qualified Data.List as Lst

main = do
    text <- getContents
    args <- Env.getArgs
    let h = head args 
    let n = read h :: Int
    putStrLn $ process text n

process :: String -> Int -> String
process text n = let ws = words
                          $ map Ch.toLower
                          $ filter (\c -> Ch.isAlpha c || Ch.isSpace c) text
                     ws_occs = map (\l -> (head l, length l))
                               $ Lst.group 
                               $ Lst.sort ws 
                     sorted_ws_occs = reverse
                                      $ Lst.sortOn snd ws_occs 
                 in unlines $ map show $ take n sorted_ws_occs
