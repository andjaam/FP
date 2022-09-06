import qualified System.Environment as Env
import qualified Control.Monad as Mnd 
import qualified Data.List as Lst 
import qualified System.Process as Pro 

--1
main' :: IO ()
main' = do 
    args <- Env.getArgs
    Mnd.forM_ args (\arg -> do 
                            putStrLn "Unesi nesto"
                            nesto <- getLine
                            putStrLn $ "Uneto: " ++ nesto)

--2
main :: IO ()
main = do 
    args <- Env.getArgs
    mapM_ process args 

process :: FilePath -> IO ()
process path = if isHaskellFile path then realProcess path else putStrLn "pogresan fajl"
               where isHaskellFile path = drop (length path - 3) path == ".hs"

realProcess :: FilePath -> IO ()
realProcess path = do 
    content <- readFile path 
    let tests = getTests content
    if null tests then putStrLn "nema testova" 
                  else execTests tests path 

getTests :: String -> [String]
getTests content = Lst.nub
                   $ filter (\x -> take 5 x == "prop_")
                   $ map fst
                   $ map head 
                   $ map lex
                   $ lines content

execTests :: [String] -> String -> IO ()
execTests tests path = do 
    writeFile "s" (":l " ++ path ++ "\n" ++ (concatMap createCommand tests))
    exitCode <- Pro.system "ghci < s"
    putStrLn $ "exit code: " ++ show exitCode

createCommand :: String -> String
createCommand test = "putStrLn " ++ show test ++ "\nquickCheck " ++ test ++ "\n"
