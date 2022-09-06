import Data.Char
import Test.QuickCheck

--1
mySum :: Num a => [a] -> a
mySum xs = foldl (+) 0 xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred xs = reverse $ foldl (\acc x -> if pred x then x:acc else acc) [] xs 

myFilterR :: (a -> Bool) -> [a] -> [a]
myFilterR pred xs = foldr (\x acc -> if pred x then x:acc else acc) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap pred xs = reverse $ foldl (\acc x -> (pred x):acc) [] xs

myMapR :: (a -> b) -> [a] -> [b]
myMapR pred xs = foldr (\x acc -> (pred x):acc) [] xs 

--2
takeWord :: String -> String
takeWord s = takeWhile (\x -> not $ isSpace x) $ dropWhile isSpace s 

filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex pred xs = map snd $ filter pairPred pairs
    where pairs         = zip [0..] xs 
          pairPred pair = pred $ fst pair

applyFunctions :: a -> [a -> b] -> [b]
applyFunctions x fs = map ($x) fs

--3
newtype Ring t = MkRing [t]

instance Show t => Show (Ring t) where 
    show (MkRing xs) = "ring (" ++ show' xs ++ ")"
        where 
            show' []     = ""
            show' [x]    = show x 
            show' (x:xs) = show x ++ "," ++ show' xs

fromList :: [a] -> Ring a 
fromList xs = MkRing xs 

toList :: Ring a -> [a]
toList (MkRing xs) = xs 

prop_conversions :: Eq a => [a] -> Bool
prop_conversions xs = xs == (toList $ fromList xs)
