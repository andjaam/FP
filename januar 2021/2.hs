-- Napraviti klasu Takeable t sa funkcijom takeSome :: Int -> t a -> [a].
-- Instancirati za listu, stablo i Maybe.

class Takeable t where 
    takeSome :: Int -> t a -> [a]

instance Takeable [] where 
    takeSome = take 

data Tree a = Null 
            | Node (Tree a) a (Tree a)
            deriving Show

toList :: Tree a -> [a]
toList Null         = []
toList (Node l k d) = toList l ++ [k] ++ toList d 

instance Takeable Tree where 
    takeSome n tree = take n $ toList tree

instance Takeable Maybe where 
    takeSome _ Nothing  = []
    takeSome n (Just x) = if n <= 0 then [] else [x]
