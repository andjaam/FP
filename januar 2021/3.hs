-- Definisati binarno stablo i instancirati Eq, Show (infiksni zapis) i Functor.

data Tree a = Null 
            | Node (Tree a) a (Tree a)

instance Eq a => Eq (Tree a) where 
    Null == Null = True 
    Null == _    = False 
    _    == Null = False
    (Node l1 k1 d1) == (Node l2 k2 d2) = l1 == l2 && k1 == k2 && d1 == d2

instance Show a => Show (Tree a) where 
    show tree = "[" ++ show' tree ++ "]"
        where 
            show' Null         = " "
            show' (Node l k d) = show' l ++ show k ++ show' d

instance Functor Tree where 
    fmap f Null         = Null
    fmap f (Node l k d) = Node (fmap f l) (f k) (fmap f d) 
