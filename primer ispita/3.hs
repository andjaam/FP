import Test.QuickCheck

-- Definisati strukturu podataka `Stek a`, gde je `a`
-- proizvoljan tip. Stek moze biti prazan ili sadrzati
-- proizvoljan broj elemenata tipa `a`.
data Stek a = Empty
            | MkStek a (Stek a)

-- Kreirati funkcije `fromList` i `toList` koje konvertuju
-- [a] u `Stek a`, i `Stek a` u `[a]`, redom.
fromList :: [a] -> Stek a 
fromList [] = Empty
fromList xs = MkStek (last xs) (fromList $ init xs)

toList :: Stek a -> [a]
toList Empty           = []
toList (MkStek x stek) = (toList stek) ++ [x]

-- Napisati QuickCheck test za funkcije `fromList` i `toList`.
prop_conversions :: Eq a => [a] -> Bool
prop_conversions xs = xs == (toList $ fromList xs)

-- Instancirati Show nad `Stek a` (pretpostaviti da je
-- `a` u klasi Show) tako da se skup ispise u formatu:
--       < dno, ... , vrh >
-- (vrh i dno su elementi steka)
-- Za prazan stek:  </>
instance Show a => Show (Stek a) where 
    show s = "< " ++ show' s ++ " >"
        where 
            show' Empty            = "/"
            show' (MkStek x Empty) = show x
            show' (MkStek x stek)  = show' stek ++ ", " ++ show x

-- Instancirati Functor nad `Stek a`
instance Functor Stek where 
    fmap f Empty           = Empty
    fmap f (MkStek x stek) = MkStek (f x) (fmap f stek)
