--1
prost :: Int -> Bool
prost 1 = False
prost n = null $ listaDelilaca n
    where listaDelilaca n = [x | x <- [2..n-1], mod n x == 0]

qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (x:xs) = (qsort manji) ++ [x] ++ (qsort veci)
    where manji = [e | e <- xs, e < x]
          veci  = [e | e <- xs, e >= x]

--2
type Tacka = (Float, Float)

tacka :: Float -> Float -> Tacka
tacka x y = (x, y)

prva :: Tacka -> Float
prva (x, y) = x 

druga :: Tacka -> Float
druga (x, y) = y

transliraj :: Tacka -> Float -> Float -> Tacka 
transliraj t x y = (prva t + x, druga t + y)

--3
data StepenStudija = Osnovne
                    | Master
                    | Doktorske
                    deriving Eq

instance Show StepenStudija where
    show Osnovne   = "BSc"
    show Master    = "MSc"
    show Doktorske = "PhD"

data Student = MkStudent {
    indeks  :: String,
    ime     :: String,
    prezime :: String,
    stepen  :: StepenStudija
}

instance Show Student where
    show s = ind ++ " / " ++ ip ++ " / " ++ step 
        where ind  = indeks s 
              ip   = (ime s) ++ " " ++ (prezime s)
              step = show $ stepen s 

instance Eq Student where 
    s1 == s2 = (indeks s1) == (indeks s2)
