-- Definisati tip podataka `Point2D` koji se može konstruisati 
-- putem konstruktora `MkPoint2D` i koji se sastoji od atributa:
--  * `x` (tipa `Double`) koje predstavlja x-koordinatu tačke 
--  * `y` (tipa `Double`) koje predstavlja y-koordinatu tačke
data Point2D = MkPoint2D {
    x :: Double,
    y :: Double
} deriving Show

-- Definisati tip podataka `Rectangle` koji se može konstruisati 
-- putem konstruktora `MkRectangle` i koji se sastoji od atributa:
--  * `bott_left` (tipa `Point2D`) koje predstavlja teme A pravougaonika 
--  * `top_right` (tipa `Point2D`) koje predstavlja teme C pravougaonika
-- (pretpostaviti da su stranice pravougaonika paralelene koordinatnim osama)
--
--             D     (C)
--
--            (A)     B
data Rectangle = MkRectangle {
    bott_left :: Point2D,
    top_right :: Point2D
} deriving Show 

-- Definisati klasu `Traversible a` koja sadrži funkciju
-- `traverse :: a -> [Point2D]`
class Traversible a where
    traverse' :: a -> [Point2D]

-- Instancirati `Traversible` za `Rectangle` tako da funkcija `traverse`
-- vrati listu [A,B,C,D]
instance Traversible Rectangle where
    traverse' (MkRectangle (MkPoint2D xa ya) (MkPoint2D xc yc)) = [MkPoint2D xa ya, 
                                                                   MkPoint2D xc ya,
                                                                   MkPoint2D xc yc,
                                                                   MkPoint2D xa yc]

-- Definisati funkciju `rotTrans rect` koja rotira pravougaonik za 90 stepeni
-- u smeru kazaljke na satu ali tako da teme B dođe na mesto temena A 
-- (kao na slici ispod) i vraća dobijeni pravougaonik:
--
--            | D     (C)         | A   (D)
--            |             ->    |
--            |(A)     B          |(B)   C
--            ````````````        ``````````
rotTrans :: Rectangle -> Rectangle
rotTrans (MkRectangle (MkPoint2D xa ya) (MkPoint2D xc yc)) = MkRectangle (MkPoint2D xc ya) (MkPoint2D xa yc)
