import Prelude hiding (Maybe, Nothing, Just)
import Control.Applicative
import Data.Char

--1
data Maybe a = Nothing
             | Just a 
             deriving (Show, Eq)

instance Functor Maybe where 
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where 
    pure x = Just x 

    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing

ourSeq :: (Applicative f) => [f a] -> f [a]
ourSeq []     = pure []
ourSeq (x:xs) = liftA2 (:) x (ourSeq xs)

--2
instance Alternative Maybe where 
    empty = Nothing

    Nothing  <|> x = x 
    (Just x) <|> _ = Just x 

instance Monad Maybe where
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x

makeBold :: String -> Maybe String
makeBold s = Just $ "<b>" ++ s ++ "</b>"

stringToUpper :: String -> Maybe String
stringToUpper s = Just $ fmap toUpper s 

shout :: String -> Maybe String
shout s = stringToUpper s >>= makeBold

--3
type Birds = Int 
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r) = if abs (l+n - r) < 3 then Just (l+n, r) else Nothing

landRight :: Birds -> Pole -> Maybe Pole 
landRight n (l, r) = if abs (r+n - l) < 3 then Just (l, r+n) else Nothing

walk = do 
    start  <- Just (0, 0)
    first  <- landLeft 2 start
    second <- landRight 3 first 
    third  <- landLeft 2 second
    landRight 5 third 
