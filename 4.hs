import Prelude hiding (Maybe, Nothing, Just, Either, Left, Right)


data Maybe a = Nothing
             | Just a 
             deriving (Show, Eq)

instance Functor Maybe where 
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)


data Either a b = Left a 
                | Right b 
                deriving (Show, Eq)

instance Functor (Either a) where 
    fmap f (Left x)  = Left x 
    fmap f (Right x) = Right (f x)
