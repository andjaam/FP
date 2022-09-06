--1
class Shape a where 
    povrsina :: a -> Float
    obim     :: a -> Float

newtype Circle = MkCircle {
    r :: Float
}

instance Shape Circle where 
    povrsina x = r x * r x * pi
    obim x     = 2   * r x * pi

instance Show Circle where 
    show x = "circle (r = " ++ show (r x) ++ ")"

data Rectangle = MkRectangle {
    a :: Float,
    b :: Float
}

instance Shape Rectangle where 
    povrsina x = a x * b x
    obim x     = 2*(a x + b x)

instance Show Rectangle where 
    show x = "rectangle (a = " ++ show (a x) ++ ", b = " ++ show (b x) ++ ")"

--2
class YesNo a where 
    yesno :: a -> Bool

instance YesNo (Maybe a) where 
    yesno Nothing  = False
    yesno (Just x) = True

instance YesNo Int where 
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where 
    yesno [] = False
    yesno _  = True 

instance YesNo Bool where 
    yesno = id 

--3
data MyList a = Empty
              | MkMyList a (MyList a)

instance Show a => Show (MyList a) where 
    show lst = "[" ++ show' lst ++ "]"
        where 
            show' Empty              = ""
            show' (MkMyList x Empty) = show x 
            show' (MkMyList x xs)    = show x ++ ", " ++ show' xs 

instance Functor MyList where 
    fmap f Empty           = Empty
    fmap f (MkMyList x xs) = MkMyList (f x) (fmap f xs)

instance Foldable MyList where 
    foldr f acc Empty           = acc 
    foldr f acc (MkMyList x xs) = f x (foldr f acc xs)
