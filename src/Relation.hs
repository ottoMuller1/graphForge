module Relation where

---------------------type declaration---------------------
data Relation a = a :--- a | a :>-- a | a :>-< a
data Weight a b = ( Relation a ) :^: b

-------------------------instances------------------------
-- Shows
instance Show a => Show ( Relation a ) where
    show ( a :--- b ) = show a ++ " --- " ++ show b
    show ( a :>-- b ) = show a ++ " >-- " ++ show b
    show ( a :>-< b ) = show a ++ " >-< " ++ show b

instance ( Show a, Show b ) => Show ( Weight a b ) where
    show ( rel :^: w ) = show rel ++ " weighs " ++ show w

-- Semigroups
instance Eq a => Semigroup ( Relation a ) where
    left@( a :--- b ) <> ( c :--- d )
        | b == c = a :--- d
        | b == d = a :--- c
        | a == c = b :--- d
        | a == d = b :--- c
        | otherwise = left
    left@( a :--- b ) <> ( c :>-- d )
        | b == c = a :>-- d
        | a == c = b :>-- d
        | otherwise = left
    left@( a :--- b ) <> ( c :>-< d )
        | b == c = a :>-- d
        | b == d = a :>-- c
        | a == c = b :>-- d
        | a == d = b :>-- c
        | otherwise = left
    left@( a :>-- b ) <> ( c :--- d )
        | b == c = a :>-- d
        | b == d = a :>-- c
        | otherwise = left
    left@( a :>-- b ) <> ( c :>-- d )
        | b == c = a :>-- d
        | otherwise = left
    left@( a :>-- b ) <> ( c :>-< d )
        | b == c = a :>-< d
        | b == d = a :>-< c
        | otherwise = left
    left@( a :>-< b ) <> ( c :--- d )
        | b == c = a :>-- d
        | b == d = a :>-- c
        | a == c = b :>-- d
        | a == d = b :>-- c
        | otherwise = left
    left@( a :>-< b ) <> ( c :>-- d )
        | b == c = a :>-- d
        | a == c = b :>-- d
        | otherwise = left
    left@( a :>-< b ) <> ( c :>-< d )
        | b == c = a :>-< d
        | b == d = a :>-< c
        | a == c = b :>-< d
        | a == d = b :>-< c
        | otherwise = left

instance ( Eq a, Semigroup b ) => Semigroup ( Weight a b ) where
    ( rel1 :^: w1 ) <> ( rel2 :^: w2 ) = ( rel1 <> rel2 ) :^: ( w1 <> w2 )

-- Functors
instance Functor Relation where
    fmap f ( a :--- b ) = f a :--- f b
    fmap f ( a :>-- b ) = f a :>-- f b
    fmap f ( a :>-< b ) = f a :>-< f b

instance Functor ( Weight a ) where
    fmap f ( rel :^: w ) = rel :^: f w

-- Apps
instance Applicative Relation where
    pure a = a :--- a
    ( f :--- g ) <*> ( a :--- b ) = f a :--- g b
    ( f :--- g ) <*> ( a :>-- b ) = f a :>-- g b
    ( f :--- g ) <*> ( a :>-< b ) = f a :>-< g b
    ( f :>-- g ) <*> ( a :--- b ) = f a :>-- g b
    ( f :>-- g ) <*> ( a :>-- b ) = f a :>-- g b
    ( f :>-- g ) <*> ( a :>-< b ) = f a :>-< g b
    ( f :>-< g ) <*> ( a :--- b ) = f a :>-< g b
    ( f :>-< g ) <*> ( a :>-- b ) = f a :>-< g b
    ( f :>-< g ) <*> ( a :>-< b ) = f a :>-< g b

-- Monads
instance Monad Relation where
    return = pure
    ( _ :--- b ) >>= f = f b
    ( _ :>-- b ) >>= f = f b
    ( _ :>-< b ) >>= f = f b
            
-- Eqs
instance Eq a => Eq ( Relation a ) where
    ( a :--- b ) == ( c :--- d ) = a == c && b == d || a == d && b == c
    ( a :>-- b ) == ( c :>-- d ) = a == c && b == d
    ( a :>-< b ) == ( c :>-< d ) = a == c && b == d || a == d && b == c
    a == b = False

instance ( Eq a, Eq b ) => Eq ( Weight a b ) where
    ( rel1 :^: w1 ) == ( rel2 :^: w2 ) = ( rel1 == rel2 ) && ( w1 == w2 )

-- Ords
instance Eq a => Ord ( Relation a ) where
    compare ( a :--- b ) ( c :--- d )
        | a == c || a == d || b == c || b == d = GT
        | otherwise = EQ
    compare ( a :--- b ) ( c :>-- d )
        | a == c || b == c = GT
        | a == d || b == d = LT
        | otherwise = EQ
    compare ( a :--- b ) ( c :>-< d )
        | a == c || a == d || b == c || b == d = GT
        | otherwise = EQ
    compare ( a :>-- b ) ( c :--- d )
        | b == c || b == d = GT
        | a == c || a == d = LT
        | otherwise = EQ
    compare ( a :>-- b ) ( c :>-- d )
        | b == c = GT
        | a == d = LT
        | otherwise = EQ
    compare ( a :>-- b ) ( c :>-< d )
        | b == c || b == d = GT
        | a == c || a == d = LT
        | otherwise = EQ
    compare ( a :>-< b ) ( c :--- d )
        | a == c || a == d || b == c || b == d = GT
        | otherwise = EQ
    compare ( a :>-< b ) ( c :>-- d )
        | a == c || b == c = GT
        | a == d || b == d = LT
        | otherwise = EQ
    compare ( a :>-< b ) ( c :>-< d )
        | a == c || a == d || b == c || b == d = GT
        | otherwise = EQ

instance ( Eq a, Ord b ) => Ord ( Weight a b ) where
    ( rel1 :^: w1 ) <= ( rel2 :^: w2 ) = w1 <= w2

-- Folds
instance Foldable Relation where
    foldr f e ( a :--- b ) = f a ( f b e )
    foldr f e ( a :>-- b ) = f a ( f b e )
    foldr f e ( a :>-< b ) = f a ( f b e )

------------------------------features-------------------------------------
-- up level of orientation
upLevel :: Relation a -> Relation a
upLevel ( a :--- b ) = a :>-- b
upLevel ( a :>-- b ) = a :>-< b
upLevel ( a :>-< b ) = a :>-< b

-- down level of orientation
downLevel :: Relation a -> Relation a
downLevel ( a :--- b ) = a :--- b
downLevel ( a :>-- b ) = a :--- b
downLevel ( a :>-< b ) = a :>-- b

-- getting reltion from weight
getRelation :: Weight a b -> Relation a
getRelation ( rel :^: _ ) = rel

-- reflexive
isRef :: Eq a => Relation a -> Bool
isRef ( a :--- b ) = a == b
isRef ( a :>-- b ) = a == b
isRef ( a :>-< b ) = a == b

-- get nodes
nodes :: Relation a -> ( a, a )
nodes ( a :--- b ) = ( a, b )
nodes ( a :>-- b ) = ( a, b )
nodes ( a :>-< b ) = ( a, b )
