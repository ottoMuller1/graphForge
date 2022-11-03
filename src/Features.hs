module Features where

insert :: Ord a => [ a ] -> [ a ]
insert [] = []
insert ( x : xs ) = 
    insert [ y | y <- xs, x <= y ] ++ 
    [ x ] ++ 
    insert [ y | y <- xs, x > y ]
