module Builder where

import qualified Graph as G
import qualified Relation as R
import Relation ( Relation ( .. ), Weight ( .. ) )
import Graph ( Graph ( .. ), Tree ( .. ) )

-------------------------features--------------------------
-- make relation tree
-- example: ( relation ( 0 :>-- 1 ) `mix` ) `with` ( 1 :>-- 2 )
relation :: Eq a => Relation a -> Tree a
relation = ( :/\: [] )

-- binding declaration between relations
mix :: Eq a => Tree a -> Relation a -> Tree a
tree `mix` rel
    | tree > tree' = head $ G.dnaOfTree [ tree, tree' ]
    | otherwise = tree
    where
        tree' = head ( G.dnaStones [ rel ] )

with :: ( a -> b ) -> a -> b
f `with` a = f a
