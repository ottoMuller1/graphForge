{-# LANGUAGE TypeFamilies #-}
module Builder where

import qualified Graph as G
import qualified Relation as R
import Relation ( Relation ( .. ), Weight ( .. ) )
import Graph ( Graph ( .. ), Tree ( .. ) )

import Control.Monad.Trans.State ( StateT ( .. ) )

------------------------type declaration------------------
-- TreeBuilder is supposed to be StateT where
-- evaluation is Tree a
-- state is Tree a -> Tree a
type TreeBuilder gTp monad = 
    StateT ( Tree gTp -> Tree gTp ) monad ( Tree gTp )

-------------------------features--------------------------
--- - - - - - - - - - - inside TreeBuilder - - - - - - - - ---
-- begining of TreeBuilder
quite :: ( Eq a, Monad b ) => TreeBuilder a b
quite = return Fault

-- build TreeBuilder
buildBy :: ( Eq a, Monad b ) => TreeBuilder a b -> ( Tree a -> Tree a ) -> b ( Tree a )
tb `buildBy` f = fst <$> runStateT tb f

-- absorbation by TreeBuilder's evaluation
absorbes :: ( Eq a, Monad b ) => Tree a -> Graph a () -> TreeBuilder a b
tree `absorbes` graph = return $ foldl (<>) tree $ G.dnaOfGraph graph
{-
quite >>= absorbes "Tree"

or

do
    tree <- quite
    tree `absorbes` "Graph"
-}
