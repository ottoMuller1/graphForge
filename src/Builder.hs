{-# LANGUAGE TypeFamilies #-}
module Builder where

import qualified Graph as G
import qualified Relation as R
import Relation ( Relation ( .. ), Weight ( .. ) )
import Graph ( Graph ( .. ), Tree ( .. ) )

import Control.Monad.Trans.State ( StateT ( .. ) )

------------------------type declaration------------------
-- there is used StateT, because of tree building requires state like a relation 
-- and evaluates a tree
type Building generalTp monad = StateT ( Tree generalTp ) monad [ Tree generalTp ]
type family Morphism a where
    Morphism ( Tree a ) = Tree a -> Tree a

-------------------------features--------------------------
--- - - - - - - - - outside using - - - - - - - - - ---
-- let's write syntax for graph implementation
-- start building of a tree throw state - relation
build :: 
    ( Monad m, Eq a ) => 
    Building a m -> 
    Morphism ( Tree a ) -> 
    Tree a -> 
    m [ Tree a ]
build plan throw relations = do
    ( trees, rel ) <- runStateT plan $ throw relations
    return trees

-- use by to just get back relation
by :: Eq a => Morphism ( Tree a )
by = id
-- here we have syntax like
-- build "some Building" by "some Graph"
-- and return monadical ( "some Trees", "state Graph )
-- or
-- build "some Building" by $ R.upLevel "state Relation"

--- - - - - - - - - - - inside using - - - - - - - - - - - ---
-- if we need to use empty graph from begining
-- then we use begin
begin :: ( Monad m, Eq a ) => Building a m
begin = StateT $ \s -> return ( [], s )

-- a new relation to tree by mixing
add :: ( Monad m, Eq a ) => Relation a -> [ Tree a ] -> Building a m
add relation trees = StateT $ \s -> return ( G.dnaOfTree $ relation :/\: [] : trees , s )

-- a simple end of evaluation
end :: ( Monad m, Eq a ) => [ Tree a ] -> Building a m
end trees = StateT $ \s -> return ( G.dnaOfTree $ s : trees, Fault )

-- a left application with trees
endL :: ( Monad m, Eq a ) => [ Tree a ] -> Building a m
endL trees = StateT $ \s -> return ( ( s <> ) <$> trees, Fault )

-- a right application with trees
endR :: ( Monad m, Eq a ) => [ Tree a ] -> Building a m
endR trees = StateT $ \s -> return ( ( <> s ) <$> trees, Fault )

{-
    an example
    begin >>= mixing "some Relation" >>= mixing "some Relation" >>= end
-}
