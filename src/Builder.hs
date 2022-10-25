{-# LANGUAGE TypeFamilies #-}
module Builder where

import qualified Graph as G
import qualified Relation as R
import Relation ( Relation ( .. ), Weight ( .. ) )
import Graph ( Graph ( .. ), Tree ( .. ) )

import Control.Monad.Trans.State ( StateT ( .. ) )

------------------------type declaration------------------
type Building generalTp monad = StateT ( Graph generalTp () ) monad ( Graph generalTp () )

-------------------------features--------------------------
--- - - - - - - - - - - inside Building - - - - - - - - ---
-- clear monad
begin :: ( Monad m, Eq a ) => Building a m
begin = StateT $ \s -> return ( [], s )

-- add Relation to evaluated Graph
add :: ( Monad m, Eq a ) => Relation a -> Graph a () -> Building a m
add rel graph
    | rel `notElem` graph = StateT $ \s -> return ( rel : graph, s )
    | otherwise = StateT $ \s -> return ( graph, s )

-- drop Relation from evaluated Graph
drop :: ( Monad m, Eq a ) => Relation a -> Graph a () -> Building a m
drop rel graph = StateT $ \s -> return ( filter ( /= rel ) graph, s )

-- concatinate graphs
concatGraphWith :: ( Monad m, Eq a ) => Graph a () -> Graph a () -> Building a m
concatGraphWith graph' graph = let graph'' = filter ( `notElem` graph ) graph' in
    StateT $ \s -> return ( G.noRepeatSimpleGraph $ graph ++ graph'', s )

-- filter of evaluated graph
filterGraphUsing :: ( Monad m, Eq a ) => Graph a () -> Graph a () -> Building a m
filterGraphUsing graph' graph = StateT $ \s -> return ( filter ( `notElem` graph' ) graph, s )

-- simple end
end :: ( Monad m, Eq a ) => Graph a () -> Building a m
end graph = 
    StateT $ \s ->
        let
            newS = G.noRepeatSimpleGraph s
            newGraph = G.noRepeatSimpleGraph graph
        in
            return ( newGraph ++ filter ( `notElem` newGraph ) newS, [] )

-- modify state
modifyState :: ( Monad m, Eq a ) => ( Relation a -> Relation a ) -> Graph a () -> Building a m
modifyState f graph = StateT $ \s -> return ( graph, G.noRepeatSimpleGraph $ f <$> s )

-- modify evaluation
modifyEval :: ( Monad m, Eq a ) => ( Relation a -> Relation a ) -> Graph a () -> Building a m
modifyEval f graph = StateT $ \s -> return ( G.noRepeatSimpleGraph $ f <$> graph, s )

-- reverse state with eval
swap :: ( Monad m, Eq a ) => Graph a () -> Building a m
swap graph = StateT $ \s -> return ( s, G.noRepeatSimpleGraph graph )

--- - - - - - - - - - - outside Building - - - - - - - - - ---
buildBy :: ( Monad m, Eq a ) => Building a m -> Graph a () -> m [ Tree a ]
buildBy plan graph = do
    ( a, s ) <- runStateT plan graph
    return $ G.dnaOfGraph a
-- using like
-- "some Building" `buildBy` "some Graph"
