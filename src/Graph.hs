{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Graph ( 
    Graph ( .. ), 
    Tree ( .. ),
    doSimple,
    width,
    treeSize,
    treeToGraph,
    treesToGraph,
    nodes,
    kids,
    getTrees,
    dnaOfGraph,
    noRepeatSimpleGraph,
    noRepeatWeightGraph ) where

import Relation ( 
    Relation ( .. ),
    Weight ( .. ) )
import qualified Relation as Rel

import Data.Kind ( Type )
import Data.Set ( 
    fromList, 
    toList )

-----------------------------type declaration--------------------------
type family Graph a b :: Type where
    Graph a () = [ Relation a ]
    Graph a b = [ Weight a b ]

data Tree a = ( Relation a ) :/\: [ Tree a ] | Fault deriving Eq

--------------------------------instances------------------------------
-- Semigroup
instance Eq a => Semigroup ( Tree a ) where
    Fault <> right = right
    left <> Fault = left
    left@( rel1 :/\: trees1 ) <> right@( rel2 :/\: trees2 )
        | rel1 > rel2 = 
            if right `notElem` trees1 
            then rel1 :/\: ( trees1 ++ [ right ] )
            else left
        | rel1 == rel2 = rel1 :/\: dnaOfTree ( trees1 ++ filter ( `notElem` trees1 ) trees2 )
        | left > right = rel1 :/\: subMapping trees1 right
        | otherwise = left
        where
            subMapping :: Eq a => [ Tree a ] -> Tree a -> [ Tree a ]
            subMapping [] _ = []
            subMapping ( l : ls ) r
                | l <> r == l = l : subMapping ls r
                | otherwise = l <> r : ls

-- Ord
-- note, that tree1 > tree2, when root of tree2 less then one of roots of tree1
instance Eq a => Ord ( Tree a ) where
    compare Fault Fault = EQ
    compare Fault _ = LT
    compare _ Fault = GT
    compare left@( rel1 :/\: _ ) right@( rel2 :/\: _ ) = let
        isRightInLeft = not $ null $ filter ( > rel2 ) $ treeToGraph left
        isLeftInRight = not $ null $ filter ( > rel1 ) $ treeToGraph right
        in case ( isRightInLeft, isLeftInRight ) of
            ( False, False ) -> EQ
            ( True, _ ) -> GT
            ( False, _ ) -> LT

-- Show
instance Show a => Show ( Tree a ) where
    show Fault = "Fault \n"
    show ( rel :/\: trees ) = helper "" rel trees
        where
            helper :: Show a => String -> Relation a -> [ Tree a ] -> String
            helper str rel trees =
                str ++ 
                show rel ++ 
                "\n" ++ 
                mconcat ( ( \case
                    Fault -> "  " ++ str ++ "Fault \n"
                    ( rel' :/\: trees' ) -> helper ( "  " ++ str ) rel' trees' ) <$> trees )

instance {-# OVERLAPPING #-} Show a => Show [ Tree a ] where
    show [] = ""
    show ( tree : trees ) = show tree ++ show trees

-- Functor
instance Functor Tree where
    fmap _ Fault = Fault
    fmap f ( rel :/\: tree ) = ( f <$> rel ) :/\: ( fmap f <$> tree )

-- Applicative
instance Applicative Tree where
    pure a = pure a :/\: []
    Fault <*> _ = Fault 
    _ <*> Fault = Fault
    ( fRel :/\: fTrees ) <*> ( rel :/\: trees ) =
        ( fRel <*> rel ) :/\: zipWith ( <*> ) fTrees trees

instance Monad Tree where
    return = pure
    Fault >>= _ = Fault
    ( rel :/\: trees ) >>= f = 
        case rel of
            ( _ :--- a ) -> 
                let
                    rel' :/\: trees' = f a
                    ( n, m ) = Rel.nodes rel'
                in
                    rel' :/\: ( trees' ++ ( helper m . ( >>= f ) <$> trees ) )
            ( _ :>-- a ) ->
                let
                    rel' :/\: trees' = f a
                    ( n, m ) = Rel.nodes rel'
                in
                    rel' :/\: ( trees' ++ ( helper m . ( >>= f ) <$> trees ) )
            ( _ :>-< a ) ->
                let
                    rel' :/\: trees' = f a
                    ( n, m ) = Rel.nodes rel'
                in
                    rel' :/\: ( trees' ++ ( helper m . ( >>= f ) <$> trees ) )
        where
            helper :: a -> Tree a -> Tree a
            helper _ Fault = Fault
            helper a ( b :--- c :/\: t ) = a :--- c :/\: t
            helper a ( b :>-- c :/\: t ) = a :>-- c :/\: t
            helper a ( b :>-< c :/\: t ) = a :>-< c :/\: t

-- Monoid
instance Eq a => Monoid ( Tree a ) where
    mempty = Fault
    mappend = ( <> )

-- Folds
instance Foldable Tree where
    foldr f a Fault = a
    foldr f a ( rel :/\: trees ) = foldr f a rel
    length Fault = 0
    length ( rel :/\: trees ) = 1 + maximum' ( length <$> trees )
        where 
            maximum' [] = 0
            maximum' xs = maximum xs

----------------------------------features-----------------------------
-- returns simple graph from weight graph
doSimple :: [ Weight a b ] -> Graph a ()
doSimple = ( Rel.getRelation <$> )

-- width of tree
-- width here is a max count of relation list in tree
width :: Eq a => Tree a -> Int
width tree = let
    newTree = dnaOfTreeKids tree
    in
    helper newTree
    where
        helper :: Eq a => Tree a -> Int
        helper Fault = 0
        helper ( rel :/\: [] ) = 1
        helper ( rel :/\: trees ) = max ( length trees ) $ sum $ helper <$> trees

-- size of tree
treeSize :: Eq a => Tree a -> ( Int, Int )
treeSize tree = ( length tree, width tree )

-- from tree to graph simple
treeToGraph :: Eq a => Tree a -> Graph a ()
treeToGraph Fault = []
treeToGraph ( rel :/\: trees ) =
    rel : treesToGraph trees

treesToGraph :: Eq a => [ Tree a ] -> Graph a ()
treesToGraph trees = mconcat $ treeToGraph <$> trees

-- get nodes of graph
nodes :: Eq a => Graph a () -> [ a ]
nodes = let
    iter :: Graph a () -> [ a ]
    iter [] = []
    iter ( ( a :--- b ) : graph ) = a : b : iter graph
    iter ( ( a :>-- b ) : graph ) = a : b : iter graph
    iter ( ( a :>-< b ) : graph ) = a : b : iter graph

    reps :: Eq a => [ a ] -> [ a ] -> [ a ]
    reps new [] = new
    reps new ( x : old )
        | x `elem` new = reps new old
        | otherwise = reps ( x : new ) old
    in reps [] . iter

-- find kids of every relation
kids :: Eq a => Graph a () -> [ Tree a ]
kids graph = [ rel :/\: [ rel' :/\: [] | rel' <- graph, rel > rel' && Rel.nodes rel /= Rel.nodes rel' ] | rel <- graph ]

-- begin of tree o2, refactor!
beginTree :: Eq a => [ Tree a ] -> [ Tree a ]
beginTree trees = [ tree | tree <- trees, tree > tree || null [ tree' | tree' <- trees, tree' > tree ] ]

-- get trees of relation in tree graph
getTrees :: Eq a => Tree a -> [ Tree a ]
getTrees Fault = []
getTrees ( rel :/\: trees ) = trees

-- do tree from simple graph
dnaStones :: Eq a => Graph a () -> [ Tree a ]
dnaStones = ( ( :/\: [] ) <$> )

-- sort relations
-- just for quick solving :(
-- refactor! o2
treeSorting :: Eq a => [ Tree a ] -> [ Tree a ]
treeSorting trees = let left = beginTree trees in
    left ++ sortMethod [ tree | tree <- trees, tree `notElem` left ]
    where
        sortMethod :: Eq a => [ Tree a ] -> [ Tree a ]
        sortMethod [] = []
        sortMethod ( x : xs ) = 
            sortMethod [ y | y <- xs, x <= y ] ++ 
            [ x ] ++ 
            sortMethod [ y | y <- xs, x > y ]

-- getting classes
classBuilder :: Eq a => [ Tree a ] -> [ Tree a ]
classBuilder [] = []
classBuilder inputTrees = let
    ( tree : trees ) = treeSorting inputTrees
    erbe = [ tree' | tree' <- trees, tree > tree' && tree /= tree' ]
    newTree = foldl ( <> ) tree erbe
    other = filter ( `notElem` ( tree : erbe ) ) trees in
    case erbe of
        [] -> tree : classBuilder other
        erbe -> classBuilder $ newTree : other

-- factorization of Graph
dnaOfGraph :: Eq a => Graph a () -> [ Tree a ]
dnaOfGraph = dnaOfTree . dnaStones

dnaOfTree :: Eq a => [ Tree a ] -> [ Tree a ]
dnaOfTree trees
    | null other = dnaOfTreeKids <$> begin
    | not ( null other ) =
        dnaOfTree $ classBuilder trees
    | otherwise = []
    where
        begin = beginTree trees
        other = filter ( `notElem` begin ) trees

dnaOfTreeKids :: Eq a => Tree a -> Tree a
dnaOfTreeKids tree = case tree of
    Fault -> Fault
    ( rel :/\: trees' ) -> rel :/\: dnaOfTree trees'

-- do simple graph no repeatable
noRepeatSimpleGraph :: Eq a => Graph a () -> Graph a ()
noRepeatSimpleGraph [] = []
noRepeatSimpleGraph ( rel : other ) = 
    rel : noRepeatSimpleGraph ( filter ( /= rel ) other )

-- do weighter graph no repeatable
noRepeatWeightGraph :: Eq a => [ Weight a b ] -> [ Weight a b ]
noRepeatWeightGraph [] = []
noRepeatWeightGraph ( weight@( rel :^: w ) : other ) = 
    weight : noRepeatWeightGraph ( filter ( \( rel' :^: _ ) -> rel /= rel' ) other )
