{-# Language DeriveFunctor #-}

module Jael.Util where

import Prelude ()
import BasePrelude
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- An annotated type. Takes 1) a type with which nodes (right word?) of a functor
-- are annotated with, and 2) a functor to be annotated.
data Ann x f a = Ann { ann :: x, unAnn :: f a }
  deriving (Eq, Show, Functor)

data Token a = Token { value :: a, lineCol :: (Int, Int) }
  deriving (Eq, Show, Functor)

type Ident = Token T.Text
type IntConst = Token Integer

instance Ord a =>  Ord (Token a) where
  compare x y = compare (value x) (value y)

addIfUnique :: Ord a => (a, b) -> M.Map a b -> Maybe (M.Map a b)
addIfUnique (k, v) m = case M.insertLookupWithKey (\_ n _ -> n) k v m of
                            (Nothing, m') -> Just m'
                            (_, _) -> Nothing

collectDup :: Ord a => (a, b) -> ([a], M.Map a b) -> ([a], M.Map a b)
collectDup ins@(k, _) (col, m) = case addIfUnique ins m of
                                      Nothing -> (k:col, m)
                                      Just m' -> (col, m')

-- Given an environment and a list of tuples to add, returns either a list of
-- duplicate names or a new environment with the elements of the list added.
insertCollectDups :: Ord a => M.Map a b -> [(a, b)] -> Either [a] (M.Map a b)
insertCollectDups m xs = let (dups, m') = foldr collectDup ([], m) xs
                   in if null dups
                         then Right m'
                         else Left dups

-- https://hackage.haskell.org/package/Unique
repeated :: Ord a => [a] -> [a]
repeated = map head . filterByGt1

{-@ filterByGt1 :: [a] -> [{v:[a] | len v > 1}] @-}
filterByGt1 :: Ord a => [a] -> [[a]]
filterByGt1 = foldr (\x acc -> if length x > 1 then x:acc else acc) [] . group . sort

-- Find undefined variables for a given dependencies map
hasUndefined :: M.Map T.Text (S.Set T.Text) -> Maybe (S.Set T.Text)
hasUndefined deps =
  let ns = M.keysSet deps
      free = S.unions (M.elems deps)
      undef = free S.\\ ns
   in if not (null undef)
         then Just undef
         else Nothing

-- Given a map of names to their dependencies return either a list of names
-- that are part of cycles or a list of names that specify the (reverse) order
-- in which the names should be processed
findCycles :: M.Map T.Text (S.Set T.Text) -> Either [T.Text] [T.Text]
findCycles m =
  let (dg, vertToNode, _) = G.graphFromEdges $ map (\(a,b)->(a,a,b))
                                             $ M.toList (M.map S.toList m)
      nodeName = (\(a,_,_)->a) . vertToNode
      (lb, ub) = A.bounds dg
      -- For each node, find it's successors and determine if it has a path back
      recDeps = [nodeName x | x <- [lb..ub]
                            , y <- dg A.! x
                            , G.path dg y x]
   in if not (null recDeps)
         then Left recDeps
         else Right . map nodeName . G.topSort $ dg

lowerFirst :: T.Text -> T.Text
lowerFirst xs = case T.uncons xs of
                     Just (x, xs') -> (T.toLower . T.singleton $ x) <> xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."
