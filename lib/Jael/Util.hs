{-# Language NoImplicitPrelude #-}

module Jael.Util where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

wrongNumberOfElements :: Integer -> String -> String -> a
wrongNumberOfElements n x y = error $ "Expected exactly " ++ show n ++ x ++ "'s in " ++ y

notEnoughElements :: Integer -> String -> String -> a
notEnoughElements n x y = error $ "Expected at least " ++ show n ++ x ++ "'s in " ++ y

addIfUnique :: Ord a => (a, b) -> M.Map a b -> Maybe (M.Map a b)
addIfUnique (k, v) m = case M.insertLookupWithKey (\_ n _ -> n) k v m of
                            (Nothing, m') -> Just m'
                            (_, _) -> Nothing

collectDup :: Ord a => (a, b) -> ([a], M.Map a b) -> ([a], M.Map a b)
collectDup ins@(k, v) (col, m) = case addIfUnique ins m of
                                      Nothing -> (k:col, m)
                                      Just m' -> (col, m')

-- Given an environment and a list of tuples to add, returns either a list of
-- duplicate names or a new environment with the elements of the list added.
insertCollectDups :: Ord a => M.Map a b -> [(a, b)] -> Either [a] (M.Map a b)
insertCollectDups m xs = let (dups, m') = foldr collectDup ([], m) xs
                   in if null dups
                         then Right m'
                         else Left dups

-- NonEmpty version of https://hackage.haskell.org/package/Unique
repeated :: Ord a => [a] -> [a]
repeated = map NE.head . filterByLength (>1)

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [NE.NonEmpty a]
filterByLength p = filter (p . length) . NE.group . sort

