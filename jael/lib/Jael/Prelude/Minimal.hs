{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Jael.Prelude.Minimal
  ( module X
  , length
  , unsafeLookup
  , sortFst
  )
where

import qualified Data.Map                      as M
import qualified Prelude                       as P

import           Control.Applicative           as X
import           Control.Monad                 as X
import           Data.Bifunctor                as X
import           Data.Bool                     as X
import           Data.Char                     as X
                                                   hiding ( digitToInt )
import           Numeric                       as X
import           Data.Either                   as X
import           Data.Eq                       as X
import           Data.Foldable                 as X
                                                   hiding ( length )
import           Data.Function                 as X
import           Data.Functor.Classes          as X
import           Data.Maybe                    as X
import           Data.Ord                      as X
import           Data.Semigroup                as X
import           Data.String                   as X
import           Data.Traversable              as X
import           Data.Tuple                    as X
import           GHC.Err                       as X
import           GHC.Num                       as X
import           Text.Show                     as X

-- placeholders
import           Development.Placeholders      as X

-- safe
import           Safe                          as X
import           Safe.Exact                    as X
--import Safe.Foldable as X
import           Data.List                     as X
                                                   hiding ( cycle
                                                          , elemIndex
                                                          , find
                                                          , findIndex
                                                          , foldl1
                                                          , foldl1'
                                                          , foldr1
                                                          , group
                                                          , head
                                                          , init
                                                          , last
                                                          , length
                                                          , lookup
                                                          , maximum
                                                          , maximumBy
                                                          , minimum
                                                          , minimumBy
                                                          , nub
                                                          , scanl1
                                                          , scanr1
                                                          , sort
                                                          , tail
                                                          , (!!)
                                                          )

-- split
import           Data.List.Split               as X

-- uniform-pair
import           Data.UniformPair              as X

import qualified Data.MultiMap                 as MM

-- Sorts according to the first field of a pair and maintains the relative
-- ordering of identical elements. Used mainly (exclusively?) for sorting
-- labels in rows/records
sortFst :: (Ord a) => [(a, b)] -> [(a, b)]
sortFst = MM.toList . MM.fromList

length :: (Foldable t) => t a -> Integer
length = P.toInteger . P.length

unsafeLookup :: (Ord k, Show k, Show a) => k -> M.Map k a -> a
unsafeLookup k m = fromMaybe (P.error errMsg) $ M.lookup k m
 where
  errMsg = "unsafeLookup: Key `" <> show k <> "` not found in:\n" <> show m
