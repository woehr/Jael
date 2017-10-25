{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language NoImplicitPrelude #-}
{-# Language TemplateHaskell #-}

module Prelude
  ( module X
  , filterByGt1
  , repeated
  , unsafeLookup
  ) where

import BasePrelude            as X hiding
  -- Names I use
  ( TVar, sizeOf
  -- Prefer Data.Semigroup
  , First, Last, (<>), getFirst, getLast
  )

import MTLPrelude             as X hiding
  -- Defined in BasePrelude
  ( shift
  )

import Data.Semigroup         as X
import Control.Comonad        as X --hiding ((<$>), (<$), ($>), fmap)

import Control.Comonad.Cofree as X
import Data.Functor.Foldable  as X hiding
  -- Defined in BasePrelude
  ( fold, gunfold
  -- Defined in Control.Comonad.Cofree
  , unfold)

import Data.Eq.Deriving       as X (deriveEq1)
import Text.Show.Deriving     as X (deriveShow1)

import qualified Data.Map as M

$(deriveEq1   ''Cofree)
$(deriveShow1 ''Cofree)

filterByGt1 :: Ord a => [a] -> [[a]]
filterByGt1 = foldr (\x acc -> if length x > 1 then x:acc else acc) [] . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByGt1

unsafeLookup :: Ord k => k -> M.Map k a -> a
unsafeLookup k m =
  case M.lookup k m of
    Just x -> x
    Nothing -> error "unsafeLookup: Key not found."
