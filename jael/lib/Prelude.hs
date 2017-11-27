{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveDataTypeable #-}
{-# Language NoImplicitPrelude #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module Prelude
  ( module X
  , filterByGt1
  , hoistFix
  , iterCofree
  , removeAnn
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

import Control.Comonad        as X --hiding ((<$>), (<$), ($>), fmap)
import Control.Comonad.Cofree as X

import Data.Functor.Foldable  as X hiding
  -- Defined in BasePrelude
  ( fold, gunfold
  -- Defined in Control.Comonad.Cofree
  , unfold)
import Data.Semigroup         as X

import Safe                   as X

import Data.Eq.Deriving       as X (deriveEq1)
import Text.Show.Deriving     as X (deriveShow1)

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M

$(deriveEq1   ''Cofree)
$(deriveShow1 ''Cofree)

deriving instance (Typeable f, Data a, Data (f (Cofree f a))) => Data (Cofree f a)

$(deriveEq1   ''C.CofreeF)
$(deriveShow1 ''C.CofreeF)

iterCofree :: Functor f => (a -> f b -> b) -> Cofree f a -> b
iterCofree fn x = fn (extract x) $ iterCofree fn <$> unwrap x

removeAnn :: Functor f => Cofree f a -> Fix f
removeAnn = iterCofree (\_ f -> Fix f)

hoistFix :: Functor f => (forall x. f x -> g x) -> Fix f -> Fix g
hoistFix n = cata (Fix . n)

filterByGt1 :: Ord a => [a] -> [[a]]
filterByGt1 = foldr (\x acc -> if length x > 1 then x:acc else acc) [] . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByGt1

unsafeLookup :: (Ord k, Show k, Show a) => k -> M.Map k a -> a
unsafeLookup k m = fromMaybe (error errMsg) $ M.lookup k m where
  errMsg = "unsafeLookup: Key `" ++ show k ++ "` not found in:\n" ++ show m
