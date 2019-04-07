{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jael.Prelude
  ( module X
  , hoistFix
  , iterCofree
  , unCofree
  , repeats
  , pShow
  , digitToInt
  )
where

import           Jael.Prelude.Minimal          as X

-- base
--import Control.Applicative  as X
--import Control.Monad        as X
import           Data.Bifoldable               as X
--import Data.Bifunctor       as X
--import Data.Bool            as X
--import Data.Char            as X hiding (digitToInt)
import           Data.Data                     as X
                                                          ( Data
                                                          , Typeable
                                                          )
--import Data.Either          as X
--import Data.Eq              as X
--import Data.Foldable        as X hiding (length)
--import Data.Function        as X
--import Data.Functor.Classes as X
--import Data.List            as X
--  ( drop
--  , dropWhile
--  , filter
--  , intercalate
--  , intersperse
--  , lookup
--  , repeat
--  , replicate
--  , take
--  , takeWhile
--  , uncons
--  , unzip
--  , zip
--  , zipWith
--  )
--import Data.Maybe as X
--import Data.Ord         as X
--import Data.Semigroup   as X
--import Data.String      as X
--import Data.Traversable as X
--import Data.Tuple       as X
import           GHC.Enum                      as X
--import GHC.Err          as X
import           GHC.Exts                      as X
                                                          ( Constraint )
import           GHC.Float                     as X
import           GHC.Generics                  as X
                                                          ( Generic )
--import GHC.Num      as X
import           GHC.Real                      as X
import           System.IO                     as X
--import Text.Show        as X

-- comonad
import           Control.Comonad               as X

-- deriving-compat
import           Data.Eq.Deriving              as X
import           Text.Show.Deriving            as X

-- discrimination
import           Data.Discrimination           as X

-- free
import           Control.Comonad.Cofree        as X

-- lens
import           Control.Lens.Operators        as X
import           Control.Lens.TH               as X

-- mtl
import           Control.Monad.Except          as X
import           Control.Monad.Identity        as X
import           Control.Monad.RWS             as X
                                                   hiding ( First
                                                          , Last
                                                          , getFirst
                                                          , getLast
                                                          , (<>)
                                                          )
import           Control.Monad.State           as X
import           Control.Monad.Writer          as X
                                                   hiding ( First
                                                          , Last
                                                          , getFirst
                                                          , getLast
                                                          , (<>)
                                                          )

-- recursion-schemes
import           Data.Functor.Foldable         as X
                                                   hiding ( fold
                                                          , gfold
                                                          , grefold
                                                          , gunfold
                                                          , refold
                                                          , unfold
                                                          )

-- tostring
import           Data.String.ToString          as X

import qualified Control.Comonad.Trans.Cofree  as C
import qualified Data.Text.Lazy                as TL
import qualified Data.TreeDiff
import qualified Prelude                       as P
import qualified Text.Pretty.Simple            as PS

deriving instance Generic (Fix f)
instance (Data.TreeDiff.ToExpr (f (Fix f))) => Data.TreeDiff.ToExpr (Fix f)

deriving instance Generic (Pair a)
deriving instance (Data a) => Data (Pair a)
instance (Data.TreeDiff.ToExpr a) => Data.TreeDiff.ToExpr (Pair a)

$(deriveEq1   ''C.CofreeF)
$(deriveShow1 ''C.CofreeF)

pShow :: (Show a) => a -> String
pShow = TL.unpack . PS.pShow

-- Total, integer version.
-- https://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Char.html#digitToInt
digitToInt :: Char -> Maybe Integer
digitToInt c = toInteger <$> if
  | (fromIntegral dec :: P.Word) <= 9  -> Just dec
  | (fromIntegral hexl :: P.Word) <= 5 -> Just $ hexl + 10
  | (fromIntegral hexu :: P.Word) <= 5 -> Just $ hexu + 10
  | otherwise                          -> Nothing
 where
  dec  = ord c - ord '0'
  hexl = ord c - ord 'a'
  hexu = ord c - ord 'A'

iterCofree :: Functor f => (a -> f b -> b) -> Cofree f a -> b
iterCofree fn x = fn (extract x) $ iterCofree fn <$> unwrap x

unCofree :: Functor f => Cofree f a -> Fix f
unCofree = iterCofree (\_ f -> Fix f)

hoistFix :: Functor f => (forall x . f x -> g x) -> Fix f -> Fix g
hoistFix n = cata (Fix . n)

repeats :: (IsString a, ToString a) => [a] -> [a]
repeats = fmap fromString . foldr f [] . group . fmap toString
 where
  f x a = case x of
    (y : _ : _) -> y : a
    _           -> a
