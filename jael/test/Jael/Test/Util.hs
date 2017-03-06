{-# Language NoImplicitPrelude #-}

module Jael.Test.Util where

import           Jael.Prelude

import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
import qualified Jael.Grammar as G
import           Jael.Infer
import           Jael.Types

toMTE :: T.Text -> MaybeTypedExpr
toMTE p = case runParser G.pExpr p of
  Left t -> error (T.unpack t)
  Right x -> jaelify x

toHM :: T.Text -> HMTypedExpr
toHM p = case hmInf emptyEnv (toMTE p) of
  Left t -> error (T.unpack t)
  Right x -> x

toTE :: T.Text -> TypedExpr
toTE p =
  let mte = toMTE p
      hme = toHM p
  in  case reQual mte hme of
        Left t -> error (T.unpack t)
        Right x -> fmap (fmap F.simplify) x
