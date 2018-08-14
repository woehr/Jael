{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Jael.DataDecl where

import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.TreeDiff as TD

import Jael.Prelude
import Jael.Type

data ConsInfo t
  = ConsInfo
  { ddciName   :: T.Text
  , ddciParams :: [t]
  , ddciTag    :: Integer
  } deriving (Eq, Functor, Generic, Show)

instance (TD.ToExpr t) => TD.ToExpr (ConsInfo t)

data DataDecl t
  = DataDecl
  { dataName  :: T.Text
  , dataTVars :: [T.Text]
  , dataCons  :: [(T.Text, ConsInfo t)]
  } deriving (Eq, Functor, Generic, Show)

instance (TD.ToExpr t) => TD.ToExpr (DataDecl t)

constructorArity :: DataDecl t -> [(T.Text, Integer)]
constructorArity DataDecl{..} =
  fmap (second $ length . ddciParams) dataCons

dataDeclType :: DataDecl Type' -> Type'
dataDeclType DataDecl{..} =
  generalize' M.empty $ TCon dataName
    (fmap (TVar @T.Text @(TypeCs T.Text T.Text)) dataTVars)

dataConTypes :: DataDecl Type' -> [(T.Text, Type')]
dataConTypes DataDecl{..} = flip fmap dataCons . second
  $ generalize' M.empty
  . foldr (TFun @T.Text Nothing) (TCon dataName $ fmap TVar dataTVars)
  . ddciParams
