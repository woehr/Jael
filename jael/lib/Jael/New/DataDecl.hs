{-# Language DeriveFunctor #-}
{-# Language RecordWildCards #-}

module Jael.New.DataDecl where

import qualified Data.Map as M
import qualified Data.Text as T

import Jael.New.Type

data ConsInfo t
  = ConsInfo
  { ddciParams :: [t]
  , ddciTag    :: Integer
  } deriving (Eq, Functor, Show)

data DataDecl t
  = DataDecl
  { dataName :: T.Text
  , dataTVars :: [T.Text]
  , dataCons  :: M.Map T.Text (ConsInfo t)
  } deriving (Eq, Functor, Show)

constructorArity :: DataDecl t -> M.Map T.Text Int
constructorArity DataDecl{..} = M.map (length . ddciParams) dataCons

dataDeclType :: DataDecl Type -> Type
dataDeclType DataDecl{..} =
  generalize' M.empty $ TCon dataName (map TVar dataTVars)

dataConTypes :: DataDecl Type -> M.Map T.Text Type
dataConTypes DataDecl{..} = flip M.map dataCons
  $ generalize' M.empty
  . foldr TFun' (TCon dataName $ map TVar dataTVars)
  . ddciParams
