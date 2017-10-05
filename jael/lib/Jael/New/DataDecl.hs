{-# Language DeriveFunctor #-}
{-# Language RecordWildCards #-}

module Jael.New.DataDecl
where

import qualified Data.Map as M
import qualified Data.Text as T

--import Jael.New.Type

data DataDecl t = DataDecl
                { dataTVars :: [T.Text]
                , dataCons  :: M.Map T.Text [t]
                } deriving (Eq, Functor, Show)

constructorArity :: DataDecl t -> M.Map T.Text Int
constructorArity (DataDecl {..}) = M.map length dataCons
