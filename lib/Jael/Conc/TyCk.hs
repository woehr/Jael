{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
--import Data.Functor.Foldable
--import qualified Data.Map as M
--import qualified Data.Set as S
import Jael.Conc.Env
import Jael.Conc.Proc
--import Jael.Conc.Session
--import Jael.Seq.Types

data SessTyErr = SessTyErr
  deriving (Eq, Show)

sessTyCheck :: ConcTyEnv -> Proc -> Maybe SessTyErr
sessTyCheck _ _ = undefined

