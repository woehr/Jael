{-# Language DeriveFunctor #-}
{-# Language NoImplicitPrelude #-}

module Jael.Types.Prog where

import Jael.Prelude

newtype ProgramF a = ProgramF
  { _unProgramF :: a
  } deriving (Show, Functor)

data Program a b c = Program
  { _types :: a
  , _funcs :: b
  , _globs :: c
--  , _sesns :: d
--  , _procs :: e
  } deriving (Eq, Show)

