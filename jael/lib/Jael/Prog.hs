{-# Language
    DeriveFunctor,
    NoImplicitPrelude #-}

module Jael.Prog where

import BasePrelude

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

