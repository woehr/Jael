{-# Language DeriveFunctor #-}

module Jael.Grammar.Located where

data Located a = Located
  { locatedOffset :: Maybe Int
  , locatedPosn   :: Maybe (Int, Int)
  , locatedValue  :: a
  }
  deriving (Eq, Functor, Show)
