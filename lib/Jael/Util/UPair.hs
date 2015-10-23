{-# Language NoImplicitPrelude #-}

module Jael.Util.UPair
( mkUPair
, UPair()
) where

import ClassyPrelude

data UPair a = UPair (a, a)
  deriving (Eq, Ord, Show)

mkUPair :: (Eq a, Ord a, Show a) => a -> a -> UPair a
mkUPair x y = if x < y then UPair (x, y) else UPair (y, x)

