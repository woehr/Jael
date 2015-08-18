{-# Language NoImplicitPrelude #-}

module Jael.Util where

import ClassyPrelude

wrongNumberOfElements :: Integer -> String -> String -> a
wrongNumberOfElements n x y = error $ "Expected exactly " ++ show n ++ x ++ "'s in " ++ y

notEnoughElements :: Integer -> String -> String -> a
notEnoughElements n x y = error $ "Expected at least " ++ show n ++ x ++ "'s in " ++ y

