{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Test.Hspec

import           Jael.Prelude
import qualified Spec         as S

main :: IO ()
main = hspec S.spec
