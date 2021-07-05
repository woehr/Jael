{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad                            ( void )
import           Hedgehog
import           Jael.Test.PrettyProps

main :: IO ()
main = void $ checkParallel tests
