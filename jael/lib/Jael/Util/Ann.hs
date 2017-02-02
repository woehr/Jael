{-# Language NoImplicitPrelude #-}

module Jael.Util.Ann where

import BasePrelude
import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable

import Jael.Util

type Ann f a = Cofree f a

setAnn :: Ann f a -> a -> Ann f a
setAnn (_ :< fa) x = x :< fa

removeAnn :: Functor f => Ann f a -> Fix f
removeAnn = iterCofree (\_ f -> Fix f)
