{-# Language NoImplicitPrelude #-}

module Jael.Types.Ann where

import Jael.Prelude
import Jael.Util

type Ann f a = Cofree f a

setAnn :: Ann f a -> a -> Ann f a
setAnn (_ :< fa) x = x :< fa

removeAnn :: Functor f => Ann f a -> Fix f
removeAnn = iterCofree (\_ f -> Fix f)
