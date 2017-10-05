{-# Language DeriveFunctor #-}

module Jael.New.Misc where

import Text.Trifecta

type PTree f = Cofree f Span

iterCofree :: Functor f => (a -> f b -> b) -> Cofree f a -> b
iterCofree f cfa = uncofree f cfa
  where uncofree fn x = fn (extract x) $ fmap (uncofree fn) $ unwrap x

removeAnn :: Functor f => Cofree f a -> Fix f
removeAnn = iterCofree (\_ f -> Fix f)
