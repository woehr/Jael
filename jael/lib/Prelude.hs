{-# Language NoImplicitPrelude #-}

module Prelude (module X) where

import BasePrelude            as X hiding (TVar)
import MTLPrelude             as X hiding (shift)
import Control.Comonad        as X hiding ((<$>), (<$), ($>), fmap)
import Control.Comonad.Cofree as X
import Data.Functor.Foldable  as X hiding (fold, gunfold, unfold)
