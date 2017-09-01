{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language NoImplicitPrelude #-}
{-# Language TemplateHaskell #-}

module Prelude (module X) where

import BasePrelude            as X hiding (TVar)
import MTLPrelude             as X hiding (shift)
import Control.Comonad        as X hiding ((<$>), (<$), ($>), fmap)
import Control.Comonad.Cofree as X
import Data.Functor.Foldable  as X hiding (fold, gunfold, unfold)
import Data.Eq.Deriving       as X (deriveEq1)
import Text.Show.Deriving     as X (deriveShow1)

$(deriveEq1   ''Cofree)
$(deriveShow1 ''Cofree)
