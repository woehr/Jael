{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language PatternSynonyms #-}

module Jael.Types.QType where

import Data.Functor.Foldable (Fix (..))
import           Control.Comonad.Trans.Cofree
import           Data.OpenADT
import           Data.Row

import qualified Data.Text                     as T

type Refinement e = Maybe (T.Text, e)

type QTypeF (t :: Row (* -> *)) e = CofreeF (VarF t) (Refinement e)

pattern QualTypeF :: VarF t q -> Refinement e -> QTypeF t e q
pattern QualTypeF f r = r :< f
pattern QualType  f r = Fix (QualTypeF r f)

pattern UnqualTypeF :: VarF t q -> QTypeF t e q
pattern UnqualTypeF f = Nothing :< f
pattern UnqualType  f = Fix (UnqualTypeF f)
