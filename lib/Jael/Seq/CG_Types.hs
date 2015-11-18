module Jael.Seq.CG_Types where

import qualified Data.Functor.Foldable as F
import           Jael.Seq.HM_Types

data CGBasicType = CGUnit
                 | CGInt { cgIntMin :: Integer
                         , cgIntMax :: Integer
                         }
                 | CGBool
                 | CGBit { cgBitSize :: Integer }
                 deriving (Eq, Show)

instance SeqTypable CGBasicType where
  tyOf (CGUnit) = TUnit
  tyOf (CGInt{}) = TInt
  tyOf (CGBool{}) = TBool
  tyOf (CGBit{}) = TBit

data CGTy = CGTySimple CGBasicType
          | CGTyStruct
          | CGTyEnumer
          | CGTyTup [CGTy]
          deriving (Eq, Show)

data CGTyF a = CGTySimpleF CGBasicType
             | CGTyStructF
             | CGTyEnumerF
             | CGTyTupF [a]
             deriving (Functor, Show)

type instance F.Base CGTy = CGTyF

instance F.Foldable CGTy where
  project (CGTySimple x) = CGTySimpleF x
  project (CGTyStruct) = CGTyStructF
  project (CGTyEnumer) = CGTyEnumerF
  project (CGTyTup xs) = CGTyTupF xs

instance F.Unfoldable CGTy where
  embed (CGTySimpleF x) = CGTySimple x
  embed (CGTyStructF) = CGTyStruct
  embed (CGTyEnumerF) = CGTyEnumer
  embed (CGTyTupF xs) = CGTyTup xs

instance SeqTypable CGTy where
  tyOf = F.cata alg
    where alg (CGTySimpleF x) = tyOf x
          alg (CGTyStructF) = undefined
          alg (CGTyEnumerF) = undefined
          alg (CGTyTupF xs) = TTup xs

