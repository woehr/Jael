module Jael.Seq.CG_Types where

import qualified Data.Functor.Foldable as F
import           Jael.Grammar
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
          | CGTyTup [CGTy]
          | CGTyNamed Text [CGTy]
          | CGTyVar Text
          deriving (Eq, Show)

data CGTyF a = CGTySimpleF CGBasicType
             | CGTyTupF [a]
             | CGTyNamedF Text [a]
             | CGTyVarF Text
             deriving (Functor, Show)

type instance F.Base CGTy = CGTyF

instance F.Foldable CGTy where
  project (CGTySimple x) = CGTySimpleF x
  project (CGTyTup xs) = CGTyTupF xs
  project (CGTyNamed n xs) = CGTyNamedF n xs
  project (CGTyVar x) = CGTyVarF x

instance F.Unfoldable CGTy where
  embed (CGTySimpleF x) = CGTySimple x
  embed (CGTyTupF xs) = CGTyTup xs
  embed (CGTyNamedF n xs) = CGTyNamed n xs
  embed (CGTyVarF x) = CGTyVar x

instance SeqTypable CGTy where
  tyOf = F.cata alg
    where alg (CGTySimpleF x) = tyOf x
          alg (CGTyTupF xs) = TTup xs
          alg (CGTyNamedF n xs) = TNamed n xs
          alg (CGTyVarF x)  = TyVar x

gToType :: GType -> CGTy
gToType GTInt  = CGTySimple $ CGInt (fromIntegral (minBound::Int32)) (fromIntegral (maxBound::Int32))
gToType GTBool = CGTySimple CGBool
gToType GTUnit = CGTySimple CGUnit
gToType (GTNamed (UIdent n) GTNamedNoParam) = CGTyNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) =
  CGTyNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = CGTyVar (pack s)
gToType (GTTup x xs) = CGTyTup $ map (\(GTTupArg y) -> gToType y) (x:xs)

