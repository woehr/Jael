module Jael.Seq.CG_Types where

import qualified Data.Functor.Foldable as F
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.HM_Types
import           Jael.Util

data BasicType a = BTUnit
                 | BTBit { btBits :: a }
                 | BTBool
                 | BTBuffer { btBufferMin :: a
                            , btBufferMax :: a
                            }
                 | BTInt { btIntMin :: a
                         , btIntMax :: a
                         }
                 deriving (Eq, Show)

instance SeqTypable (BasicType a) where
  tyOf (BTUnit)     = TySimple TyUnit
  tyOf (BTBit{})    = TySimple TyBit
  tyOf (BTBool{})   = TySimple TyBool
  tyOf (BTBuffer{}) = TySimple TyBuffer
  tyOf (BTInt{})    = TySimple TyInt

data GramTy = GramTySimple (BasicType (Maybe Integer))
            | GramTyTup [GramTy]
            | GramTyNamed Text [GramTy]
            | GramTyVar Text
            deriving (Eq, Show)

data GramTyF a = GramTySimpleF (BasicType (Maybe Integer))
               | GramTyTupF [a]
               | GramTyNamedF Text [a]
               | GramTyVarF Text
               deriving (Functor, Show)

type instance F.Base GramTy = GramTyF

instance F.Foldable GramTy where
  project (GramTySimple x)   = GramTySimpleF x
  project (GramTyTup xs)     = GramTyTupF xs
  project (GramTyNamed n xs) = GramTyNamedF n xs
  project (GramTyVar n)      = GramTyVarF n

instance F.Unfoldable GramTy where
  embed (GramTySimpleF x)   = GramTySimple x
  embed (GramTyTupF xs)     = GramTyTup xs
  embed (GramTyNamedF n xs) = GramTyNamed n xs
  embed (GramTyVarF n)      = GramTyVar n

data CGTy = CGTySimple (BasicType Integer)
          | CGTyTup [CGTy]
          | CGTyNamed Text [CGTy]
          deriving (Eq, Show)

data CGTyF a = CGTySimpleF (BasicType Integer)
             | CGTyTupF [a]
             | CGTyNamedF Text [a]
             deriving (Functor, Show)

type instance F.Base CGTy = CGTyF

instance F.Foldable CGTy where
  project (CGTySimple x) = CGTySimpleF x
  project (CGTyTup xs) = CGTyTupF xs
  project (CGTyNamed n xs) = CGTyNamedF n xs

instance F.Unfoldable CGTy where
  embed (CGTySimpleF x) = CGTySimple x
  embed (CGTyTupF xs) = CGTyTup xs
  embed (CGTyNamedF n xs) = CGTyNamed n xs

instance SeqTypable GramTy where
  tyOf = F.cata alg
    where alg (GramTySimpleF x) = tyOf x
          alg (GramTyTupF xs) = TyTup xs
          alg (GramTyNamedF n xs) = TyNamed n xs
          alg (GramTyVarF x)  = TyVar x

gramTypeDepends :: GramTy -> S.Set Text
gramTypeDepends = F.cata alg
  where alg (GramTyNamedF n xs) = S.insert n (S.unions xs)
        alg (GramTyTupF xs) = S.unions xs
        alg _ = S.empty

gTypeParamOne :: GIntParamOne -> Maybe Integer
gTypeParamOne (GIntParam1 i) = Just (parseAnyInt i)
gTypeParamOne (GIntParam1Empty) = Nothing

gTypeParamTwo :: GIntParamTwo -> (Maybe Integer, Maybe Integer)
gTypeParamTwo (GIntParam2 iLow iHigh) = (Just (parseAnyInt iLow), Just (parseAnyInt iHigh))
gTypeParamTwo (GIntParam2Lower iLow) = (Just (parseAnyInt iLow), Nothing)
gTypeParamTwo (GIntParam2Upper iHigh) = (Nothing, Just (parseAnyInt iHigh))
gTypeParamTwo (GIntParam2Empty) = (Nothing, Nothing)

gToType :: GType -> GramTy
gToType GTUnit = GramTySimple BTUnit
gToType (GTBit constraint) = GramTySimple $ BTBit (gTypeParamOne constraint)
gToType GTBool = GramTySimple BTBool
gToType (GTBuffer constraint) = GramTySimple $ uncurry BTBuffer (gTypeParamTwo constraint)
gToType (GTInt constraint) = GramTySimple $ uncurry BTInt (gTypeParamTwo constraint)
gToType (GTNamed (UIdent n) GTNamedNoParam) = GramTyNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) =
  GramTyNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = GramTyVar (pack s)
gToType (GTTup x xs) = GramTyTup $ map (\(GTTupArg y) -> gToType y) (x:xs)

