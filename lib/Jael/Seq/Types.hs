module Jael.Seq.Types where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Util

-- A class for types that can be subtypes. isSubtypeOf should return True when
-- the first argument is a subtype of the second.
class Subtypable a where
  isSubtypeOf :: a -> a -> Bool

class HMTypable a where
  hmTyOf :: a -> HMTy

class TIOps a where
  ftv :: a -> S.Set Text
  apply :: M.Map Text HMTy -> a -> a

data BasicType a = BTUnit
                 | BTBit { btBits :: a }
                 | BTBool
                 | BTBuffer { btBufferMin :: a
                           -- , btBufferMax :: a
                            }
                 | BTInt { btIntMin :: a
                         , btIntMax :: a
                         }
                 deriving (Eq, Show)

instance Subtypable (BasicType Integer) where
  (BTUnit) `isSubtypeOf` (BTUnit) = True
  (BTBit{btBits=b1}) `isSubtypeOf` (BTBit{btBits=b2}) = b1 == b2
  (BTBool) `isSubtypeOf` (BTBool) = True
  (BTBuffer{btBufferMin=min1}) `isSubtypeOf` (BTBuffer{btBufferMin=min2}) =
    min1 >= min2
  (BTInt{btIntMin=min1, btIntMax=max1}) `isSubtypeOf`
   (BTInt{btIntMin=min2, btIntMax=max2}) = min1 >= min2 && max1 <= max2
  _ `isSubtypeOf` _ = False

-- The stage 1 type most closely reflects the types in the code

data S1Ty = S1TySimple (BasicType (Maybe Integer))
          | S1TyTup [S1Ty]
          | S1TyNamed Text [S1Ty]
          | S1TyVar Text
          deriving (Eq, Show)

data S1TyF a = S1TySimpleF (BasicType (Maybe Integer))
             | S1TyTupF [a]
             | S1TyNamedF Text [a]
             | S1TyVarF Text
             deriving (Functor, Show)

type instance F.Base S1Ty = S1TyF

instance F.Foldable S1Ty where
  project (S1TySimple x)   = S1TySimpleF x
  project (S1TyTup xs)     = S1TyTupF xs
  project (S1TyNamed n xs) = S1TyNamedF n xs
  project (S1TyVar n)      = S1TyVarF n

instance F.Unfoldable S1Ty where
  embed (S1TySimpleF x)   = S1TySimple x
  embed (S1TyTupF xs)     = S1TyTup xs
  embed (S1TyNamedF n xs) = S1TyNamed n xs
  embed (S1TyVarF n)      = S1TyVar n

instance HMTypable S1Ty where
  hmTyOf = F.cata alg
    where alg (S1TySimpleF BTUnit)     = HMTyUnit
          alg (S1TySimpleF BTBit{})    = HMTyBit
          alg (S1TySimpleF BTBool)     = HMTyBool
          alg (S1TySimpleF BTBuffer{}) = HMTyBuffer
          alg (S1TySimpleF BTInt{})    = HMTyInt
          alg (S1TyTupF xs)            = HMTyTup xs
          alg (S1TyNamedF n xs)        = HMTyNamed n xs
          alg (S1TyVarF n)             = HMTyVar n

-- The structure used for type inference is an intermediate structure between
-- stage 1 and stage 2

data HMTy = HMTyUnit
          | HMTyBool
          | HMTyBit
          | HMTyBuffer
          | HMTyInt
          | HMTyVar Text
          | HMTyTup [HMTy]
          | HMTyNamed Text [HMTy]
          | HMTyFun HMTy HMTy
          deriving (Eq, Show)

data HMTyF a = HMTyUnitF
             | HMTyIntF
             | HMTyBoolF
             | HMTyBitF
             | HMTyBufferF
             | HMTyVarF Text
             | HMTyTupF [a]
             | HMTyNamedF Text [a]
             | HMTyFunF a a
             deriving (Show, Functor)

type instance F.Base HMTy = HMTyF

instance F.Foldable HMTy where
  project (HMTyUnit)      = HMTyUnitF
  project (HMTyInt)       = HMTyIntF
  project (HMTyBool)      = HMTyBoolF
  project (HMTyBit)       = HMTyBitF
  project (HMTyBuffer)    = HMTyBufferF
  project (HMTyVar x)     = HMTyVarF x
  project (HMTyTup x)     = HMTyTupF x
  project (HMTyNamed x y) = HMTyNamedF x y
  project (HMTyFun x y)   = HMTyFunF x y

instance F.Unfoldable HMTy where
  embed (HMTyUnitF)      = HMTyUnit
  embed (HMTyIntF)       = HMTyInt
  embed (HMTyBoolF)      = HMTyBool
  embed (HMTyBitF)       = HMTyBit
  embed (HMTyBufferF)    = HMTyBuffer
  embed (HMTyVarF x)     = HMTyVar x
  embed (HMTyTupF x)     = HMTyTup x
  embed (HMTyNamedF x y) = HMTyNamed x y
  embed (HMTyFunF x y)   = HMTyFun x y

instance HMTypable HMTy where
  hmTyOf = id

instance TIOps HMTy where
  ftv = F.cata alg
    where alg (HMTyVarF t)      = S.singleton t
          alg (HMTyNamedF _ ts) = S.unions ts
          alg (HMTyTupF ts)     = S.unions ts
          alg (HMTyFunF t1 t2)  = t1 `S.union` t2
          alg _                 = S.empty

  apply s = F.cata alg
    where alg t@(HMTyVarF v) = M.findWithDefault (F.embed t) v s
          alg t = F.embed t

data HMPolyTy = HMPolyTy [Text] HMTy
              deriving (Show)

instance TIOps HMPolyTy where
  -- Free type variables of a type scheme are the ones not bound by a universal
  -- quantifier. I.e., the type variables within t not in vs
  ftv (HMPolyTy vs t) = ftv t `S.difference` S.fromList vs
  -- This first deletes the variables of the scheme from the substitution then
  -- applies the substitution
  apply s (HMPolyTy vs t) = HMPolyTy vs (apply (foldr M.delete s vs) t)

arityOf :: HMTy -> Integer
arityOf (HMTyFun _ x) = 1 + arityOf x
arityOf _ = 0

-- Converts a list of argument types and a return type to a function
typesToFun :: ([HMTy], HMTy) -> HMTy
typesToFun = uncurry $ flip (foldr HMTyFun)

-- The inverse of typesToFun
funToTypes :: HMTy -> ([HMTy], HMTy)
funToTypes = recFun []
  where recFun acc (HMTyFun x y) = first (x:) (recFun acc y)
        recFun acc x = (acc, x)

polyTy :: HMTy -> HMPolyTy
polyTy t = HMPolyTy (S.toList $ ftv t) t

-- The stage 2 type has undergone type inference and constraint checking

data S2Ty = S2TySimple (BasicType Integer)
          | S2TyTup [S2Ty]
          | S2TyNamed Text [S2Ty]
          | S2TyVar Text
          deriving (Eq, Show)

data S2TyF a = S2TySimpleF (BasicType Integer)
             | S2TyTupF [a]
             | S2TyNamedF Text [a]
             | S2TyVarF Text
             deriving (Functor, Show)

type instance F.Base S2Ty = S2TyF

instance F.Foldable S2Ty where
  project (S2TySimple x)   = S2TySimpleF x
  project (S2TyTup xs)     = S2TyTupF xs
  project (S2TyNamed n xs) = S2TyNamedF n xs
  project (S2TyVar x)      = S2TyVarF x

instance F.Unfoldable S2Ty where
  embed (S2TySimpleF x)   = S2TySimple x
  embed (S2TyTupF xs)     = S2TyTup xs
  embed (S2TyNamedF n xs) = S2TyNamed n xs
  embed (S2TyVarF x)      = S2TyVar x

instance HMTypable S2Ty where
  hmTyOf = F.cata alg
    where alg (S2TySimpleF BTUnit)     = HMTyUnit
          alg (S2TySimpleF BTBit{})    = HMTyBit
          alg (S2TySimpleF BTBool)     = HMTyBool
          alg (S2TySimpleF BTBuffer{}) = HMTyBuffer
          alg (S2TySimpleF BTInt{})    = HMTyInt
          alg (S2TyTupF xs)            = HMTyTup xs
          alg (S2TyNamedF n xs)        = HMTyNamed n xs
          alg (S2TyVarF n)             = HMTyVar n

-- Returns true if t1 is a more general type than t2
instance Subtypable S2Ty where
  (S2TySimple t1) `isSubtypeOf` (S2TySimple t2) = t1 `isSubtypeOf` t2
  (S2TyTup t1) `isSubtypeOf` (S2TyTup t2) = all (uncurry isSubtypeOf) (zip t1 t2)
  (S2TyNamed n1 t1) `isSubtypeOf` (S2TyNamed n2 t2) =
    n1 == n2 && all (uncurry isSubtypeOf) (zip t1 t2)
  (S2TyVar _) `isSubtypeOf` _ = error "TODO"
  _ `isSubtypeOf` (S2TyVar _) = error "TODO"
  _ `isSubtypeOf` _ = False

-- Stage 3 does not modify how types are represented
-- Stage 4 removes polymorphism from types

s1TyDepends :: S1Ty -> S.Set Text
s1TyDepends = F.cata alg
  where alg (S1TyNamedF n xs) = S.insert n (S.unions xs)
        alg (S1TyTupF xs) = S.unions xs
        alg _ = S.empty

gTypeParamOne :: GIntParamOne -> Maybe Integer
gTypeParamOne (GIntParam1 i) = Just (parseAnyInt i)
gTypeParamOne (GIntParam1Empty) = Nothing

gTypeParamTwo :: GIntParamTwo -> (Maybe Integer, Maybe Integer)
gTypeParamTwo (GIntParam2 iLow iHigh) = (Just (parseAnyInt iLow), Just (parseAnyInt iHigh))
gTypeParamTwo (GIntParam2Lower iLow) = (Just (parseAnyInt iLow), Nothing)
gTypeParamTwo (GIntParam2Upper iHigh) = (Nothing, Just (parseAnyInt iHigh))
gTypeParamTwo (GIntParam2Empty) = (Nothing, Nothing)

gToType :: GType -> S1Ty
gToType GTUnit = S1TySimple BTUnit
gToType (GTBit constraint) = S1TySimple $ BTBit (gTypeParamOne constraint)
gToType GTBool = S1TySimple BTBool
gToType (GTBuffer constraint) = S1TySimple $ uncurry BTBuffer (gTypeParamTwo constraint)
gToType (GTInt constraint) = S1TySimple $ uncurry BTInt (gTypeParamTwo constraint)
gToType (GTNamed (UIdent n) GTNamedNoParam) = S1TyNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) =
  S1TyNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = S1TyVar (pack s)
gToType (GTTup x xs) = S1TyTup $ map (\(GTTupArg y) -> gToType y) (x:xs)

