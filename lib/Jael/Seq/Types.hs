{-# Language DeriveFunctor, NoImplicitPrelude, TypeFamilies #-}

module Jael.Seq.Types where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar

data Ty = TVar Text
        | TUnit
        | TInt
        | TBool
        | TBit
        | TNamed Text [Ty]
        | TFun Ty Ty
        deriving (Show)

data TyF a = TVarF Text
           | TUnitF
           | TIntF
           | TBoolF
           | TBitF
           | TNamedF Text [a]
           | TFunF a a
           deriving (Show, Functor)

type instance Base Ty = TyF

instance Foldable Ty where
  project (TVar x) = TVarF x
  project (TUnit) = TUnitF
  project (TInt) = TIntF
  project (TBool) = TBoolF
  project (TBit) = TBitF
  project (TNamed x y) = TNamedF x y
  project (TFun x y) = TFunF x y

instance Unfoldable Ty where
  embed (TVarF x) = TVar x
  embed (TUnitF) = TUnit
  embed (TIntF) = TInt
  embed (TBoolF) = TBool
  embed (TBitF) = TBit
  embed (TNamedF x y) = TNamed x y
  embed (TFunF x y) = TFun x y

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

data TyEnv = TyEnv (M.Map Text PolyTy)
  deriving (Show)

type TySub = M.Map Text Ty

class TyOps a where
  ftv :: a -> S.Set Text
  apply :: TySub -> a -> a

instance TyOps Ty where
  ftv = cata ftvFn
    where ftvFn (TVarF t)      = S.singleton t
          ftvFn (TNamedF _ ts) = foldr S.union S.empty ts
          ftvFn (TFunF t1 t2)  = t1 `S.union` t2
          ftvFn _              = S.empty

  apply s = cata applyFn
    where applyFn t@(TVarF v)    = fromMaybe (embed t) (M.lookup v s)
          applyFn t = embed t

instance TyOps PolyTy where
  -- Free type variables of a type scheme are the ones not bound by a universal
  -- quantifier. I.e., the type variables within t not in vs
  ftv (PolyTy vs t) = ftv t `S.difference` S.fromList vs
  -- This first deletes the variables of the scheme from the substitution then
  -- applies the substitution
  apply s (PolyTy vs t) = PolyTy vs (apply (foldr M.delete s vs) t)

instance TyOps a => TyOps [a] where
  ftv = foldr (S.union . ftv) S.empty
  apply s = map (apply s)

instance TyOps TyEnv where
  ftv (TyEnv env) = ftv $ M.elems env
  apply sub (TyEnv env) = TyEnv $ M.map (apply sub) env

-- Two PolyTy are equivalent when their structure is the same and there exists a
-- one-to-one mapping of the type variables of both types to each other.
-- a -> b -> b is equivalent to b -> c -> c because the substituion a->b; b->c
-- makes the first into the second and b->a; c->b makes the second.
tyEquiv :: Ty -> Ty -> Bool
tyEquiv t u =
    case mkSub M.empty t u of
         Nothing -> False
         Just s  -> apply s t `identical` u
    where mkSub :: TySub -> Ty -> Ty -> Maybe TySub
          mkSub sub (TVar a) b@(TVar bname) =
            case M.lookup a sub of
                 Just (TVar b'name) -> if bname == b'name
                                          then Just sub
                                          else Nothing
                 _ -> Just (M.insert a b sub)
          mkSub sub (TNamed n as) (TNamed m bs) =
            if n == m && length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TFun a a') (TFun b b') =
            case mkSub sub a b of
                 Just sub' -> mkSub sub' a' b'
                 Nothing -> Nothing
          mkSub sub a b = if a `identical` b
                             then Just sub
                             else Nothing

          identical :: Ty -> Ty -> Bool
          identical (TVar a) (TVar b) = a == b
          identical (TUnit) (TUnit) = True
          identical (TInt) (TInt) = True
          identical (TBool) (TBool) = True
          identical (TBit) (TBit) = True
          identical (TNamed n ts) (TNamed m us) = n == m &&
                                                  length ts == length us &&
                                                  and (zipWith identical ts us)
          identical (TFun w x) (TFun y z) = w `identical` y && x `identical` z
          identical _ _ = False


polyEquiv :: PolyTy -> PolyTy -> Bool
polyEquiv (PolyTy _ t) (PolyTy _ u) = t `tyEquiv` u

gToType :: GType -> Ty
gToType GTInt = TInt
gToType GTBool = TBool
gToType (GTUnit GUnit) = TUnit
gToType (GTNamed (UIdent n) GTNamedNoParam) = TNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) = TNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = TVar (pack s)
gToType (GTTup xs) = TNamed ("Tup" ++ tshow (length xs+1))
                            (map (\(GTTupArg x) -> gToType x) xs)

