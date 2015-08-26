{-# Language NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

module Jael.Seq.AST where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S

data Ex = EVar Text
        | EUnit
        | EInt Integer
        | EBool Bool
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data Ty = TVar Text
        | TUnit
        | TInt
        | TBool
        | TNamed Text [Ty]
        | TFun Ty Ty
          deriving (Eq, Show)

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

type TyEnv = M.Map Text PolyTy

type TySub = M.Map Text Ty

class TyOps a where
  ftv :: a -> S.Set Text
  apply :: TySub -> a -> a

instance TyOps Ty where
  ftv (TVar t)      = S.singleton t
  ftv TUnit         = S.empty
  ftv TInt          = S.empty
  ftv TBool         = S.empty
  ftv (TNamed _ ts) = ftv ts
  ftv (TFun t1 t2)  = ftv t1 `S.union` ftv t2

  apply s t@(TVar v)    = fromMaybe t (M.lookup v s)
  apply s (TNamed n ts) = TNamed n (apply s ts)
  apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
  apply _ t = t

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
  ftv env = ftv $ M.elems env
  apply sub = M.map (apply sub)

-- Two PolyTy are equivalent when their structure is the same and there exists a
-- one-to-one mapping of the type variables of both types to each other.
-- a -> b -> b is equivalent to b -> c -> c because the substituion a->b; b->c
-- makes the first into the second and b->a; c->b makes the second.
tyEquiv :: Ty -> Ty -> Bool
tyEquiv t u =
    case mkSub M.empty t u of
         Nothing -> False
         Just s  -> apply s t == u
    where mkSub :: TySub -> Ty -> Ty -> Maybe TySub
          mkSub sub (TVar a) b@(TVar _) =
            case M.lookup a sub of
                 Just b' -> if b == b'
                               then Just sub
                               else Nothing
                 Nothing -> Just (M.insert a b sub)
          mkSub sub (TNamed n as) (TNamed m bs) =
            if n == m && length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TFun a a') (TFun b b') =
            case mkSub sub a b of
                 Just sub' -> mkSub sub' a' b'
                 Nothing -> Nothing
          mkSub sub a b = if a == b
                             then Just sub
                             else Nothing

polyEquiv :: PolyTy -> PolyTy -> Bool
polyEquiv (PolyTy _ t) (PolyTy _ u) = t `tyEquiv` u

