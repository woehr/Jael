module Jael.Seq.HM_Types where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S

data SimpleType = TyUnit
                | TyInt
                | TyBool
                | TyBit
                | TyBuffer
                deriving (Eq, Show)

data Ty = TySimple SimpleType
        | TyVar Text
        | TyTup [Ty]
        | TyNamed Text [Ty]
        | TyFun Ty Ty
        deriving (Eq, Show)

data TyF a = TySimpleF SimpleType
           | TyVarF Text
           | TyTupF [a]
           | TyNamedF Text [a]
           | TyFunF a a
           deriving (Show, Functor)

type instance F.Base Ty = TyF

instance F.Foldable Ty where
  project (TySimple x) = TySimpleF x
  project (TyVar x) = TyVarF x
  project (TyTup x) = TyTupF x
  project (TyNamed x y) = TyNamedF x y
  project (TyFun x y) = TyFunF x y

instance F.Unfoldable Ty where
  embed (TySimpleF x) = TySimple x
  embed (TyVarF x) = TyVar x
  embed (TyTupF x) = TyTup x
  embed (TyNamedF x y) = TyNamed x y
  embed (TyFunF x y) = TyFun x y

class SeqTypable a where
  tyOf :: a -> Ty

instance SeqTypable Ty where
  tyOf = id

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

newtype TyEnv = TyEnv { toMap :: M.Map Text PolyTy }
  deriving (Show)

type TySub = M.Map Text Ty

class TyOps a where
  ftv :: a -> S.Set Text
  apply :: TySub -> a -> a

instance TyOps Ty where
  ftv = F.cata ftvFn
    where ftvFn (TyVarF t)     = S.singleton t
          ftvFn (TyNamedF _ ts) = S.unions ts
          ftvFn (TyTupF ts)     = S.unions ts
          ftvFn (TyFunF t1 t2)  = t1 `S.union` t2
          ftvFn _              = S.empty

  apply s = F.cata applyFn
    where applyFn t@(TyVarF v) = M.findWithDefault (F.embed t) v s
          applyFn t = F.embed t

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

identical :: Ty -> Ty -> Bool
identical (TyVar a) (TyVar b) = a == b
identical (TySimple x) (TySimple y) = x == y
identical (TyTup as) (TyTup bs) = length as == length bs &&
                                    and (zipWith identical as bs)
identical (TyNamed n ts) (TyNamed m us) = n == m &&
                                          length ts == length us &&
                                          and (zipWith identical ts us)
identical (TyFun w x) (TyFun y z) = w `identical` y && x `identical` z
identical _ _ = False

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
          mkSub sub (TyVar a) b@(TyVar bname) =
            case M.lookup a sub of
                 Just (TyVar b'name) -> if bname == b'name
                                          then Just sub
                                          else Nothing
                 _ -> Just (M.insert a b sub)
          mkSub sub (TyNamed n as) (TyNamed m bs) =
            if n == m && length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TyTup as) (TyTup bs) =
            if length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TyFun a a') (TyFun b b') =
            case mkSub sub a b of
                 Just sub' -> mkSub sub' a' b'
                 Nothing -> Nothing
          mkSub sub a b = if a `identical` b
                             then Just sub
                             else Nothing

polyEquiv :: PolyTy -> PolyTy -> Bool
polyEquiv (PolyTy _ t) (PolyTy _ u) = t `tyEquiv` u

-- Return the type variables of a type
typeVars :: Ty -> S.Set Text
typeVars = F.cata alg
  where alg :: TyF (S.Set Text) -> S.Set Text
        alg (TyVarF x)   = S.singleton x
        alg (TyNamedF _ ts) = S.unions ts
        alg (TyTupF ts) = S.unions ts
        alg (TyFunF x y) = x `S.union` y
        alg _           = S.empty

typeVars' :: Ty -> [Text]
typeVars' = S.toList . typeVars

typeVars'' :: [Ty] -> [Text]
typeVars'' = join . map typeVars'

polyTy :: Ty -> PolyTy
polyTy t = PolyTy (typeVars' t) t

arityOf :: Ty -> Integer
arityOf (TyFun _ x) = 1 + arityOf x
arityOf _ = 0

-- Converts a list of argument types and a return type to a function
typesToFun :: ([Ty], Ty) -> Ty
typesToFun = uncurry $ flip (foldr TyFun)

-- The inverse of typesToFun
funToTypes :: Ty -> ([Ty], Ty)
funToTypes = recFun []
  where recFun acc (TyFun x y) = first (x:) (recFun acc y)
        recFun acc x = (acc, x)

