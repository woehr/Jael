module Jael.Seq.HM_Types where

import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar

data Ty = TyVar Text
        | TUnit
        | TInt
        | TBool
        | TBit
        | TTup [Ty]
        | TNamed Text [Ty]
        | TFun Ty Ty
        deriving (Eq, Show)

data TyF a = TyVarF Text
           | TUnitF
           | TIntF
           | TBoolF
           | TBitF
           | TTupF [a]
           | TNamedF Text [a]
           | TFunF a a
           deriving (Show, Functor)

type instance F.Base Ty = TyF

instance F.Foldable Ty where
  project (TyVar x) = TyVarF x
  project (TUnit) = TUnitF
  project (TInt) = TIntF
  project (TBool) = TBoolF
  project (TBit) = TBitF
  project (TTup x) = TTupF x
  project (TNamed x y) = TNamedF x y
  project (TFun x y) = TFunF x y

instance F.Unfoldable Ty where
  embed (TyVarF x) = TyVar x
  embed (TUnitF) = TUnit
  embed (TIntF) = TInt
  embed (TBoolF) = TBool
  embed (TBitF) = TBit
  embed (TTupF x) = TTup x
  embed (TNamedF x y) = TNamed x y
  embed (TFunF x y) = TFun x y

class SeqTypable a where
  tyOf :: a -> Ty

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
          ftvFn (TNamedF _ ts) = S.unions ts
          ftvFn (TTupF ts)     = S.unions ts
          ftvFn (TFunF t1 t2)  = t1 `S.union` t2
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
identical (TUnit) (TUnit) = True
identical (TInt) (TInt) = True
identical (TBool) (TBool) = True
identical (TBit) (TBit) = True
identical (TTup as) (TTup bs) = length as == length bs &&
                                  and (zipWith identical as bs)
identical (TNamed n ts) (TNamed m us) = n == m &&
                                        length ts == length us &&
                                        and (zipWith identical ts us)
identical (TFun w x) (TFun y z) = w `identical` y && x `identical` z
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
          mkSub sub (TNamed n as) (TNamed m bs) =
            if n == m && length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TTup as) (TTup bs) =
            if length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (TFun a a') (TFun b b') =
            case mkSub sub a b of
                 Just sub' -> mkSub sub' a' b'
                 Nothing -> Nothing
          mkSub sub a b = if a `identical` b
                             then Just sub
                             else Nothing

polyEquiv :: PolyTy -> PolyTy -> Bool
polyEquiv (PolyTy _ t) (PolyTy _ u) = t `tyEquiv` u

gToType :: GType -> Ty
gToType GTInt = TInt
gToType GTBool = TBool
gToType GTUnit = TUnit
gToType (GTNamed (UIdent n) GTNamedNoParam) = TNamed (pack n) []
gToType (GTNamed (UIdent n) (GTNamedParams xs)) =
  TNamed (pack n) (map (\(GTNamedParam t) -> gToType t) xs)
gToType (GTTVar (LIdent s)) = TyVar (pack s)
gToType (GTTup x xs) = TTup $ map (\(GTTupArg y) -> gToType y) (x:xs)

-- Return the type variables of a type
typeVars :: Ty -> S.Set Text
typeVars = F.cata alg
  where alg :: TyF (S.Set Text) -> S.Set Text
        alg (TyVarF x)   = S.singleton x
        alg (TNamedF _ ts) = S.unions ts
        alg (TTupF ts) = S.unions ts
        alg (TFunF x y) = x `S.union` y
        alg _           = S.empty

typeVars' :: Ty -> [Text]
typeVars' = S.toList . typeVars

typeVars'' :: [Ty] -> [Text]
typeVars'' = join . map typeVars'

polyTy :: Ty -> PolyTy
polyTy t = PolyTy (typeVars' t) t

arityOf :: Ty -> Integer
arityOf (TFun _ x) = 1 + arityOf x
arityOf _ = 0

