{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Jael.TH (mkConBoilerplate, mkVariantInstances) where

import Jael.Prelude.Minimal hiding (length)

import Control.Exception     (assert)
import Data.Eq.Deriving
import Data.Int              (Int)
import Data.List             (init, last, length, splitAt)
import Haskus.Utils.VariantF
import Language.Haskell.TH
import Text.Show.Deriving


mkConBoilerplate :: Name -> Q [Dec]
mkConBoilerplate tyName = do
  bs <- reify tyName >>= \case
          TyConI (DataD _ _ bs _ _ _) -> return bs
          TyConI (NewtypeD _ _ bs _ _ _) -> return bs
          _ -> error "mkConBoilderplate: Only expected data or newtype"

  -- List of type variables of the data decl as Type
  let tvs = fmap tyVarBndrToType bs
  -- The type of the data decl
  let dataTy = foldl' AppT (ConT tyName) tvs
  -- Partially applied type constructor suitable for deriving Functor
  let dataTyPartial = foldl' AppT (ConT tyName) (init tvs)

  let derivDecs = fmap (mkStandalone (dataTy, tvs) . ConT . mkName)
        ["Eq", "Show", "Typeable"]

  -- Get warnings when: deriving instance (Generic a) => Generic (F a)
  let derivDecs' = fmap (mkStandaloneNoCtx dataTy . ConT . mkName)
        ["Generic"]

  let kToKInstances = fmap (mkStandaloneNoCtx dataTyPartial . ConT . mkName)
        ["Foldable", "Functor", "Traversable"]

  let instanceDecs = fmap ((\cls -> InstanceD Nothing (fmap (AppT cls) tvs) (AppT cls dataTy) []) . ConT . mkName)
        ["Data.TreeDiff.ToExpr"]

  lift1Decs <- concat <$> sequence [deriveShow1 tyName, deriveEq1 tyName]

  return $ derivDecs <> derivDecs' <> kToKInstances <> instanceDecs <> lift1Decs

  where
    tyVarBndrToType :: TyVarBndr -> Type
    tyVarBndrToType (PlainTV n)    = VarT n
    tyVarBndrToType (KindedTV n k) = SigT (VarT n) k

    mkStandaloneNoCtx :: Type -> Type -> Dec
    mkStandaloneNoCtx dataTy cls = mkStandalone (dataTy, []) cls

    mkStandalone :: (Type, [Type]) -> Type -> Dec
    mkStandalone (dataTy, tvs) cls =
      StandaloneDerivD Nothing (fmap (AppT cls) tvs) (AppT cls dataTy)

mkVariantInstances :: Name -> Q [Dec]
mkVariantInstances n = do
  (ClassI (ClassD _ cls tvs _ decs) _) <- reify n
  let tvKind = case tvs of
        (KindedTV _ x : _) -> x
        _                  -> error "Unexpected match failure."
  let instArity = arity tvKind
  let instKind = length tvs
  assert (instKind >= 1) $ return ()
  assert (instArity == 1 || instArity == 0) $ return ()
  clsToInst cls instKind instArity decs

  where
    clsToInst :: Name -> Int -> Int -> [Dec] -> Q [Dec]
    clsToInst cls k a sigs = do
      let clsT = ConT cls
      aTv <- freshTv "a"
      let addVar :: Type -> Type
          addVar t = if a == 0 then AppT t aTv else t
      baseT <- addVar <$> [t| (VariantF '[]) |]
      clsTvs <- traverse (freshTv . ("a"<>) . show) [1..k-1]
      let baseInstT = myAppT clsT (baseT:clsTvs)
      baseCaseFns <- traverse baseCaseSigToExp (acceptableSigsOnly sigs)
      let baseInst = mkInst [] baseInstT baseCaseFns


      xT <- freshTv "x"
      xs'T <- freshTv "xs'"
      let xsT = myAppT PromotedConsT [xT, xs'T]
      recT <- addVar <$> [t| VariantF $(return xsT)  |]

      let ctx1 = myAppT clsT (addVar xT   : clsTvs)
      let ctx2 = myAppT clsT $ addVar (myAppT (ConT (mkName "VariantF")) [xs'T]) : clsTvs
      let recInstT = myAppT clsT (recT:clsTvs)
      recCaseFns <- traverse recCaseSigToExp (acceptableSigsOnly sigs)

      let recInst = mkInst [ctx1, ctx2] recInstT recCaseFns
      return [baseInst, recInst]

baseCaseSigToExp :: Dec -> Q (Name, [Pat], Exp)
baseCaseSigToExp (SigD fn _) = do
  baseBody <- [| error "Empty variant" |]
  return (fn, [], baseBody)
baseCaseSigToExp _ = error "Base case: Expected signatures only"

recCaseSigToExp :: Dec -> Q (Name, [Pat], Exp)
recCaseSigToExp (SigD fn fnT) = do
  let sigArity = arity fnT :: Int
  args <- traverse (newName . ("v" <>) . show) [1..sigArity]
  let (xs, variantArg) = case splitAt (sigArity - 1) args of
        (xs', [variantArg']) -> (fmap VarE xs', VarE variantArg')
        _ -> error $ "Expected at least one argument in class function " <> show fn
  (caseVarP, caseVarE) <- freshVar "x"
  recBody <- [| case popVariantFHead $(return variantArg) of
                  Right $(return caseVarP) -> $(return $ appFn fn (xs <> [caseVarE]))
                  Left  $(return caseVarP) -> $(return $ appFn fn (xs <> [caseVarE]))
             |]
  return (fn, fmap VarP args, recBody)
recCaseSigToExp _ = error "Recursive case: Expected signatures only"

acceptableSigsOnly :: [Dec] -> [Dec]
acceptableSigsOnly = filter (\case
  (SigD _ fnT) -> arity fnT > 0 && isVarT (last (init (fnArgs fnT)))
  _ -> False)

mkInst :: [Type] -> Type -> [(Name, [Pat], Exp)] -> Dec
mkInst ctx instT fns = InstanceD Nothing ctx instT $ flip fmap fns $ \(n,ps,e) -> mkFunDec n ps e

mkFunDec :: Name -> [Pat] -> Exp -> Dec
mkFunDec fn pats body = FunD fn [Clause pats (NormalB body) []]

appFn :: Name -> [Exp] -> Exp
appFn fn = foldl' AppE (VarE fn)

freshVar :: String -> Q (Pat, Exp)
freshVar x = do
  x' <- newName x
  return (VarP x', VarE x')

freshTv :: String -> Q Type
freshTv x = VarT <$> newName x

myAppT :: Type -> [Type] -> Type
myAppT f = foldl' AppT f

arity :: Type -> Int
arity = \case
  ForallT _ _ t          -> arity t
  AppT (AppT ArrowT _) t -> arity t + 1
  _                      -> 0

fnArgs :: Type -> [Type]
fnArgs = \case
  ForallT _ _ t          -> fnArgs t
  AppT (AppT ArrowT x) y -> x : fnArgs y
  t -> [t]

isVarT :: Type -> Bool
isVarT (VarT _)   = True
isVarT (AppT t _) = isVarT t
isVarT _          = False
