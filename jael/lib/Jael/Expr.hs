{-# Language
    DeriveFunctor
  , FlexibleInstances
  , TemplateHaskell
  , TypeFamilies #-}

module Jael.Expr where

-- To make liquid haskell happy
import Prelude ()
import BasePrelude
import qualified Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Development.Placeholders
import           Jael.Type
import           Jael.Util

data Constant = CUnit
              | CBool Bool
              | CInt IntConst
              | CTup
              | CAdd
              | CSub
              | CMul
              | CDiv
              | CMod
              | COr
              | CAnd
              | CEq
              | CNe
              | CGe
              | CLe
              | CGt
              | CLt
              | CBitCat
              | CNot
              deriving (Eq, Show)

data Expr = EApp Expr Expr
          | EAbs Ident Expr
          | ELet Ident Expr Expr
          | EIte Expr Expr Expr
          | EVar Ident
          | ECon Constant
          deriving (Eq, Show)

data ExprF a = EAppF a a
             | EAbsF Ident a
             | ELetF Ident a a
             | EIteF a a a
             | EVarF Ident
             | EConF Constant
             deriving (Eq, Functor, Show)

type instance F.Base Expr = ExprF

instance F.Recursive Expr where
  project (EApp x y)     = EAppF x y
  project (EAbs x y)     = EAbsF x y
  project (ELet x y z)   = ELetF x y z
  project (EIte x y z)   = EIteF x y z
  project (EVar x)       = EVarF x
  project (ECon x)       = EConF x

instance F.Corecursive Expr where
  embed (EAppF x y)     = EApp x y
  embed (EAbsF x y)     = EAbs x y
  embed (ELetF x y z)   = ELet x y z
  embed (EIteF x y z)   = EIte x y z
  embed (EVarF x)       = EVar x
  embed (EConF x)       = ECon x

class SyntaxTree a where
  freeVars :: a -> S.Set Ident

instance SyntaxTree Expr where
  freeVars = F.cata alg
    where alg (EAppF e1 e2) = e1 `S.union` e2
          alg (EAbsF n e) = S.delete n e
          alg (ELetF x e1 e2) = e1 `S.union` S.delete x e2
          alg (EIteF e1 e2 e3) = e1 `S.union` e2 `S.union` e3
          alg (EVarF x) = S.singleton x
          alg _ = S.empty

-- A typed expression type for after type inference is run
-- Note that this data type is suitable for both hm and liquid type inference
-- since types include base types without refinements and refined types
data AnnExpr x = AnnExpr (Ann x ExprF (AnnExpr x))
  deriving (Eq, Show)

data AnnExprF x a = AnnExprF (Ann x ExprF a)
  deriving (Eq, Functor, Show)

type instance F.Base (AnnExpr t) = AnnExprF t

instance F.Recursive (AnnExpr t) where
  project (AnnExpr (Ann t e)) = AnnExprF (Ann t e)

instance F.Corecursive (AnnExpr t) where
  embed (AnnExprF (Ann t e)) = AnnExpr (Ann t e)

type MaybeTypedExpr = AnnExpr (Maybe QType)
type TypedExpr = AnnExpr QType
type NamedExpr = (Ident, MaybeTypedExpr)

mkUntypedExpr :: ExprF MaybeTypedExpr -> MaybeTypedExpr
mkUntypedExpr = AnnExpr . Ann Nothing

mkConstExpr :: Constant -> MaybeTypedExpr
mkConstExpr = mkUntypedExpr . EConF

mkVarExpr :: Ident -> MaybeTypedExpr
mkVarExpr = mkUntypedExpr . EVarF

mkAppExpr :: MaybeTypedExpr -> [MaybeTypedExpr] -> MaybeTypedExpr
mkAppExpr f as = foldl' (\acc e ->  mkUntypedExpr $ EAppF acc e) f as

mkTupExpr :: MaybeTypedExpr -> MaybeTypedExpr -> MaybeTypedExpr
mkTupExpr e1 e2 = mkAppExpr (mkConstExpr CTup) [e1, e2]

mkAnnExpr :: ExprF (AnnExpr a) -> a -> AnnExpr a
mkAnnExpr e a = AnnExpr $ Ann a e

unann :: AnnExpr a -> Expr
unann = F.cata alg
  where alg (AnnExprF (Ann _ (EAppF e1 e2)))   = EApp e1 e2
        alg (AnnExprF (Ann _ (EAbsF n e)))     = EAbs n e
        alg (AnnExprF (Ann _ (ELetF n e1 e2))) = ELet n e1 e2
        alg (AnnExprF (Ann _ (EIteF b t e)))   = EIte b t e
        alg (AnnExprF (Ann _ (EVarF n)))       = EVar n
        alg (AnnExprF (Ann _ (EConF c)))       = ECon c

annOf :: AnnExpr a -> a
annOf (AnnExpr (Ann a _)) = a

data HMTypeErr = HMTypeErr
               deriving (Eq, Show)

hm :: M.Map T.Text Type -> MaybeTypedExpr -> Either HMTypeErr TypedExpr
hm env (AnnExpr (Ann mType (EAppF e1 e2)))   = undefined
hm env (AnnExpr (Ann mType (EAbsF n e)))     = undefined
hm env (AnnExpr (Ann mType (ELetF n e1 e2))) = undefined
hm env (AnnExpr (Ann mType (EIteF b t e)))   = undefined
hm env (AnnExpr (Ann mType (EVarF n)))       = undefined
hm _   (AnnExpr (Ann mType (EConF c)))       =
  let t = case c of
            CUnit   -> TBuiltin BTUnit
            CBool _ -> TBuiltin BTBool
            CInt _  -> TBuiltin BTInt
            CTup    -> undefined
            CAdd    -> undefined
            CSub    -> undefined
            CMul    -> undefined
            CDiv    -> undefined
            CMod    -> undefined
            COr     -> undefined
            CAnd    -> undefined
            CEq     -> undefined
            CNe     -> undefined
            CGe     -> undefined
            CLe     -> undefined
            CGt     -> undefined
            CLt     -> undefined
            CBitCat -> undefined
            CNot    -> undefined
  in case mType of
          Just _  -> $(todo "constants can have a type with refinements")
          Nothing -> Right $ AnnExpr (Ann t (EConF c))
