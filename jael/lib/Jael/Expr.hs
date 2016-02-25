module Jael.Expr where

import qualified Data.Functor.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import           Jael.Type
import           Jael.Util

type Ident = T.Text

data Constant = CUnit
              | CInt Integer
              | CBool Bool
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
              | CNot
              | CBitCat
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

instance F.Foldable Expr where
  project (EApp x y)     = EAppF x y
  project (EAbs x y)     = EAbsF x y
  project (ELet x y z)   = ELetF x y z
  project (EIte x y z)   = EIteF x y z
  project (EVar x)       = EVarF x
  project (ECon x)       = EConF x

instance F.Unfoldable Expr where
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
data TypedExpr t = TypedExpr (Ann t ExprF (TypedExpr t))
  deriving (Eq, Show)

data TypedExprF t a = TypedExprF (Ann t ExprF a)
  deriving (Eq, Functor, Show)

type instance F.Base (TypedExpr t) = TypedExprF t

instance F.Foldable (TypedExpr t) where
  project (TypedExpr Ann {ann=t, unAnn=e}) = TypedExprF Ann {ann=t, unAnn=e}

instance F.Unfoldable (TypedExpr t) where
  embed (TypedExprF Ann {ann=t, unAnn=e}) = TypedExpr Ann {ann=t, unAnn=e}

type MaybeTypedExpr = TypedExpr (Maybe Type)

mkUntypedExpr :: ExprF MaybeTypedExpr -> MaybeTypedExpr
mkUntypedExpr e = TypedExpr $ Ann Nothing e
