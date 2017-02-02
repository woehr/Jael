{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeSynonymInstances #-}

module Jael.Expr where

-- To make liquid haskell happy
import           Prelude ()
import           BasePrelude hiding ((<>), (<$>), (<+>))

import           Control.Comonad hiding ((<$>))
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Bifunctor.TH (deriveBifunctor)
import           Data.Functor.Foldable
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.PrettyPrint.Leijen.Text

import           Jael.Type
import           Jael.Util
import           Jael.Util.Ann

data Constant = CUnit
              | CBool Bool
              | CInt IntConst
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
              deriving (Eq, Ord, Show)

instance Pretty Constant where
  pretty CUnit = "void"
  pretty (CBool True) = "true"
  pretty (CBool False) = "false"
  pretty (CInt (Token i _)) =
    let i' = textStrict . T.pack . show . abs $ i
    in  if i >= 0
          then i'
          else text "~" <> i'
  pretty CAdd    = text "+"
  pretty CSub    = text "-"
  pretty CMul    = text "*"
  pretty CDiv    = text "/"
  pretty CMod    = text "%"
  pretty COr     = text "||"
  pretty CAnd    = text "&&"
  pretty CEq     = text "=="
  pretty CNe     = text "!="
  pretty CGe     = text ">="
  pretty CLe     = text "<="
  pretty CGt     = text "<"
  pretty CLt     = text ">"
  pretty CBitCat = text "#"
  pretty CNot    = text "!"

data ExprF b a = EAppF a a
               | EAbsF b a
               | ELetF Ident a a
               | EIteF a a a
               | ETupF [a]
               | EVarF Ident
               | EConF Constant
               deriving (Eq, Functor, Show)

type Expr = Fix (ExprF Ident)

instance Pretty Expr where
  pretty = pretty . cata alg
    where
      alg :: ExprF Ident MaybeTypedExpr -> MaybeTypedExpr
      alg x = Nothing :< (first (\n->(n, Nothing)) x)

-- An annotated expression
type MaybeTypedExpr  = Ann (ExprF (Ident, Maybe QType)) (Maybe QType)
type MaybeTypedExprF = ExprF (Ident, Maybe QType)
type TypedExpr       = Ann (ExprF Ident) QType
type GlobExpr        = (Ident, MaybeTypedExpr)
type FuncExpr        = (Ident, [(Ident, QType)], QType, MaybeTypedExpr)

pattern MTECon :: Constant -> Maybe QType -> MaybeTypedExpr
pattern MTECon c mt = mt :< EConF c

pattern MTEVar :: Ident -> Maybe QType -> MaybeTypedExpr
pattern MTEVar n mt = mt :< EVarF n

-- An expression more suitable for pretty printing
data PExprF a = PAppF PExpr [a]
              | PAbsF [(T.Text, Maybe QType)] a
              | PLetF [(T.Text, a)] a
              | PIteF a a a
              | PTupF [a]
              | PVarF (T.Text, Maybe QType)
              | PConF (Constant, Maybe QType)
  deriving (Functor)

type PExpr = Fix PExprF

pattern PApp :: PExpr -> [PExpr] -> PExpr
pattern PApp a b   = Fix (PAppF a b)

pattern PAbs :: [(T.Text, Maybe QType)] -> PExpr -> PExpr
pattern PAbs a b   = Fix (PAbsF a b)

pattern PLet :: [(T.Text, PExpr)] -> PExpr -> PExpr
pattern PLet a b   = Fix (PLetF a b)

pattern PIte :: PExpr -> PExpr -> PExpr -> PExpr
pattern PIte a b c = Fix (PIteF a b c)

pattern PTup :: [PExpr] -> PExpr
pattern PTup a     = Fix (PTupF a)

pattern PVar :: (T.Text, Maybe QType) -> PExpr
pattern PVar a     = Fix (PVarF a)

pattern PCon :: (Constant, Maybe QType) -> PExpr
pattern PCon a     = Fix (PConF a)

instance Pretty MaybeTypedExpr where
  pretty = combinePExpr . cata alg
    where
      alg :: C.CofreeF MaybeTypedExprF (Maybe QType) PExpr -> PExpr

      alg (_  C.:< (EAppF e1 e2)) = case e1 of
        (PApp f as) -> PApp f $ as ++ [e2]
        _           -> PApp e1 [e2]

      alg (_  C.:< (EAbsF ((Token n _), mt) e)) = case e of
        (PAbs as e') -> PAbs ((n, mt) : as) e'
        _            -> PAbs [(n, mt)] e

      alg (_  C.:< (ELetF (Token n _) e1 e2)) = case e2 of
        (PLet xs e) -> PLet ((n, e1):xs) e
        _           -> PLet [(n, e1)   ] e2

      alg (_  C.:< (EIteF d1 d2 d3)) = PIte d1 d2 d3

      alg (_  C.:< (ETupF ds))          = PTup ds
      alg (mt C.:< (EVarF (Token v _))) = PVar (v, mt)
      alg (mt C.:< (EConF c))           = PCon (c, mt)

      combinePExpr :: PExpr -> Doc
      combinePExpr = cata alg'

      alg' :: PExprF Doc -> Doc
      alg' (PAppF f as)
        | PVar (_, Just _) <- f
        = parens (combinePExpr f) <> tupled as

      alg' (PAppF f as)
        | PCon (c, _) <- f
        , [a1, a2] <- as
        , c `S.member` infixOps
        = parens $ a1 <+> pretty c <+> a2

      alg' (PAppF f as) = (combinePExpr f) <> tupled as

      alg' (PLetF xs e) = semiBraces $
        map (\(n,d)->textStrict n <+> equals <+> d) xs ++ [e]

      alg' (PAbsF as e) = backslash <> tupled as' <+> text "->" <$> indent 2 e
        where as' = map (\(n, mt) -> case mt of
                            Just t -> textStrict n <+> colon <+> pretty t
                            _      -> textStrict n
                        ) as

      alg' (PIteF b t e) = text "if" <+> b
                                 <$> indent 2 t
                                 <$> text "else"
                                 <$> indent 2 e

      alg' (PTupF es)   = tupled es

      alg' (PVarF (n, mt)) = case mt of
        Just t -> textStrict n <+> colon <+> pretty t
        _      -> textStrict n

      alg' (PConF (c, mt)) = case mt of
        Just t@(Just _ :< _) -> pretty c <+> colon <+> pretty t
        _                    -> pretty c

infixOps :: S.Set Constant
infixOps = S.fromList
  [CAdd, CSub, CMul, CDiv, CMod, COr, CAnd
  ,CEq, CNe, CGe, CLe, CGt, CLt, CBitCat]

instance Pretty TypedExpr where
  pretty = pretty . cata alg
    where
      alg :: C.CofreeF (ExprF Ident) QType MaybeTypedExpr
                   -> MaybeTypedExpr
      alg (t C.:< x) = Just t :< (first (\n->(n,Nothing)) x)

freeVars :: Expr -> S.Set T.Text
freeVars = S.map value . cata alg
    where alg (EAppF e1 e2) = e1 `S.union` e2
          alg (EAbsF n e) = S.delete n e
          alg (ELetF x e1 e2) = e1 `S.union` S.delete x e2
          alg (EIteF e1 e2 e3) = e1 `S.union` e2 `S.union` e3
          alg (ETupF es) = S.unions es
          alg (EVarF n) = S.singleton n
          alg _ = S.empty

getType :: TypedExpr -> Type
getType = removeAnn . extract

$(deriveBifunctor ''ExprF)
