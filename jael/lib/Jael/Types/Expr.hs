{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}
{-# Language DeriveDataTypeable #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleContexts #-}

module Jael.Types.Expr where

import           Prelude --hiding ((<+>), (<$>))
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Eq.Deriving (deriveEq1)
--import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
--import qualified Language.Fixpoint.Types as F
import qualified Text.PrettyPrint.Leijen.Text as P
import           Text.Show.Deriving (deriveShow1)

import           Jael.Types.Ann
import           Jael.Types.Type
import           Jael.Util

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
              | CIff
              | CImp
              | CBitCat
              | CNot
              deriving (Data, Eq, Ord, Show)

instance P.Pretty Constant where
  pretty CUnit = "void"
  pretty (CBool True) = "true"
  pretty (CBool False) = "false"
  pretty (CInt (Token i _)) =
    let i' = P.textStrict . T.pack . show . abs $ i
    in  if i >= 0
          then i'
          else P.text "~" <> i'
  pretty CAdd    = P.text "+"
  pretty CSub    = P.text "-"
  pretty CMul    = P.text "*"
  pretty CDiv    = P.text "/"
  pretty CMod    = P.text "%"
  pretty COr     = P.text "||"
  pretty CAnd    = P.text "&&"
  pretty CEq     = P.text "=="
  pretty CNe     = P.text "!="
  pretty CGe     = P.text ">="
  pretty CLe     = P.text "<="
  pretty CGt     = P.text "<"
  pretty CLt     = P.text ">"
  pretty CIff    = P.text "-->"
  pretty CImp    = P.text "<->"
  pretty CBitCat = P.text "#"
  pretty CNot    = P.text "!"

data ExprF a = EAppF a a
             | EAbsF Ident a
             | ELetF Ident a a
             | EIteF a a a
             | ETupF [a]
             | EVarF Ident
             | EConF Constant
             deriving (Data, Eq, Foldable, Functor, Show, Traversable)

type Expr = Fix ExprF

instance P.Pretty Expr where
  pretty = (P.pretty::MaybeTypedExpr -> P.Doc) . cata ([] :<)

-- An annotated expression
type HMTypedExpr     = Ann ExprF TScheme
type MaybeTypedExpr  = Ann ExprF [QType]
type TypedExpr       = Ann ExprF QScheme

deriving instance Data a => Data (Ann ExprF a)

type GlobExpr        = (Ident, MaybeTypedExpr)
type FuncExpr        = (Ident, [(Ident, QType)], QType, MaybeTypedExpr)

pattern MTECon :: Constant -> [QType] -> MaybeTypedExpr
pattern MTECon c mt = mt :< EConF c

pattern MTEVar :: Ident -> [QType] -> MaybeTypedExpr
pattern MTEVar n mt = mt :< EVarF n

-- An expression more suitable for pretty printing
data PExprF a = PAppF PExpr [a]
              | PAbsF [T.Text] a
              | PLetF [(T.Text, a)] a
              | PIteF a a a
              | PTupF [a]
              | PVarF (T.Text, [QType])
              | PConF (Constant, [QType])
  deriving (Functor)

type PExpr = Fix PExprF

pattern PApp :: PExpr -> [PExpr] -> PExpr
pattern PApp a b   = Fix (PAppF a b)

pattern PAbs :: [T.Text] -> PExpr -> PExpr
pattern PAbs a b = Fix (PAbsF a b)

pattern PLet :: [(T.Text, PExpr)] -> PExpr -> PExpr
pattern PLet a b   = Fix (PLetF a b)

pattern PIte :: PExpr -> PExpr -> PExpr -> PExpr
pattern PIte a b c = Fix (PIteF a b c)

pattern PTup :: [PExpr] -> PExpr
pattern PTup a     = Fix (PTupF a)

pattern PVar :: (T.Text, [QType]) -> PExpr
pattern PVar a     = Fix (PVarF a)

pattern PCon :: (Constant, [QType]) -> PExpr
pattern PCon a     = Fix (PConF a)

instance P.Pretty MaybeTypedExpr where
  pretty = combinePExpr . cata alg
    where
      alg :: C.CofreeF ExprF [QType] PExpr -> PExpr

      alg (_  C.:< (EAppF e1 e2)) = case e1 of
        (PApp f as) -> PApp f $ as ++ [e2]
        _           -> PApp e1 [e2]

      alg (_  C.:< (EAbsF (Token n _) e)) = case e of
        (PAbs as e') -> PAbs (n : as) e'
        _            -> PAbs [n] e

      alg (_  C.:< (ELetF (Token n _) e1 e2)) = case e2 of
        (PLet xs e) -> PLet ((n, e1):xs) e
        _           -> PLet [(n, e1)   ] e2

      alg (_  C.:< (EIteF d1 d2 d3)) = PIte d1 d2 d3

      alg (_  C.:< (ETupF ds))          = PTup ds
      alg (mt C.:< (EVarF (Token v _))) = PVar (v, mt)
      alg (mt C.:< (EConF c))           = PCon (c, mt)

      combinePExpr :: PExpr -> P.Doc
      combinePExpr = cata alg'

      alg' :: PExprF P.Doc -> P.Doc
      alg' (PAppF f as)
        | PVar (_, [_]) <- f
        = P.parens (combinePExpr f) <> P.tupled as

      alg' (PAppF f as)
        | PCon (c, _) <- f
        , [a1, a2] <- as
        , c `S.member` infixOps
        = P.parens $ a1 P.<+> P.pretty c P.<+> a2

      alg' (PAppF f as) = (combinePExpr f) <> P.tupled as

      alg' (PLetF xs e) = P.semiBraces $
        map (\(n,d) -> P.textStrict n P.<+> P.equals P.<+> d) xs ++ [e]

      alg' (PAbsF as e) = P.backslash <> P.tupled as' P.<$> P.indent 2 e
        where as' = map P.textStrict as

      alg' (PIteF b t e) = P.text "if" P.<+> b
                                 P.<$> P.indent 2 t
                                 P.<$> P.text "else"
                                 P.<$> P.indent 2 e

      alg' (PTupF es)   = P.tupled es

      alg' (PVarF (n, mt)) = case mt of
        [] -> P.textStrict n
        ts -> foldr (\t acc-> acc P.<+> P.colon P.<+> P.pretty t)
                    (P.textStrict n) ts

      alg' (PConF (c, mt)) = case mt of
        [] -> P.pretty c
        ts -> foldr (\t acc-> acc P.<+> P.colon P.<+> P.pretty t)
                    (P.pretty c) ts

infixOps :: S.Set Constant
infixOps = S.fromList
  [CAdd, CSub, CMul, CDiv, CMod, COr, CAnd
  ,CEq, CNe, CGe, CLe, CGt, CLt, CBitCat]

--instance Pretty HMTypedExpr where
--  pretty = pretty . cata alg
--    where
--      alg :: C.CofreeF ExprF Type MaybeTypedExpr -> MaybeTypedExpr
--      alg (t C.:< x) = [noQual t] :< x

--instance Pretty TypedExpr where
--  pretty = pretty . cata alg
--    where
--      alg :: C.CofreeF ExprF QType MaybeTypedExpr -> MaybeTypedExpr
--      alg (t C.:< x) = [t] :< x

freeVars :: Expr -> S.Set T.Text
freeVars = S.map value . cata alg
    where alg (EAppF e1 e2) = e1 `S.union` e2
          alg (EAbsF n e) = S.delete n e
          alg (ELetF x e1 e2) = e1 `S.union` S.delete x e2
          alg (EIteF e1 e2 e3) = e1 `S.union` e2 `S.union` e3
          alg (ETupF es) = S.unions es
          alg (EVarF n) = S.singleton n
          alg _ = S.empty

getType :: HMTypedExpr -> Type
getType = schType . extract

-- -- reQual e1 e2 adds refinements from e1 to e2 while retaining the
-- -- types of e2. Both e1 and e2 must have the same structure. As such, serves as
-- -- a sanity check of hm inference.
-- reQual :: MaybeTypedExpr -> HMTypedExpr -> Either T.Text TypedExpr
-- reQual mte hte = return $ joinExprs addQuals mte hte

-- addQuals :: [QType] -> TScheme -> QScheme
-- addQuals qs s = foldr addQuals' (fmap noQual s) qs

-- addQuals' :: QType -> QScheme -> QScheme
-- addQuals' x s = let (Scheme gs is y) = asdf x s
--                 in Scheme gs is $ joinTypes (<>) x y

-- -- Instantiate a type variable in s when that type is known in t
-- asdf :: QType -> QScheme -> QScheme
-- asdf t (Scheme gs is s) = Scheme gs is $ asdf' is t s

-- asdf' :: M.Map T.Text QType -> QType -> QType -> QType
-- asdf' sub x@(_:< x') (r :< TVarF i) =
--   assert (removeAnn (M.findWithDefault undefined (value i) sub) == removeAnn x)
--     $ r :< x'
-- asdf' sub (_ :< TFunF _ t u) (r :< TFunF b' t' u') =
--   r :< TFunF b' (asdf' sub t t') (asdf' sub u u')
-- asdf' _ _ x = x

joinTypes :: (Show a, Show b) => (a -> b -> c) -> Cofree TypeF a -> Cofree TypeF b -> Cofree TypeF c
joinTypes f tA tB = case (tA, tB) of
  (r :< TVarF i, r' :< TVarF i') ->
    assert (i == i') $ f r r' :< TVarF i
  (r :< TFunF b t u, r' :< TFunF b' t' u') ->
    assert (b == b') $ f r r' :< TFunF b (joinTypes f t t') (joinTypes f u u')
  (r :< TConF i ts, r' :< TConF i' ts') ->
    assert (i == i') $ f r r' :< TConF i (zipWith (joinTypes f) ts ts')
  (r :< TTupF ts, r' :< TTupF ts') ->
    f r r' :< TTupF (zipWith (joinTypes f) ts ts')
  (t1, t2) -> error $ "Types' structures don't match\n" ++ show t1 ++ "\n" ++ show t2

joinExprs :: (a -> b -> c) -> Cofree ExprF a -> Cofree ExprF b -> Cofree ExprF c
joinExprs f exA exB = case (exA, exB) of
  (x :< EAppF a b, y :< EAppF a' b') ->
    f x y :< EAppF (joinExprs f a a') (joinExprs f b b')
  (x :< EAbsF i a, y :< EAbsF i' a') ->
    assert (i == i') $ f x y :< EAbsF i (joinExprs f a a')
  (x :< ELetF i a b, y :< ELetF i' a' b') ->
    assert (i == i') $ f x y :< ELetF i (joinExprs f a a') (joinExprs f b b')
  (x :< EIteF a b c, y :< EIteF a' b' c') ->
    f x y :< EIteF (joinExprs f a a') (joinExprs f b b') (joinExprs f c c')
  (x :< ETupF as, y :< ETupF as') ->
    f x y :< ETupF (zipWith (joinExprs f) as as')
  (x :< EVarF i, y :< EVarF i') ->
    assert (i == i') $ f x y :< EVarF i
  (x :< EConF c, y :< EConF c') ->
    assert (c == c') $ f x y :< EConF c
  (_, _) -> error "Expressions' structures don't match"

--dispSubExprs :: TypedExpr -> [String]
--dispSubExprs a = do
--  let b = duplicate a :: Cofree ExprF (Cofree ExprF QScheme)
--  let f1 te@(t :< _) fb =
--        let x :: [String]
--            x = case fb of
--                  EAppF e1 e2 -> e1 ++ e2
--                  EAbsF _ e -> e
--                  ELetF _ e1 e2 -> e1 ++ e2
--                  EVarF _ -> []
--                  EConF _ -> []
--                  EIteF u v w -> u ++ v ++ w
--                  ETupF xs -> concat xs
--        in  (show (pretty t) ++ "\n" ++ show (pretty $ removeAnn te)) : x
--  iterCofree f1 b

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)
