{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}

module Jael.Expr where

-- To make liquid haskell happy
import           Prelude ()
import           BasePrelude hiding ((<>), (<$>), (<+>))
import           Control.Comonad hiding ((<$>))
import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as C
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

data ExprF a = EAppF a a
             | EAbsF Ident a
             | ELetF Ident a a
             | EIteF a a a
             | ETupF [a]
             | EVarF Ident
             | EConF Constant
             deriving (Eq, Functor, Show)

type Expr = Fix ExprF

instance Pretty Expr where
  pretty = (pretty::MaybeTypedExpr -> Doc) . cata ([] :<)

-- An annotated expression
type HMTypedExpr     = Ann ExprF Type
type MaybeTypedExpr  = Ann ExprF [QType]
type TypedExpr       = Ann ExprF QType

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

instance Pretty MaybeTypedExpr where
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

      combinePExpr :: PExpr -> Doc
      combinePExpr = cata alg'

      alg' :: PExprF Doc -> Doc
      alg' (PAppF f as)
        | PVar (_, [_]) <- f
        = parens (combinePExpr f) <> tupled as

      alg' (PAppF f as)
        | PCon (c, _) <- f
        , [a1, a2] <- as
        , c `S.member` infixOps
        = parens $ a1 <+> pretty c <+> a2

      alg' (PAppF f as) = (combinePExpr f) <> tupled as

      alg' (PLetF xs e) = semiBraces $
        map (\(n,d)->textStrict n <+> equals <+> d) xs ++ [e]

      alg' (PAbsF as e) = backslash <> tupled as' <$> indent 2 e
        where as' = map textStrict as

      alg' (PIteF b t e) = text "if" <+> b
                                 <$> indent 2 t
                                 <$> text "else"
                                 <$> indent 2 e

      alg' (PTupF es)   = tupled es

      alg' (PVarF (n, mt)) = case mt of
        [] -> textStrict n
        ts -> foldr (\t acc-> acc <+> colon <+> pretty t)
                    (textStrict n) ts

      alg' (PConF (c, mt)) = case mt of
        [] -> pretty c
        ts -> foldr (\t acc-> acc <+> colon <+> pretty t)
                    (pretty c) ts

infixOps :: S.Set Constant
infixOps = S.fromList
  [CAdd, CSub, CMul, CDiv, CMod, COr, CAnd
  ,CEq, CNe, CGe, CLe, CGt, CLt, CBitCat]

instance Pretty HMTypedExpr where
  pretty = pretty . cata alg
    where
      alg :: C.CofreeF ExprF Type MaybeTypedExpr -> MaybeTypedExpr
      alg (t C.:< x) = [noQual t] :< x

instance Pretty TypedExpr where
  pretty = pretty . cata alg
    where
      alg :: C.CofreeF ExprF QType MaybeTypedExpr -> MaybeTypedExpr
      alg (t C.:< x) = [t] :< x

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
getType = extract

-- reannotateQual e1 e2 adds refinements from e1 to e2 while retaining the
-- types of e2. Both e1 and e2 must have the same structure. As such, serves as
-- a sanity check of hm inference.
reQual :: MaybeTypedExpr -> HMTypedExpr -> Either T.Text TypedExpr
reQual mte hte = do
  (t, es) <- case (mte, hte) of
               (qts :< e1, hmt :< e2) -> liftA (,(e1,e2)) $ addQuals qts hmt
  q <- case es of
         (EAppF x y,   EAppF x' y')    -> liftA2 EAppF (reQual x x')
                                                       (reQual y y')

         (EAbsF n e,   EAbsF n' e')    -> assert (n==n') $
                                          liftA (EAbsF n) (reQual e e')

         (ELetF n x y, ELetF n' x' y') -> assert (n==n') $
                                          liftA2 (ELetF n) (reQual x x')
                                                           (reQual y y')

         (EIteF b x y, EIteF b' x' y') -> liftA3 EIteF (reQual b b')
                                                       (reQual x x')
                                                       (reQual y y')

         (ETupF xs,    ETupF xs')      -> liftA ETupF $ sequenceA $
                                          map (uncurry reQual) (zip xs xs')

         (EVarF n,     EVarF n')       -> assert (n==n') $ pure $ EVarF n
         (EConF c,     EConF c')       -> assert (c==c') $ pure $ EConF c

         (_,           _)              -> error "Structure of expressions \
                                                \do not match"
  return $ t :< q

addQuals :: [QType] -> Type -> Either T.Text QType
addQuals qs t =
  let t' = cata ([] :<) t
  in  foldM (flip addQual) t' qs

-- Appends refinements from the first type to the second as long as the shapes
-- of the two make sense.
addQual :: QType -> QType -> Either T.Text QType
addQual (r :< (TVarF _))  (r' :< t)  = return $ (r++r') :< t
addQual (r :< _) (r' :< t@(TVarF _)) = return $ (r++r') :< t

addQual (r :< TBuiltinF x) (r' :< t@(TBuiltinF y)) =
  return $ assert (x==y) $ (r++r') :< t

addQual (r :< TFunF t1 t2) (r' :< TFunF t1' t2') = liftA ((r++r') :<) $
  liftA2 TFunF (addQual t1 t1') (addQual t2 t2')

addQual (r :< TTupF ts) (r' :< TTupF ts') =
  liftA ((r++r') :<) $ liftA TTupF $ addQualList ts ts'

addQual (r :< TNamedF n ts) (r' :< TNamedF n' ts') = assert (value n==value n') $
  liftA ((r++r') :<) $ liftA (TNamedF n) $ addQualList ts ts'

addQual t t' = error $
  "Doesn't make sense to add refinements from "
    ++ show (pretty t)
    ++ " to "
    ++ show (pretty t')

addQualList :: [QType] -> [QType] -> Either T.Text [QType]
addQualList ts ts' = sequenceA $ map (uncurry addQual) (zip ts ts')
