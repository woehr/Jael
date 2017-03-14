{-# Language DeriveFunctor #-}
{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}
{-# Language DeriveDataTypeable #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleContexts #-}

module Jael.Types.Expr where

import           Jael.Prelude hiding ((<>), (<+>), (<$>))
import qualified Control.Comonad.Trans.Cofree as C
import           Data.Eq.Deriving (deriveEq1)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
import           Text.PrettyPrint.Leijen.Text
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
              | CBitCat
              | CNot
              deriving (Data, Eq, Ord, Show)

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
             deriving (Data, Eq, Functor, Show)

type Expr = Fix ExprF

instance Pretty Expr where
  pretty = (pretty::MaybeTypedExpr -> Doc) . cata ([] :<)

-- An annotated expression
type HMTypedExpr     = Ann ExprF Type
type MaybeTypedExpr  = Ann ExprF [QType]
type TypedExpr       = Ann ExprF QType

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

-- reQual e1 e2 adds refinements from e1 to e2 while retaining the
-- types of e2. Both e1 and e2 must have the same structure. As such, serves as
-- a sanity check of hm inference.
reQual :: MaybeTypedExpr -> HMTypedExpr -> Either T.Text TypedExpr
reQual mte hte = do
  (t, es) <- case (mte, hte) of
               (qts :< e1, hmt :< e2) ->
                 liftA (,(e1,e2)) $ foldM (flip addReftsTo) (noQual hmt) qts
  q <- matchExprs es
  return $ t :< q

matchExprs :: (ExprF MaybeTypedExpr, ExprF HMTypedExpr) -> Either T.Text (ExprF TypedExpr)
matchExprs es =
  case es of
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

    (e1,          e2)             -> error $ errMsg e1 e2

  where errMsg e1 e2 = "Structure of expressions not match\n\n" ++
          show e1 ++ "\n\n" ++ show e2

-- Appends refinements from the first type to the second as long as the shapes
-- of the two make sense.
addReftsTo :: QType -> QType -> Either T.Text QType
addReftsTo (r :< (TVarF _))  (r' :< t)  = return $ (r `mappend` r') :< t

addReftsTo (_ :< t1) (_ :< t2@(TVarF _)) = error $
  "Can't add quals from " ++ show t1 ++ " to " ++ show t2

addReftsTo (r :< TFunF b1 t1 t2) (r' :< TFunF b2 t1' t2') =
  assert (b1 == b2)
  liftA (r `mappend` r' :<) $
  liftA2 (TFunF b1) (t1 `addReftsTo` t1') (t2 `addReftsTo` t2')

addReftsTo (r :< TTupF ts) (r' :< TTupF ts') =
  liftA (r `mappend` r' :<) $ liftA TTupF $ ts `addReftsList` ts'

addReftsTo (r :< TConF n ts) (r' :< TConF n' ts') = assert (value n==value n') $
  liftA (r `mappend` r' :<) $ liftA (TConF n) $ ts `addReftsList` ts'

addReftsTo (r :< TInsF _ t) t' =
  assert (r == F.trueReft)
    addReftsTo t t'

addReftsTo t (r :< TInsF _ t') =
  assert (r == F.trueReft)
    addReftsTo t t'

addReftsTo (r :< TGenF _ t) t' =
  assert (r == F.trueReft)
    addReftsTo t t'

addReftsTo t (r :< TGenF _ t') =
  assert (r == F.trueReft)
    addReftsTo t t'

addReftsTo t t' = trace (show t ++ "\n\n" ++ show t')
  error $
  "Doesn't make sense to add refinements from "
    ++ show (pretty t)
    ++ " to "
    ++ show (pretty t')

addReftsList :: [QType] -> [QType] -> Either T.Text [QType]
addReftsList ts ts' = sequenceA $ map (uncurry addReftsTo) (zip ts ts')

dispSubExprs :: TypedExpr -> [String]
dispSubExprs a = do
  let b = duplicate a :: Cofree ExprF (Cofree ExprF QType)
  let f1 te@(t :< _) fb =
        let x :: [String]
            x = case fb of
                  EAppF e1 e2 -> e1 ++ e2
                  EAbsF _ e -> e
                  ELetF _ e1 e2 -> e1 ++ e2
                  EVarF _ -> []
                  EConF _ -> []
                  EIteF u v w -> u ++ v ++ w
                  ETupF xs -> concat xs
        in  (show (pretty t) ++ "\n" ++ show (pretty $ removeAnn te)) : x
  iterCofree f1 b

$(deriveEq1   ''ExprF)
$(deriveShow1 ''ExprF)
