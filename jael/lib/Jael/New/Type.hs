{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}

module Jael.New.Type where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen as P
import           Data.Eq.Deriving (deriveEq1)
import           Text.Show.Deriving (deriveShow1)

data TypeF a = TAllF [T.Text] a
             | TFunF a a
             | TConF T.Text [a]
             | TTupF [a]
             | TRecF [(T.Text, a)]
             | TArrF a Integer
             | TVarF T.Text
             deriving (Eq, Foldable, Functor, Show, Traversable)

type Type = Fix TypeF

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)

pattern TAll :: [T.Text] -> Type -> Type
pattern TAll a b = Fix (TAllF a b)

pattern TFun :: Type -> Type -> Type
pattern TFun a b = Fix (TFunF a b)

pattern TCon :: T.Text -> [Type] -> Type
pattern TCon a b = Fix (TConF a b)

pattern TTup :: [Type] -> Type
pattern TTup xs = Fix (TTupF xs)

pattern TRec :: [(T.Text, Type)] -> Type
pattern TRec xs = Fix (TRecF xs)

pattern TArr :: Type -> Integer -> Type
pattern TArr t n = Fix (TArrF t n)

pattern TVar :: T.Text -> Type
pattern TVar a = Fix (TVarF a)

pattern TBool :: Type
pattern TBool = Fix (TConF "Bool" [])

pattern TInt :: Type
pattern TInt = Fix (TConF "Int" [])

class TIOps a where
  ftv :: a -> S.Set T.Text
  apply :: M.Map T.Text Type -> a -> a

instance TIOps Type where
  ftv = cata alg
    where alg (TVarF v)     = S.singleton v
          alg (TFunF t1 t2) = t1 `S.union` t2
          alg (TConF _ ts)  = S.unions ts
          alg (TTupF ts)    = S.unions ts
          alg (TRecF fs)    = S.unions $ map snd fs
          alg (TArrF t _)   = t
          alg (TAllF vs t)  = t S.\\ S.fromList vs

  apply s (TVar v)     = M.findWithDefault (TVar v) v s
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply s (TCon n ts)  = TCon n $ map (apply s) ts
  apply s (TTup ts)    = TTup $ map (apply s) ts
  apply s (TRec fs)    = TRec $ map (fmap $ apply s) fs
  apply s (TArr t i)   = TArr (apply s t) i
  apply s (TAll _ t)  = let t' = apply s t in TAll (S.toList $ ftv t') t'
  apply _ _ = error "Looks like exhaustive patterns to me ..."

instance TIOps (Type, Type) where
  ftv (t1, t2) = S.union (ftv t1) (ftv t2)
  apply s ts = join bimap (apply s) ts

instance TIOps a => TIOps [a] where
  ftv = S.unions . map ftv
  apply s = map (apply s)

instance TIOps (M.Map T.Text Type) where
  ftv = S.unions . map ftv . M.elems
  apply s = M.map (apply s)

generalize' :: M.Map T.Text Type -> Type -> Type
generalize' env t
  | vs <- S.toList $ ftv t S.\\ ftv env
  , not (null vs)
  = TAll vs t
  | otherwise
  = t

alphaEq :: Type -> Type -> Bool
alphaEq t u = case mkSub M.empty t u of
  Nothing -> False
  Just s  -> apply (M.map TVar s) t == u
  where
    mkSubFold :: M.Map T.Text T.Text -> (Type, Type)
              -> Maybe (M.Map T.Text T.Text)
    mkSubFold acc (a, b) = mkSub acc a b

    mkSub :: M.Map T.Text T.Text -> Type -> Type
          -> Maybe (M.Map T.Text T.Text)
    mkSub sub (TAll _ v) (TAll _ v') = mkSub sub v v'

    mkSub sub (TVar a) (TVar b) =
      case M.lookup a sub of
        Just b' | b == b'   -> Just sub
                | otherwise -> Nothing
        _                   -> Just (M.insert a b sub)

    mkSub sub (TCon n as) (TCon m bs)
      | n == m && length as == length bs
      = foldM mkSubFold sub (zip as bs)
      | otherwise
      = Nothing

    mkSub sub (TTup as) (TTup bs)
      | length as == length bs = foldM mkSubFold sub (zip as bs)
      | otherwise              = Nothing

    mkSub sub (TArr a i) (TArr a' i')
      | i == i'   = mkSub sub a a'
      | otherwise = Nothing

    mkSub sub (TRec fs) (TRec fs')
      | m  <- M.fromList fs
      , m' <- M.fromList fs'
      , M.keys m == M.keys m'
      = foldM mkSubFold sub $ M.intersectionWith (,) m m'
      | otherwise
      = Nothing

    mkSub sub (TFun a a') (TFun b b') =
      mkSub sub a b >>= \sub' -> mkSub sub' a' b'

    mkSub sub a b
      | a == b    = Just sub
      | otherwise = Nothing

class Prec a where
  prec :: a -> Integer

instance Prec (TypeF a) where
  prec (TAllF _ _) = 0
  prec (TFunF _ _) = 1
  prec (TConF _ _) = 2
  prec (TRecF _)   = 2
  prec (TTupF _)   = 2
  prec (TArrF _ _) = 2
  prec (TVarF _)   = 2

instance Prec Type where
  prec (Fix x) = prec x

braced :: [Doc] -> Doc
braced = encloseSep lbrace rbrace comma

checkPrec :: (Prec a, Prec b, Pretty b) => (Integer -> Integer -> Bool) -> a -> b -> Doc
checkPrec f x y =
  let p = pretty y
  in if prec x `f` prec y then parens p else p

precGt :: (Prec a, Prec b, Pretty b) => a -> b -> Doc
precGt = checkPrec (>)

precGe :: (Prec a, Prec b, Pretty b) => a -> b -> Doc
precGe = checkPrec (>=)

instance (Prec a, Pretty a) => Pretty (TypeF a) where
  pretty x@(TAllF as t) =
          text "forall"
    P.<+> hsep (map (string . T.unpack) as)
    P.<>  dot
    P.<+> precGt x t

  pretty x@(TFunF a b) =
          precGe x a
    P.<+> string "->"
    P.<+> precGt x b

  pretty (TConF n xs) =
    let xs' = if null xs then P.empty else tupled $ map pretty xs
    in  pretty (T.unpack n) P.<> xs'

  pretty (TTupF xs) = tupled $ map pretty xs

  pretty r@(TRecF xs) = braced $ flip map xs
    (\(l, t) -> pretty (T.unpack l) P.<+> colon P.<+> precGt r t)

  pretty (TArrF t n) = P.brackets $ pretty t P.<> semi P.<+> pretty n

  pretty (TVarF x) = string (T.unpack x)

instance P.Pretty Type where
  pretty (Fix x) = P.pretty x
