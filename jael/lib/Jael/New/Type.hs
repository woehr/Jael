{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language TypeSynonymInstances #-}

module Jael.New.Type where

import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Eq.Deriving (deriveEq1)
import           Text.Show.Deriving (deriveShow1)

-- Types of kind row
data Row' v t = Row [(T.Text, t)] (Maybe v)
  deriving (Data, Eq, Foldable, Functor, Show, Traversable, Typeable)

$(deriveEq1   ''Row')
$(deriveShow1 ''Row')

-- Types of kind * with universal quantification over both * and row
data TypeF v t = TAllF [v] t
               | TFunF (Maybe v) t t
               | TConF T.Text [t]
               | TTupF [t]
               | TRecF (Row' v t)
               | TArrF t Integer
               | TVarF v
               deriving (Data, Eq, Foldable, Functor, Show, Traversable, Typeable)

type Row = Row' T.Text Type
type Type = Fix (TypeF T.Text)

$(deriveEq1   ''TypeF)
$(deriveShow1 ''TypeF)

pattern TAll :: [T.Text] -> Type -> Type
pattern TAll a b = Fix (TAllF a b)

pattern TFun :: Maybe T.Text -> Type -> Type -> Type
pattern TFun n a b = Fix (TFunF n a b)

pattern TFun' :: Type -> Type -> Type
pattern TFun' a b = Fix (TFunF Nothing a b)

pattern TCon :: T.Text -> [Type] -> Type
pattern TCon a b = Fix (TConF a b)

pattern TTup :: [Type] -> Type
pattern TTup xs = Fix (TTupF xs)

pattern TRec :: Row  -> Type
pattern TRec r = Fix (TRecF r)

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

class RowOps a where
  rftv :: a -> S.Set T.Text
  rapply :: M.Map T.Text Row -> a -> a

instance RowOps Row where
  rftv (Row _ Nothing)  = S.empty
  rftv (Row _ (Just x))  = S.singleton x

  rapply _ r@(Row _ Nothing) = r
  rapply s r@(Row fs (Just x)) =
    case M.lookup x s of
      Just (Row gs y) -> sortRow $ Row (fs `mappend` gs) y
      Nothing -> r

instance RowOps (Row' T.Text (S.Set T.Text)) where
  rftv (Row fs (Just v)) = S.unions $ S.singleton v : map snd fs
  rftv (Row fs Nothing)  = S.unions $ map snd fs
  rapply = error "Why would we need this?"

instance RowOps a => RowOps [a] where
  rftv = S.unions . fmap rftv
  rapply s = fmap (rapply s)

instance RowOps Type where
  rftv = cata $ \case
    TRecF r -> rftv r
    TAllF vs t -> t S.\\ S.fromList vs
    t -> foldr S.union S.empty t

  rapply s = cata $ \case
    TRecF r -> TRec $ rapply s r
    TAllF vs t ->
      let t' :: Type
          t' = rapply s t
          f v acc = case M.lookup v s of
                      Just (Row _ (Just x)) -> x:acc
                      Just (Row _ Nothing)  -> acc
                      Nothing -> v:acc
          vs' = foldr f [] vs
       in TAll vs' t'
    t -> Fix $ fmap (rapply s) t

instance RowOps (Type, Type) where
  rftv (a, b) = rftv a `S.union` rftv b
  rapply s = join bimap (rapply s)

instance RowOps a => RowOps (M.Map T.Text a) where
  rftv = S.unions . map rftv . M.elems
  rapply s = M.map (rapply s)

instance TIOps Type where
  ftv = cata alg
    where alg (TVarF v)     = S.singleton v
          alg (TRecF r@(Row fs _)) = S.unions $ rftv r : map snd fs
          alg (TAllF vs t)  = t S.\\ S.fromList vs
          alg t = foldr S.union S.empty t

  apply s (TVar v)     = M.findWithDefault (TVar v) v s
  apply s (TFun n t1 t2) = TFun n (apply s t1) (apply s t2)
  apply s (TCon n ts)  = TCon n $ map (apply s) ts
  apply s (TTup ts)    = TTup $ map (apply s) ts
  apply s (TRec (Row fs v)) = TRec (Row (map (second $ apply s) fs) v)
  apply s (TArr t i)   = TArr (apply s t) i
  apply s (TAll vs t)  =
    let t'  = apply s t
        vs' = S.unions . map ftv . apply s . map TVar $ vs
     in TAll (S.toList $ vs' `S.intersection` ftv t') t'
  apply _ _ = error "Looks like exhaustive patterns to me ..."

instance TIOps a => TIOps [a] where
  ftv = S.unions . fmap ftv
  apply s = fmap (apply s)

instance TIOps (Type, Type) where
  ftv (a, b) = ftv a `S.union` ftv b
  apply s = join bimap (apply s)

instance TIOps Row where
  ftv (Row fs _)  = S.unions $ map (ftv . snd) fs
  apply s (Row fs mv) = Row (map (second $ apply s) fs) mv

instance TIOps a => TIOps (M.Map T.Text a) where
  ftv = S.unions . map ftv . M.elems
  apply s = M.map (apply s)

rowUpdate :: T.Text -> t -> Row' v t -> Maybe (Row' v t)
rowUpdate l t (Row ls mv) =
  let mm = MM.fromList ls
   in case MM.lookup l mm of
        (_:vs) -> Just $ Row (MM.toList . MM.fromMap . M.insert l (t:vs) . MM.toMap $ mm) mv
        _      -> Nothing

rowUpdate' :: T.Text -> t -> TypeF v t -> Maybe (TypeF v t)
rowUpdate' l t (TRecF r) = TRecF <$> rowUpdate l t r
rowUpdate' _ _ _ = Nothing

rowExtend :: T.Text -> t -> Row' v t -> Row' v t
rowExtend l t (Row ls mv) = Row ((l,t):ls) mv

rowExtend' :: T.Text -> t -> TypeF v t -> Maybe (TypeF v t)
rowExtend' l t (TRecF r) = Just . TRecF $ rowExtend l t r
rowExtend' _ _ _ = Nothing

rowRename :: T.Text -> T.Text -> Row' v t -> Row' v t
rowRename l' l (Row ls mv) =
  Row (map (\x@(y, z) -> if y == l then (l', z) else x) ls) mv

rowRename' :: T.Text -> T.Text -> TypeF v t -> Maybe (TypeF v t)
rowRename' l' l (TRecF r) = Just . TRecF $ rowRename l' l r
rowRename' _ _ _ = Nothing

rowRemove :: Row' v t -> T.Text -> Maybe (Row' v t)
rowRemove (Row ls mv) l =
  let mm = MM.fromList ls
   in case MM.lookup l mm of
        [_]    -> Just $ Row (MM.toList $ MM.delete l mm) mv
        (_:vs) -> Just $ Row (MM.toList . MM.fromMap . M.insert l vs . MM.toMap $ mm) mv
        _      -> Nothing

rowRemove' :: TypeF v t -> T.Text -> Maybe (TypeF v t)
rowRemove' (TRecF r) l = TRecF <$> rowRemove r l
rowRemove' _ _ = Nothing

rowSelect :: Row' v t -> T.Text -> Maybe t
rowSelect (Row ls _) l = headMay $ MM.lookup l (MM.fromList ls)

rowSelect' :: TypeF v x -> T.Text -> Maybe x
rowSelect' (TRecF r) l = rowSelect r l
rowSelect' _ _ = Nothing

generalize' :: M.Map T.Text Type -> Type -> Type
generalize' env t
  | vs <- S.toList $ ftv t S.\\ ftv env
  , not (null vs)
  = TAll vs t
  | otherwise
  = t

-- Sort according to the first element of the tuple while making sure elements
-- with the same name stay in the same relative order.
sortRow :: Row' v t -> Row' v t
sortRow (Row fs mv) = Row (MM.toList . MM.fromList $ fs) mv

sortRecords :: Type -> Type
sortRecords = hoistFix $ \case
  TRecF r -> TRecF (sortRow r)
  x -> x

alphaEq :: Type -> Type -> Bool
alphaEq t u = case mkSub (M.empty, M.empty) t u of
  Nothing -> False
  Just (s1, s2) ->
    let t' = rapply (fmap (Row [] . Just) s2) $
              apply (fmap TVar s1) $ sortRecords t
        u' = sortRecords u
     in t' == u'
  where
    mkSub :: (M.Map T.Text T.Text, M.Map T.Text T.Text) -> Type -> Type
          -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
    mkSub sub (TAll _ v) (TAll _ v') = mkSub sub v v'

    mkSub sub (TVar a) (TVar b) =
      case M.lookup a (fst sub) of
        Just b' | b == b'   -> Just sub
                | otherwise -> Nothing
        _                   -> Just $ first (M.insert a b) sub

    mkSub sub (TCon n as) (TCon m bs)
      | n == m && length as == length bs
      = foldM mkSubFold sub (zip as bs)
      | otherwise
      = Nothing

    mkSub sub (TTup as) (TTup bs)
      | length as == length bs
      = foldM mkSubFold sub (zip as bs)
      | otherwise
      = Nothing

    mkSub sub (TArr a i) (TArr a' i')
      | i == i'   = mkSub sub a a'
      | otherwise = Nothing

    -- Record types can have duplicate labels so when checking for
    -- equivalence the first label in one type must always match to the first
    -- matching label in the other
    -- A core assumption in this function is that MultiMap retains the order
    -- of values when constructing the maps from fs and fs'
    mkSub sub r1@(TRec _) r2@(TRec _)
      | (TRec (Row fs  mv )) <- sortRecords r1
      , (TRec (Row fs' mv')) <- sortRecords r2
      , map fst fs == map fst fs'
      = do
        sub' <- foldM mkSubFold sub $ zip (map snd fs) (map snd fs')
        case (mv, mv') of
          (Nothing, Nothing) -> Just sub'
          (Just x, Just x') -> Just $ second (M.insert x x') sub'
          _ -> Nothing

      | otherwise
      = Nothing

    mkSub sub (TFun _ a a') (TFun _ b b') =
      mkSub sub a b >>= \sub' -> mkSub sub' a' b'

    mkSub sub a b
      | a == b    = Just sub
      | otherwise = Nothing

    mkSubFold :: (M.Map T.Text T.Text, M.Map T.Text T.Text) -> (Type, Type)
              -> Maybe (M.Map T.Text T.Text, M.Map T.Text T.Text)
    mkSubFold acc (a, b) = mkSub acc a b

arity :: Type -> Integer
arity t = go t 0 where
  go (TFun _ _ t2) acc = go t2 (acc + 1)
  go (TAll _ t') acc = go t' acc
  go _ acc = acc

argTypes :: Type -> [Type]
argTypes t = go t [] where
  go (TFun _ t1 t2) acc = t1 : go t2 acc
  go (TAll _  t') acc = go t' acc
  go _ acc = acc

returnType :: Type -> Type
returnType (TFun _ _ t2) = returnType t2
returnType (TAll vs t) = TAll vs $ returnType t
returnType t = t
