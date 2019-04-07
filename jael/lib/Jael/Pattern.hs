{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Jael.Pattern where

import Jael.Prelude

-- import qualified Data.TreeDiff

-- import           Jael.Classes
-- import           Jael.TH

import Data.Row
import Data.Row.Variants (view)
import qualified Data.Row.Variants as V

class MappableVar to fFrom where
  type Mapped to fFrom

instance MappableVar to (NlF from) where
  type instance Mapped to (NlF from) = NlF to

foo :: Var ("nil" .== NlF String)
foo = V.transform f (IsJust #nil NlF' :: Var ("nil" .== NlF Integer))
  where f :: (MappableVar String (NlF Integer)) => NlF Integer -> (Mapped String) (NlF Integer)
        f NlF' = NlF'

data CnsF a x = CnsF' a x
  deriving (Eq, Functor, Show)

data NlF x = NlF'
  deriving (Eq, Functor, Show)

type LstRow a x = ("cons" .== CnsF a x .+ "nil" .== NlF x)
type LstVar a x = Var (LstRow a x)

newtype LstF a x = LstF (LstVar a x)
  deriving (Eq, Show)

type Lst a = Fix (LstF a)

pattern CnsF :: a -> Lst b -> LstF a (Lst b)
pattern CnsF x xs <- LstF (view (Label :: Label "cons") -> Just (CnsF' x xs)) where
        CnsF x xs =  LstF (IsJust (Label :: Label "cons") (CnsF' x xs))

pattern NlF :: LstF a (Lst b)
pattern NlF <- LstF (view (Label :: Label "nil") -> Just NlF') where
        NlF =  LstF (IsJust (Label :: Label "nil") NlF')

{-# Complete CnsF, NlF #-}

pattern Cns :: a -> Lst a -> Lst a
pattern Cns x xs <- Fix (LstF (view (Label :: Label "cons") -> Just (CnsF' x xs))) where
        Cns x xs =  Fix (LstF (IsJust (Label :: Label "cons") (CnsF' x xs)))

pattern Nl :: Lst a
pattern Nl <- Fix (LstF (view (Label :: Label "nil") -> Just NlF')) where
        Nl =  Fix (LstF (IsJust (Label :: Label "nil") NlF'))

{-# Complete Cns, Nl #-}

instance Functor (LstF a) where
  fmap f (LstF v) = LstF $ switch v $ (Label :: Label "cons") .== IsJust (Label :: Label "cons") . fmap f .+ (Label :: Label "nil") .== IsJust (Label :: Label "nil") . fmap f

instance (Show a) => Show1 (LstF a) where
  liftShowsPrec f _ p (LstF x) =
    switch x $ (Label :: Label "cons") .== (\(CnsF' a y) ->
                            (<>) ("(CnsF " <> show a <> " " <> f p y ")"))
             .+ (Label :: Label "nil") .== const ("NlF" <>)

test :: Lst Integer
test = Cns 42 Nl

main :: IO ()
main =
  case test of 
    Cns x xs -> do
      print test
      print x
      print xs
      print $ cata (f (+1)) test
    Nl -> undefined
 where 
  f :: (a -> b) -> LstF a (Lst b) -> Lst b
  f _ NlF = Nl
  f g (CnsF x xs) = Cns (g x) xs

--print (IsJust #or (#patterns .== []) :: PatVar Integer)

newtype POrF p = POrF [p]
-- $(mkConBoilerplate ''POrF)

data PConF l p = PConF l [p]
-- $(mkConBoilerplate ''PConF)

data PVarF v p = PVarF v
-- $(mkConBoilerplate ''PVarF)

data PAtF v p = PAtF v p
-- $(mkConBoilerplate ''PAtF)

data PLitF l p = PLitF l
-- $(mkConBoilerplate ''PLitF)

data PWildF p = PWildF
-- $(mkConBoilerplate ''PWildF)

newtype PTupF p = PTupF [p]
-- $(mkConBoilerplate ''PTupF)

newtype PArrF p = PArrF [p]
-- $(mkConBoilerplate ''PArrF)

data PRecEmptyF p = PRecEmptyF
-- $(mkConBoilerplate ''PRecEmptyF)

-- type RecTailPat v = EADT '[PVarF v, PWildF, PRecEmptyF]

-- data PRecF v b p = PRecF [(b, p)] (RecTailPat v)
-- $(mkConBoilerplate ''PRecF)

-- pattern POr :: (POrF :<: xs) => [EADT xs] -> EADT xs
-- pattern POr ps = VF (POrF ps)

-- pattern PCon :: (PConF b :<: xs) => b -> [EADT xs] -> EADT xs
-- pattern PCon x ps = VF (PConF x ps)

-- pattern PVar :: (PVarF v :<: xs) => v -> EADT xs
-- pattern PVar v = VF (PVarF v)

-- pattern PAt :: (PAtF v :<: xs) => v -> EADT xs -> EADT xs
-- pattern PAt v p = VF (PAtF v p)

-- pattern PLit :: (PLitF l :<: xs) => l -> EADT xs
-- pattern PLit x = VF (PLitF x)

-- pattern PWild :: (PWildF :<: xs) => EADT xs
-- pattern PWild = VF PWildF

-- pattern PRecEmpty :: (PRecEmptyF :<: xs) => EADT xs
-- pattern PRecEmpty = VF PRecEmptyF

-- pattern PTup :: (PTupF :<: xs) => [EADT xs] -> EADT xs
-- pattern PTup xs = VF (PTupF xs)

-- pattern PArr :: (PArrF :<: xs) => [EADT xs] -> EADT xs
-- pattern PArr ps = VF (PArrF ps)

-- pattern PRec :: (PRecF v b :<: xs) => [(b, EADT xs)] -> RecTailPat v -> EADT xs
-- pattern PRec bp's t = VF (PRecF bp's t)

-- type PatternCs  v b l = '[POrF, PConF b, PVarF v, PAtF v, PLitF l, PWildF, PRecEmptyF, PTupF, PArrF, PRecF v b]
-- type PatternF   v b l = VariantF (PatternCs v b l)
-- type Pattern    v b l = EADT (PatternCs v b l)
-- type PatternCs' v b l = '[PConF b, PVarF v, PAtF v, PLitF l, PWildF, PRecEmptyF, PTupF, PArrF, PRecF v b]
-- type PatternF'  v b l = VariantF (PatternCs' v b l)
-- type Pattern'   v b l = EADT (PatternCs' v b l)

-- instance Remake (PVarF v) where
--   remake (PVarF v) = PVarF v

-- instance Remake PWildF where
--   remake PWildF = PWildF

-- instance Remake PRecEmptyF where
--   remake PRecEmptyF = PRecEmptyF

-- instance Remake (PLitF l) where
--   remake (PLitF l) = PLitF l

-- class PatVars (f :: * -> *) v where
--   patternVars' :: f [v] -> [v]

-- instance {-# Overlappable #-} (Foldable f) => PatVars f v where
--   patternVars' = foldr (<>) []

-- $(mkVariantInstances ''PatVars)

-- instance PatVars (PVarF v) v where
--   patternVars' (PVarF v) = [v]

-- instance PatVars (PAtF v) v where
--   patternVars' (PAtF v p) = v:p

-- patternVars :: ( Functor (VariantF xs), PatVars (VariantF xs) v
--                ) => EADT xs -> [v]
-- patternVars = cata patternVars'

-- class PatExpandOr (f :: * -> *) xs where
--   expandOr' :: f [EADT xs] -> [EADT xs]

-- $(mkVariantInstances ''PatExpandOr)

-- instance PatExpandOr POrF xs where
--   expandOr' (POrF ps) = join ps

-- combinations :: [[a]] -> [[a]]
-- combinations = foldrM (\x a -> fmap (:a) x) []

-- instance (PConF b :<: xs) => PatExpandOr (PConF b) xs where
--   expandOr' (PConF n ps) = PCon n <$> combinations ps

-- instance (PAtF v :<: xs) => PatExpandOr (PAtF v) xs where
--   expandOr' (PAtF v p) = fmap (PAt v) p

-- instance (PTupF :<: xs) => PatExpandOr PTupF xs where
--   expandOr' (PTupF ps) = PTup <$> combinations ps

-- instance (PArrF :<: xs) => PatExpandOr PArrF xs where
--   expandOr' (PArrF ps) = PArr <$> combinations ps

-- instance (PRecF v b :<: xs) => PatExpandOr (PRecF v b) xs where
--   expandOr' (PRecF ps t) = flip PRec t <$> combinations (distVar <$> ps)
--     where distVar (v, xs) = (v,) <$> xs

-- instance {-# Overlappable #-} (f :<: xs, Remake f) => PatExpandOr f xs where
--   expandOr' = (:[]) . VF . remake

-- expandOr :: ( Functor (VariantF xs), PatExpandOr (VariantF xs) ys
--             ) => EADT xs -> [EADT ys]
-- expandOr = cata expandOr'

-- instance (PVarF v' :<: ys) => VarsApply (PVarF v) v v' ys where
--   varApp f (PVarF v) = PVar (f v)
--   varAppMaybe f (PVarF v) = maybe (Left [v]) (Right . PVar) (f v)

-- instance (PAtF v' :<: ys) => VarsApply (PAtF v) v v' ys where
--   varApp f (PAtF v x) = PAt (f v) x
--   varAppMaybe f (PAtF v x) =
--     case x of
--       Left vs   -> Left (v:vs)
--       Right ast -> maybe (Left [v]) (Right . flip PAt ast) (f v)

-- instance ( PRecF v' b :<: ys
--          ) => VarsApply (PRecF v b) v v' ys where
--   varApp f (PRecF ps t) = PRec ps (cata (varApp f) t :: RecTailPat v')
--   varAppMaybe f (PRecF ps t) =
--     case cata (varAppMaybe f) t :: Either [v] (RecTailPat v') of
--       Left vs  -> Left $ vs <> concat (lefts (fmap snd ps))
--       Right t' -> PRec <$> collect (fmap sequence ps) <*> pure t'

-- patternArity :: forall v b l. Pattern' v b l -> Integer
-- patternArity (PCon (_  ::b) (ps :: [Pattern' v b l]))                 = length ps
-- patternArity (PAt  (_  ::v) (p::Pattern' v b l))                      = patternArity p
-- patternArity (PTup (ps :: [Pattern' v b l]))                          = length ps
-- patternArity (PArr (ps :: [Pattern' v b l]))                          = length ps
-- patternArity (PRec (ps :: [(b, Pattern' v b l)]) (_ :: RecTailPat v)) = length ps + 1 -- There is always a tail
-- patternArity _ = 0

-- sortPatternRows :: (Ord b) => Pattern' v b l -> Pattern' v b l
-- sortPatternRows = cata sortRows'

-- class SortRows (f :: * -> *) ys where
--   sortRows' :: f (EADT ys) -> EADT ys

-- $(mkVariantInstances ''SortRows)

-- instance {-# Overlappable #-} (f :<: ys) => SortRows f ys where
--   sortRows' x = VF x

-- instance (PRecF v b :<: ys, Ord b) => SortRows (PRecF v b) ys where
--   sortRows' (PRecF xs t) = PRec (sortFst xs) t

-- normalisePatternMatrix :: (Ord b) => [[Pattern' v b l]] -> [[Pattern' v b l]]
-- normalisePatternMatrix pss = let
--   ls = fmap length pss
--   l = headNote "Should be at least one pattern vector in the matrix." ls
--   in assertNote "All pattern vectors should have the same length" (all (== l) ls) $ fmap (fmap sortPatternRows) pss

patternArity :: Pattern v l -> Int
patternArity (POr ps         ) = patternArity (head ps)
patternArity (PCon _ ps      ) = length ps
patternArity (PAt  _ p       ) = patternArity p
patternArity (PTup ps        ) = length ps
patternArity (PArr ps        ) = length ps
patternArity (PRec (Row ps _)) = foldr (\x a -> a + length x) 1 ps
patternArity _                 = 0

class OverPattern (r :: R.Row (* -> *)) v v' l l' (f :: * -> *) where
  fmapPattern' :: (v -> v') -> (l -> l') -> f (OpenADT r) -> OpenADT r

instance ( OpenAlg r "pOrF" POrF (OpenADT r)
         ) => OverPattern r v v' l l' POrF where
  fmapPattern' _ _ (POrF' ps) = POr ps

instance ( OpenAlg r "pVarF" (PVarF v') (OpenADT r)
         ) => OverPattern r v v' l l' (PVarF v) where
  fmapPattern' f _ (PVarF' v) = PVar (f v)

instance ( OpenAlg r "pLitF" (PLitF l') (OpenADT r)
         ) => OverPattern r v v' l l' (PLitF l) where
  fmapPattern' _ g (PLitF' l) = PLit (g l)

instance ( OpenAlg r "pWildF" PWildF (OpenADT r)
         ) => OverPattern r v v' l l' PWildF where
  fmapPattern' _ _ PWildF' = PWild

fmapPattern
  :: forall u w c v v' l l'
   . (Forall u Functor, Forall u (OverPattern w v v' l l'))
  => (v -> v')
  -> (l -> l')
  -> OpenADT u
  -> OpenADT w
fmapPattern f g =
--  cata (varFAlg @u @(OverPattern w v v' l l') @(OpenADT w) (fmapPattern' f g))
  cata (fmapPattern' f g)

instance ( Forall u (OverPattern w v v' l l')
         ) => OverPattern w v v' l l' (VarF u) where
  --fmapPattern' :: (v -> v') -> (l -> l') -> VarF u (OpenADT w) -> OpenADT w
  fmapPattern' f g = varFAlg @u
                             @(OverPattern w v v' l l')
                             @(OpenADT w)
                             @(OpenADT w)
                             (fmapPattern' f g)

type P v l = OpenADT
      ( "pOrF"
          .==
          POrF
          .+
          "pWildF"
          .==
          PWildF
          .+
          "pLitF"
          .==
          PLitF
          l
          .+
          "pVarF"
          .==
          PVarF
          v
      )

foo :: P String String
foo = fmapPattern (show @Float)
                  (show @Int)
                  (POr [PWild, PVar 0.42, PLit 42] :: P Float Int)
