{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jael.Classes where

-- import qualified Data.DList                    as DL
-- import qualified Data.Set                      as S

-- import           Jael.Prelude

-- Change the type of a thing without changing its content
-- Example: SomeBaseCaseF :: f x
--          (remake SomeBaseCaseF) :: f y
class Remake (f :: * -> *) where
  remake :: f x -> f y


-- Substitute a constructor for another value
-- class SubbableADT (f :: * -> *) ys where
--   subAdt' :: (EADT ys -> Maybe (EADT ys)) -> f (EADT ys) -> EADT ys

-- $(mkVariantInstances ''SubbableADT)

-- instance {-# Overlappable #-} (f :<: ys)
--   => SubbableADT f ys where
--   subAdt' f x = fromMaybe (VF x) (f (VF x))

-- subAdt :: (Functor (VariantF ys), SubbableADT (VariantF ys) ys)
--        => (EADT ys -> Maybe (EADT ys))
--        -> EADT ys
--        -> EADT ys
-- subAdt f = cata (subAdt' f)

-- class VarsApply (f :: * -> *) v v' ys where
--   -- Map over a variable
--   varApp :: (v -> v') -> f (EADT ys) -> EADT ys
--   varAppMaybe :: (v -> Maybe v') -> f (Either [v] (EADT ys)) -> Either [v] (EADT ys)

-- $(mkVariantInstances ''VarsApply)

-- instance {-# Overlappable #-} (Functor f, Foldable f, f :<: ys) => VarsApply f v v' ys where
--   varApp _ = VF
--   varAppMaybe _ = fmap VF . collect

-- collect :: (Functor f, Foldable f) => f (Either [v] a) -> Either [v] (f a)
-- collect x =
--   let vs = lefts (toList x)
--   in  if null vs
--         then Right $ fmap (fromRight $ error "There shouldn't be any lefts.") x
--         else Left $ concat vs

-- applyVar :: (Functor (VariantF xs), VarsApply (VariantF xs) v v' ys)
--          => (v -> v')
--          -> EADT xs
--          -> EADT ys
-- applyVar f = cata (varApp f)

-- applyVarMaybe :: (Functor (VariantF xs), VarsApply (VariantF xs) v v' ys)
--               => (v -> Maybe v')
--               -> EADT xs
--               -> Either [v] (EADT ys)
-- applyVarMaybe f = cata (varAppMaybe f)


-- class VarsCollect (f :: * -> *) v where
--   -- Map over a variable
--   vars' :: f (DL.DList v) -> DL.DList v

-- $(mkVariantInstances ''VarsCollect)

-- instance {-# Overlappable #-} (Foldable f) => VarsCollect f v where
--   vars' = foldr DL.append DL.empty

-- vars :: (Functor (VariantF ys), VarsCollect (VariantF ys) v) => EADT ys -> [v]
-- vars = DL.toList . cata vars'


-- class VarsFree (f :: * -> *) v where
--   freeVars' :: f (S.Set v) -> S.Set v

-- $(mkVariantInstances ''VarsFree)

-- instance {-# Overlappable #-} (Foldable f, Ord v) => VarsFree f v where
--   freeVars' = foldr S.union S.empty

-- freeVars :: (Functor (VariantF ys), VarsFree (VariantF ys) v)
--          => EADT ys
--          -> S.Set v
-- freeVars = cata freeVars'

-- class LiftToVarsFree t v where
--   ftv :: t -> S.Set v

-- instance (Functor (VariantF ys), VarsFree (VariantF ys) v) => LiftToVarsFree (EADT ys) v where
--   ftv = freeVars

-- instance LiftToVarsFree (S.Set v) v where
--   ftv = id

-- instance (Functor t, Foldable t, LiftToVarsFree (EADT ys) v, Ord v) => LiftToVarsFree (t (EADT ys)) v where
--   ftv = foldr S.union S.empty . fmap ftv
