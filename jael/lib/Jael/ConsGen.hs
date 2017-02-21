{-# Language NoImplicitPrelude #-}

module Jael.ConsGen where

import           BasePrelude
import           MTLPrelude

import           Control.Comonad.Cofree
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Language.Fixpoint.Types as F

import           Jael.Types
import           Jael.Util

type Template = QScheme

data CGEnv = CGEnv { cgTmplts :: M.Map T.Text QScheme
                   , cgGuards :: [F.Expr]
                   } deriving (Show)

data CGCons a = CGCons { wfs :: [F.WfC a]
                       , subs :: [F.SubC a]
                       } deriving (Show)

instance Monoid (CGCons a) where
  mempty = CGCons [] []
  mappend (CGCons a b) (CGCons a' b') = CGCons (a <> a') (b <> b')

data CGState = CGState { kIndex :: Integer
                       } deriving (Show)

initState :: CGState
initState = CGState 0

type CG = RWST CGEnv (CGCons ()) CGState (Except T.Text)

consGen :: TypedExpr -> CG Template
consGen (qt :< EVarF (Token n _)) =
  tmpltOf n

instTmplt :: Template -> (T.Text, QType) -> Template
instTmplt = undefined

freshTmplt :: Type -> CG Template
freshTmplt = undefined

tmpltOf :: T.Text -> CG Template
tmpltOf n = fromMaybe
              (error "At this point HM inference was successful so we should \
                     \always have what we lookup in our environment")
              <$> (M.lookup n <$> asks cgTmplts)

-- Make a monoid instance?
conjoinTmplts :: Template -> Template -> Template
conjoinTmplts (Scheme xs t1) (Scheme ys t2) =
  assert (S.fromList xs == S.fromList ys) $
  Scheme xs $ conjoinTypes t1 t2

conjoinTypes :: QType -> QType -> QType

conjoinTypes (q1s :< TFunF l1 r1) (q2s :< TFunF l2 r2) =
  q1s <> q2s :< TFunF (conjoinTypes l1 l2) (conjoinTypes r1 r2)

conjoinTypes (q1s :< TTupF t1s) (q2s :< TTupF t2s) =
  q1s <> q2s :< TTupF (map (uncurry conjoinTypes) (zip t1s t2s))

conjoinTypes (q1 :< TNamedF n1 t1) (q2 :< TNamedF n2 t2) =
  assert (n1 == n2) $
  q1 <> q2 :< TNamedF n1 (map (uncurry conjoinTypes) (zip t1 t2))

conjoinTypes (q1 :< TVarF n1) (q2 :< TVarF n2) =
  assert (n1 == n2) $ q1 <> q2 :< TVarF n1

conjoinTypes (q1 :< TBuiltinF b1) (q2 :< TBuiltinF b2) =
  assert (b1 == b2) $ q1 <> q2 :< TBuiltinF b1

conjoinTypes _ _ = error "Shapes of types don't match"
