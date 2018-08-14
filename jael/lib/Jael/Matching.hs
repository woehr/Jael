{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Jael.Matching
--( cc
--, DecisionTree
--, Action
--, DefaultCase
--)
                     where

import           Prelude                                  ( Int )

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Haskus.Utils.VariantF
import           Data.Tree                                ( Tree(..) )
import           Control.Lens.TH
import           Control.Lens.Operators

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.MultiSet                 as MS
import qualified Data.Text                     as T

import           Jael.DataDecl
import           Jael.Expr                         hiding ( Constructor )
import           Jael.Pattern
import           Jael.Prelude.Minimal
import           Jael.Prelude                             ( Grouping
                                                          , Generic
                                                          , nub
                                                          )
import           Jael.TH

type P = Pattern' T.Text T.Text Literal
type Action = Integer
type PatternVec = [P]
type PatternMatrix = [PatternVec]
type ClauseMatrix = [(PatternVec, Action)]
type ClauseMatrix' = [(Integer, PatternVec, Action)]
type DefaultCase = Maybe DecisionTree

-- This Occurence type tracks variables in patterns. The first field is
-- occurence leading to the position in the pattern. The second field is the
-- row and corresponding name of the variable in the matrix.
data Occurence v = Occurence [Integer] (Integer, v)
  deriving (Eq, Ord, Show)

type OccurenceVec = [[Integer]]

data Constructor = CTuple | CArray | CRecord | CName String | CInt Int
  deriving (Eq, Generic, Show)
instance Grouping Constructor

data CaseList = CaseList [(Constructor, DecisionTree)] DefaultCase
  deriving (Eq, Show)

data DecisionTree = Fail | Leaf Action | Switch [Integer] CaseList
  deriving (Eq, Show)

data Env = Env
  { _cinfo :: M.Map T.Text (S.Set T.Text)
  , _selectColumnFn :: PatternMatrix -> Integer
  , _occurenceVec :: OccurenceVec
  }
makeLenses ''Env

type MatchS = [Occurence T.Text]

initState :: MatchS
initState = []

newtype Err = Err String
  deriving (Eq, Show)

type MatchM v = StateT MatchS (ReaderT Env (Except Err))

-- Any record patterns in the ClauseMatrix are sorted prior to calling cc on it.
-- Use normalisePatternMatrix before calling.
cc :: [[ConsInfo t]]
   -> (PatternMatrix -> Integer)
   -> ClauseMatrix
   -> Either Err (DecisionTree, S.Set (Occurence T.Text))
cc cinfos selectColumn m = runExcept
  (runReaderT (second S.fromList <$> runStateT (cc' m') initState) env)
 where
  m' :: ClauseMatrix'
  m' = fmap (\(x, (y, z)) -> (x, y, z)) (zip [0 ..] m)
  env :: Env
  env =
    let xss = zip cinfos . fmap (S.fromList . fmap ddciName) $ cinfos
        ci  = M.fromList [ (ddciName x, s) | (xs, s) <- xss, x <- xs ]
    in  Env
          { _cinfo          = ci
          , _selectColumnFn = selectColumn
          , _occurenceVec   = [ [i] | i <- [0 .. cmWidth m - 1] ]
          }

cc' :: (MonadError Err m, MonadReader Env m, MonadState MatchS m)
    => ClauseMatrix'
    -> m DecisionTree
cc' []                               = return Fail

-- Handles cases where length p0 == 0 and all p0 are wildcards
cc' ((i, p0, a) : _) | all isWild p0 = do
  occs <- asks _occurenceVec
  id <>= concatMap (varOcc i) (zip p0 occs)
  return (Leaf a)

-- length p0 > 0 and not all p's in p0 are wildcards
cc' m = do
  selFn <- asks _selectColumnFn
  let i = selFn (patternMatrix m)
  local (occurenceVec %~ swapOcc i) $ cc'' (preparePRec (swapCM i m))

cc'' :: forall m
      . (MonadError Err m, MonadReader Env m, MonadState MatchS m)
     => ClauseMatrix'
     -> m DecisionTree
cc'' m = do
  let headIsCs = catMaybes . fmap (\(i, pvec, _) -> (i, ) <$> headMay pvec) $ m
  -- If any of the headCs are PAt patterns the occurence of the variable
  -- needs to be recorded. This also records variable occurences that might
  -- disappear when a default matrix is processed.
  Just o0 <- headMay <$> asks _occurenceVec
  forM_ headIsCs $ \(i, c) -> id <>= varOcc i (c, o0)

  let headCs =
        nubBy sameConstructor . filter (not . isWild) . fmap snd $ headIsCs
  when (null headCs)
    $ error "Should always be at least one pattern that is not a wildcard"
  cs <- nub <$> mapM toConstructor headCs
  when (length headCs /= length cs) $ error
    (  "Patterns and constructor lengths don't match\n"
    <> show headCs
    <> "\n"
    <> show cs
    )

  defTree  <- local (occurenceVec %~ drop 1) $ cc' (def m)
  mDefTree <- case cs of
    (CName _ : _) -> do
      isSig <- isSignature (cNames cs)
      return $ if isSig then Nothing else Just defTree
    (CInt _ : _) -> return $ Just defTree
    _            -> return Nothing

  Switch o0 . flip CaseList mDefTree <$> mapM cc'specialised (zip cs headCs)
 where
  cc'specialised :: (Constructor, P) -> m (Constructor, DecisionTree)
  cc'specialised (c, p) = do
    dt <- local (occurenceVec %~ expandOcc (patternArity p)) (cc' $ spec p m)
    return (c, dt)

  expandOcc :: Integer -> OccurenceVec -> OccurenceVec
  expandOcc 0 (_  : os) = os
  expandOcc i (o1 : os) = zipWith (:) [0 .. i - 1] (repeat o1) <> os
  expandOcc _ _         = error "expandOcc"

varOcc :: Integer -> (P, [Integer]) -> [Occurence T.Text]
varOcc i (p, o) = case p of
  PVar v   -> [Occurence o (i, v)]
  PAt v p' -> Occurence o (i, v) : varOcc i (p', o)
  _        -> []

preparePRec :: ClauseMatrix' -> ClauseMatrix'
preparePRec m = flip fmap m $ \case
  (n, PRec ls (t :: RecTailPat T.Text) : ps, a) ->
    (n, PRec (fillRow ls) t : ps, a)
  x -> x
 where
  mRecLabels :: [[T.Text]]
  mRecLabels = catMaybes $ fmap f m
   where
    f (_, PRec (ls :: [(T.Text, P)]) (_ :: RecTailPat T.Text) : _, _) =
      Just $ fmap fst ls
    f _ = Nothing

  mergedRows :: [T.Text]
  mergedRows = MS.toList
    $ foldr (\xs ms -> MS.fromList xs `MS.maxUnion` ms) MS.empty mRecLabels

  fillRow :: [(T.Text, P)] -> [(T.Text, P)]
  fillRow = go mergedRows
   where
    go :: [T.Text] -> [(T.Text, P)] -> [(T.Text, P)]
    go (xl : xs) zs@((yl, yp) : ys) =
      if xl == yl then (yl, yp) : go xs ys else (xl, PWild) : go xs zs
    go xs [] = fmap (, PWild) xs
    go [] _  = error "spec: fillRow: xs should never run out before ys"

isSignature :: (MonadReader Env m) => S.Set T.Text -> m Bool
isSignature s = case headMay (S.toList s) of
  Just sigElem -> do
    mSig <- M.lookup sigElem <$> asks _cinfo
    case mSig of
      Just sig -> do
        assertNote "isSignature: input is not subset of looked up signature"
                   (S.isSubsetOf s sig)
                   (return ())
        if s == sig then return True else return False
      Nothing -> error "isSignature: sig name not found"
  Nothing -> error "isSignature: empty set"

swap0 :: (Show a) => Integer -> [a] -> [a]
swap0 i xs = case splitAt (fromInteger i) xs of
  (as, b : bs) -> b : (as <> bs)
  _            -> error $ "Tried to swap index " <> show i <> " of " <> show xs

swapCM :: Integer -> ClauseMatrix' -> ClauseMatrix'
swapCM i = fmap $ \(x, y, z) -> (x, swap0 i y, z)

swapOcc :: Integer -> OccurenceVec -> OccurenceVec
swapOcc = swap0

cmWidth :: ClauseMatrix -> Integer
cmWidth ((ps, _) : _) = length ps
cmWidth _             = error "Malformed ClauseMatrix"

spec :: P -> ClauseMatrix' -> ClauseMatrix'
spec c m = catMaybes $ fmap s' m
 where
  s' (_, [], _) =
    error "Can't call s on clause matrix with empty pattern vector"
  s' (i, p : ps, a) = if
    | sameConstructor c p -> Just (i, subPatterns p <> ps, a)
    | isWild p -> Just
      (i, replicate (fromInteger $ patternArity c) PWild <> ps, a)
    | otherwise -> Nothing

def :: ClauseMatrix' -> ClauseMatrix'
def m = catMaybes $ fmap d' m
 where
  d' (i, p : ps, a) | isWild p = Just (i, ps, a)
  d' _                         = Nothing

isPRec :: P -> Bool
isPRec (PRec (_ :: [(T.Text, P)]) (_ :: RecTailPat T.Text)) = True
isPRec _ = False

toConstructor :: (MonadError Err m) => P -> m Constructor
toConstructor (PCon (n :: T.Text) _) = return $ CName (T.unpack n)
toConstructor (PTup _              ) = return CTuple
toConstructor (PArr _              ) = return CArray
toConstructor (PRec (_ :: [(T.Text, P)]) (_ :: RecTailPat T.Text)) =
  return CRecord
toConstructor (PAt (_ :: T.Text) p  ) = toConstructor p
toConstructor (PLit (LInt JInt {..})) = return (CInt $ fromInteger intValue)
toConstructor p =
  throwError (Err $ "Can't convert " <> show p <> " to Constructor")

cNames :: [Constructor] -> S.Set T.Text
cNames = foldr f S.empty
 where
  f (CName n) s = S.insert (T.pack n) s
  f _         s = s

sameConstructor :: P -> P -> Bool
sameConstructor (PCon (c :: T.Text) _) (PCon (c' :: T.Text) _) = c == c'
sameConstructor (PTup _              ) (PTup _               ) = True
sameConstructor (PArr _              ) (PArr _               ) = True
sameConstructor (PRec (_ :: [(T.Text, P)]) (_ :: RecTailPat T.Text)) (PRec (_ :: [ ( T.Text
    , P
    )
  ]) (_ :: RecTailPat T.Text))
  = True
sameConstructor (PAt (_ :: T.Text) p) p' = sameConstructor p p'
sameConstructor p' (PAt (_ :: T.Text) p) = sameConstructor p p'
sameConstructor (PLit (l :: Literal)) (PLit l') = l == l'
sameConstructor _ _ = False

subPatterns :: P -> [P]
subPatterns (PCon (_ :: T.Text) ps) = ps
subPatterns (PTup ps              ) = ps
subPatterns (PArr ps              ) = ps
subPatterns (PRec (xs :: [(T.Text, P)]) (t :: RecTailPat T.Text)) =
  fmap snd xs <> [liftEADT t]
subPatterns (PAt (_ :: T.Text) p) = subPatterns p
subPatterns _                     = []

patternMatrix :: ClauseMatrix' -> PatternMatrix
patternMatrix = fmap (\(_, x, _) -> x)

class IsWild (f :: * -> *) where
  isWild' :: f Bool -> Bool
instance {-# Overlappable #-} IsWild f where
  isWild' _ = False
instance IsWild PWildF where
  isWild' _ = True
instance IsWild (PVarF v) where
  isWild' _ = True
instance IsWild (PAtF v) where
  isWild' (PAtF _ p) = p
-- PRecEmpty is considered a wildcard because, if the pattern has typechecked,
-- the record tail pattern never needs to be tested.
instance IsWild PRecEmptyF where
  isWild' _ = True

isWild :: P -> Bool
isWild = cata isWild'

$(mkVariantInstances ''IsWild)

decisionTreeToTree :: DecisionTree -> Tree String
decisionTreeToTree Fail     = Node "Fail" []
decisionTreeToTree (Leaf a) = Node ("Leaf: " <> show a) []
decisionTreeToTree (Switch o (CaseList cases defCase)) =
  Node ("occurence: " <> show o)
    $ fmap caseToTree (cases <> maybeToList (fmap (CName "*", ) defCase))

caseToTree :: (Constructor, DecisionTree) -> Tree String
caseToTree (c, dt) = Node (show c) [decisionTreeToTree dt]

------------------------- Column selection heuristics -------------------------

-- Pick first column that isn't all wildcards (from the left)
selectLeftColumn :: PatternMatrix -> Integer
selectLeftColumn [] =
  error "Shouldn't happen because selectColumn is called in third case of cc"
selectLeftColumn pm = foldr go 0 (zip [0 ..] $ transpose pm)
  where go (colN, col) i = if all isWild col then i else colN

-- Pick first column that isn't all wildcards (from the right)
selectRightColumn :: PatternMatrix -> Integer
selectRightColumn [] =
  error "Shouldn't happen because selectColumn is called in third case of cc"
selectRightColumn pm = foldl' go 0 (zip [0 ..] $ transpose pm)
  where go i (colN, col) = if all isWild col then i else colN
