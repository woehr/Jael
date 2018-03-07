{-# Language TupleSections #-}

module Jael.New.Check where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Trifecta

import qualified Jael.Data.MultiMap as MM

import Jael.New.DataDecl
import Jael.New.Expr
import Jael.New.Parser
import Jael.New.Type

data ExprErr = EEUnboundVar  (T.Text, Span)
             | EEShadowedVar (T.Text, Span, Maybe Span)
             deriving (Eq, Show)

checkExpr :: S.Set T.Text -> E -> [ExprErr]
checkExpr bvs e =
  map EEUnboundVar (unboundVars bvs e) <>
  map EEShadowedVar (shadowedVars bvs e)

unboundVars :: S.Set T.Text -> E -> [(T.Text, Span)]
unboundVars bvs = MM.toList . flip (foldr MM.delete) (S.toList bvs) . cata alg
  where
    alg (_ C.:< ETAbsF _ _) = error "Expected untyped expression."
    alg (_ C.:< ETAppF _ _) = error "Expected untyped expression."

    alg (_ C.:< EAbsF ps e) =
      foldr (MM.delete . fst) e (concatMap (patternBinds . fst) ps)

    alg (_ C.:< ELamCaseF _ _alts) = undefined

    alg (_ C.:< EAppF f as) = MM.unionsWith (++) (f:as)
    alg (_ C.:< ETupF   es) = MM.unionsWith (++) es

    alg (_ C.:< ELetF _bs _e) = undefined

    alg (_ C.:< ERecExtendF _l _e  _r) = undefined
    alg (_ C.:< ERecUpdateF _l _e  _r) = undefined
    alg (_ C.:< ERecRenameF _l _l' _r) = undefined
    alg (_ C.:< ERecRemoveF _e _l) = undefined
    alg (_ C.:< ERecSelectF _e _l) = undefined

    alg (_ C.:< ECaseF _e _alts) = undefined
    alg (_ C.:< EIfF _b _t _e) = undefined
    alg (_ C.:< EMultiIfF _gs _me) = undefined

    alg (s C.:< EVarF v) = MM.fromList [(v, s)]
    alg _ = MM.empty

shadowedVars :: S.Set T.Text -> E -> [(T.Text, Span, Maybe Span)]
shadowedVars bvs = map (\(a,(b,c)) -> (a,b,c)) . MM.toList . snd . cata alg
  where
    alg :: C.CofreeF (ExprF t P s) Span
           (M.Map T.Text Span, MM.MultiMap T.Text (Span, Maybe Span))
        -> (M.Map T.Text Span, MM.MultiMap T.Text (Span, Maybe Span))
    -- Collect two maps, the first of where a variable was most recently bound
    -- and the second of binds which re-occur in the first
    alg (_ C.:< ETAbsF _ _) = error "Expected untyped expression."
    alg (_ C.:< ETAppF _ _) = error "Expected untyped expression."

    alg (_ C.:< EAbsF ps e) = foldr (checkForShadows . patternBinds . fst) e ps
    alg (_ C.:< ELamCaseF _ _alts) = undefined

    alg (_ C.:< EAppF f as) = mergeMaps (f:as)
    alg (_ C.:< ETupF   _es) = undefined

    alg (_ C.:< ELetF _bs _e) = undefined

    alg (_ C.:< ERecExtendF _l _e  _r) = undefined
    alg (_ C.:< ERecUpdateF _l _e  _r) = undefined
    alg (_ C.:< ERecRenameF _l _l' _r) = undefined
    alg (_ C.:< ERecRemoveF _e _l) = undefined
    alg (_ C.:< ERecSelectF _e _l) = undefined

    alg (_ C.:< ECaseF _e _alts) = undefined
    alg (_ C.:< EIfF _b _t _e) = undefined
    alg (_ C.:< EMultiIfF _gs _me) = undefined

    alg _ = (M.empty, MM.empty)

    mergeMaps :: (Ord a, Ord c)
              => [(M.Map a b, MM.MultiMap c d)] -> (M.Map a b, MM.MultiMap c d)
    mergeMaps ms = ( M.unions $ map fst ms
                   , MM.unionsWith (++) $ map snd ms
                   )

    checkForShadows :: [(T.Text, Span)]
      -> (M.Map T.Text Span, MM.MultiMap T.Text (Span, Maybe Span))
      -> (M.Map T.Text Span, MM.MultiMap T.Text (Span, Maybe Span))
    checkForShadows vs (bs, shads) =
      ( foldr (uncurry M.insert) bs vs
      , foldr updateShads shads vs
      )
      where
        updateShads :: (T.Text, Span) -> MM.MultiMap T.Text (Span, Maybe Span)
                                      -> MM.MultiMap T.Text (Span, Maybe Span)
        updateShads (v, s) m
          | Just s' <- M.lookup v bs = MM.insert v (s, Just s') m
          | S.member v bvs           = MM.insert v (s, Nothing) m
          | otherwise                = m

data PatternErr = PEDupBind T.Text
                | PEInvalidConstructor T.Text
                | PEMultiMulti
                | PEInvalidMulti
                | PEArity T.Text Int Int
                deriving (Eq, Show)

-- Pattern must not have or-patterns
patternBinds :: Cofree (PatternF b) Span -> [(b, Span)]
patternBinds = cata alg
  where
    alg :: C.CofreeF (PatternF b) Span [(b, Span)] -> [(b, Span)]
    alg (_ C.:< POrF  _)               = error "Expected pattern without POrF"
    alg (_ C.:< PPatF _ ps)            = concat ps
    alg (_ C.:< PTupF ps)              = concat ps
    alg (s C.:< PRecF fs (TailBind v)) = (v,s) : concatMap snd fs
    alg (_ C.:< PRecF fs _)            = concatMap snd fs
    alg (_ C.:< PArrF ps)              = concat ps
    alg (s C.:< PBindF v (Just p))     = (v,s):p
    alg (s C.:< PBindF v Nothing)      = [(v,s)]
    alg _ = []

-- Resulting list will have a length >= 1
expandPattern :: P -> [P]
expandPattern = cata alg
  where
    alg (_ C.:< POrF ps) = join ps
    alg (s C.:< PPatF c ps) = ((s :<) . PPatF c) <$> expandPatternList ps
    alg (s C.:< PTupF ps)   = ((s :<) . PTupF) <$> expandPatternList ps
    alg (s C.:< PRecF fs mv) = ((s :<) . flip PRecF mv) <$> expandPatternList
      (map (\(f,ps) -> map (f,) ps) fs)
    alg (s C.:< PArrF ps) = ((s :<) . PArrF) <$> expandPatternList ps
    alg (s C.:< PBindF v (Just p)) = ((s :<) . PBindF v . Just) <$> p

    alg (s C.:< PBindF v Nothing) = [s :< PBindF v Nothing]
    alg (s C.:< PLitF c) = [s :< PLitF c]
    alg (s C.:< PWildF) = [s :< PWildF]

    expandPatternList :: [[a]] -> [[a]]
    expandPatternList = foldrM (\acc x -> map (:x) acc) []

checkPattern :: DataDecl Type -> P -> Either [PatternErr] [P]
checkPattern dd p =
  let ps = expandPattern p
      es = map PEDupBind (concatMap dupBinds ps) ++
           checkConstructors dd p
  in  if null es then Right ps else Left es

-- Pattern must not have or-patterns
dupBinds :: P -> [T.Text]
dupBinds = repeated . map fst . patternBinds

-- Check for valid constructors and arity
checkConstructors :: DataDecl Type -> P -> [PatternErr]
checkConstructors dd = cata alg
  where
    cs = constructorArity dd

    alg (_ C.:< PPatF c ps) =
      let ps' = concat ps :: [PatternErr]
      in case M.lookup c cs of
           Just i | i == length ps -> ps'
           Just i    -> PEArity c i (length ps) : ps'
           Nothing   -> PEInvalidConstructor c  : ps'
    alg (_ C.:< PTupF ps) = concat ps
    alg (_ C.:< POrF ps)  = concat ps
    alg (_ C.:< PRecF fs _) = concatMap snd fs
    alg (_ C.:< PArrF ps) = concat ps
    alg (_ C.:< PBindF _ (Just p)) = p
    alg _ = []
