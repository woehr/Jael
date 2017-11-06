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

data ExprErr = EE_UnboundVar  (T.Text, Span)
             | EE_ShadowedVar (T.Text, Span, Maybe Span)
             deriving (Eq, Show)

checkExpr :: S.Set T.Text -> E -> [ExprErr]
checkExpr bvs e =
  map EE_UnboundVar (unboundVars bvs e) <>
  map EE_ShadowedVar (shadowedVars bvs e)

unboundVars :: S.Set T.Text -> E -> [(T.Text, Span)]
unboundVars bvs = MM.toList . flip (foldr MM.delete) (S.toList bvs) . cata alg
  where
    alg (_ C.:< ETAbsF _ _) = error "Expected untyped expression."
    alg (_ C.:< ETAppF _ _) = error "Expected untyped expression."

    alg (_ C.:< EAbsF ps _ e) =
      foldr MM.delete e (map fst $ concatMap patternBinds ps)

    alg (_ C.:< ELamCaseF _alts) = undefined

    alg (_ C.:< EAppF f as) = MM.unionsWith (++) (f:as)
    alg (_ C.:< ETupF   es) = MM.unionsWith (++) es

    alg (_ C.:< ELetF _bs _e) = undefined

    alg (_ C.:< ERecF    _fs) = undefined
--    alg (_ C.:< ERecUpF  _fs _e) = undefined
    alg (_ C.:< ERecExtF _top _bot) = undefined
    alg (_ C.:< ERecResF _e _l) = undefined
    alg (_ C.:< ERecSelF _e _l) = undefined

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

    alg (_ C.:< EAbsF ps _ e) = foldr checkForShadows e (map patternBinds ps)

    alg (_ C.:< ELamCaseF _alts) = undefined

    alg (_ C.:< EAppF f as) = mergeMaps (f:as)
    alg (_ C.:< ETupF   _es) = undefined

    alg (_ C.:< ELetF _bs _e) = undefined

    alg (_ C.:< ERecF    _fs) = undefined
--    alg (_ C.:< ERecUpF  _fs _e) = undefined
    alg (_ C.:< ERecExtF _top _bot) = undefined
    alg (_ C.:< ERecResF _e _l) = undefined
    alg (_ C.:< ERecSelF _e _l) = undefined

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

data PatternErr = PE_DupBind T.Text
                | PE_InvalidConstructor T.Text
                | PE_MultiMulti
                | PE_InvalidMulti
                | PE_Arity T.Text Int Int
                deriving (Eq, Show)

-- Pattern must not have or-patterns
patternBinds :: P -> [(T.Text, Span)]
patternBinds = cata alg
  where
    alg :: C.CofreeF PatternF Span [(T.Text, Span)] -> [(T.Text, Span)]
    alg (_ C.:< POrF  _)     = error "Expected pattern without POrF"
    alg (_ C.:< PPatF _ ps)  = concat ps
    alg (_ C.:< PTupF ps)    = concat ps
    alg (_ C.:< PRecF fs Nothing) = concat $ map snd fs
    alg (s C.:< PRecF fs (Just v)) = (v,s): (concat $ map snd fs)
    alg (_ C.:< PArrF ps)    = concat ps
    alg (s C.:< PBindF v (Just p)) = (v,s):p
    alg (s C.:< PBindF v Nothing)  = [(v,s)]
    alg _ = []

-- Resulting list will have a length >= 1
expandPattern :: P -> [P]
expandPattern = cata alg
  where
    alg (_ C.:< POrF ps) = join ps
    alg (s C.:< PPatF c ps) = fmap ((s :<) . PPatF c) $ expandPatternList ps
    alg (s C.:< PTupF ps) = fmap ((s :<) . PTupF) $ expandPatternList ps
    alg (s C.:< PRecF fs mv) = fmap ((s :<) . flip PRecF mv) $ expandPatternList $
                                 map (\(f,ps) -> map (f,) ps) fs
    alg (s C.:< PArrF ps) = fmap ((s :<) . PArrF) $ expandPatternList ps
    alg (s C.:< PBindF v (Just p)) = fmap ((s :<) . PBindF v . Just) p

    alg (s C.:< PBindF v Nothing) = [s :< PBindF v Nothing]
    alg (s C.:< PConstF c) = [s :< PConstF c]
    alg (s C.:< PWildF) = [s :< PWildF]
    alg (s C.:< PMultiWildF) = [s :< PMultiWildF]

    expandPatternList :: [[a]] -> [[a]]
    expandPatternList = foldrM (\acc x -> map (:x) acc) []

checkPattern :: DataDecl Type -> P -> Either [PatternErr] [P]
checkPattern dd p =
  let ps = expandPattern p
      es = map PE_DupBind (concatMap dupBinds ps) ++
           checkConstructors dd p ++
           if any multiMultiwild ps then [PE_MultiMulti] else [] ++
           if any invalidMulti ps then [PE_InvalidMulti] else []
  in  if null es then Right ps else Left es

isMulti :: P -> Bool
isMulti (_ :< PMultiWildF) = True
isMulti _                  = False

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
           Just i | otherwise -> (PE_Arity c i $ length ps):ps'
           Nothing -> (PE_InvalidConstructor c):ps'
    alg (_ C.:< PTupF ps) = concat ps
    alg (_ C.:< POrF ps)  = concat ps
    alg (_ C.:< PRecF fs _) = concat $ map snd fs
    alg (_ C.:< PArrF ps) = concat ps
    alg (_ C.:< PBindF _ (Just p)) = p
    alg _ = []

-- Only a single multi-wildcard is allowed per array pattern
multiMultiwild :: P -> Bool
multiMultiwild = para alg where
  alg (_ C.:< PPatF _ ps) = or $ map snd ps
  alg (_ C.:< PTupF ps)   = or $ map snd ps
  alg (_ C.:< POrF ps)    = or $ map snd ps
  alg (_ C.:< PRecF fs _) = or $ map (snd . snd) fs
  alg (_ C.:< PArrF ps)   = let numMulti = length $ filter isMulti (map fst ps)
                             in or $ (numMulti>1):(map snd ps)
  alg (_ C.:< PBindF _ (Just p)) = snd p
  alg _ = False

-- The "multi-wildcard" pattern is only allowed in array patterns
invalidMulti :: P -> Bool
invalidMulti p = isMulti p || para alg p where
  alg (_ C.:< PPatF _ ps) = or $ map (isMulti . fst) ps ++ map snd ps
  alg (_ C.:< PTupF ps)   = or $ map (isMulti . fst) ps ++ map snd ps
  alg (_ C.:< POrF ps)    = or $ map (isMulti . fst) ps ++ map snd ps
  alg (_ C.:< PRecF fs _) = or $ map (isMulti . fst . snd) fs
                                   ++ map (snd . snd) fs
  alg (_ C.:< PArrF ps)   = or $ map snd ps
  alg (_ C.:< PBindF _ (Just p')) = isMulti (fst p') || snd p'
  alg _ = False
