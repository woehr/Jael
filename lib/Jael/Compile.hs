{-# Language NoImplicitPrelude #-}

module Jael.Compile where

import ClassyPrelude hiding (Foldable, Prim)
import Data.Foldable (foldrM)
import Data.Functor.Foldable
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Closure hiding (freeVars)
import Jael.Seq.Env
import Jael.Seq.Expr (freeVars, gToEx)
import Jael.Seq.TI
import Jael.Seq.Types
import Jael.Seq.UserDefTy

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefVar [Text]
                | UndefType [Text]
                | CallCycle [Text]
                | RecType [Text]
                | TypeDefErr [TDefError]
                | TypeInfErr [Text]
  deriving (Eq, Show)

data Global = Global Ex TyEnv
  deriving (Show)

-- splits the top level definition into its different types of components
splitTop :: GProg -> ([GTypeDef], [GGlobal], [GFunc], [GProc], [GHwproc])
splitTop (GProg xs) =
  foldr
    (\x (ts, gs, fs, ps, hs) -> case x of
          GTopDefGTypeDef t -> (t:ts, gs,   fs,   ps,   hs)
          GTopDefGGlobal  g -> (ts,   g:gs, fs,   ps,   hs)
          GTopDefGFunc    f -> (ts,   gs,   f:fs, ps,   hs)
          GTopDefGProc    p -> (ts,   gs,   fs,   p:ps, hs)
          GTopDefGHwproc  h -> (ts,   gs,   fs,   ps,   h:hs)
    )
    ([], [], [], [], [])
    xs

gglobToGlob :: GGlobal -> (Text, Global)
gglobToGlob (GGlobal (LIdent i) e) = (pack i, Global (gToEx e) (TyEnv M.empty))

gfuncToGlob :: GFunc -> (Text, Global)
gfuncToGlob (GFunc (LIdent i) as e) =
  (pack i, Global (cata argsToAbs as) (TyEnv M.empty))
  where argsToAbs :: Prim [GFuncArg] Ex -> Ex
        argsToAbs (Cons (GFuncArg (LIdent x)) xs) = EAbs (pack x) xs
        argsToAbs Nil = (gToEx e)

-- Find undefined variables for a given dependencies map
hasUndefined :: M.Map Text (S.Set Text) -> Maybe [Text]
hasUndefined deps =
  let ns = M.keysSet deps
      undef = M.filter (\x -> S.size (x S.\\ ns) /= 0) deps
   in if M.size undef /= 0
         then Just $ concatMap S.toList $ M.elems undef
         else Nothing

-- Given a map of names to their dependencies return either a list of names
-- that are part of cycles or a list of names that specify the (reverse) order
-- in which the names should be processed
findCycles :: M.Map Text (S.Set Text) -> Either [Text] [Text]
findCycles m =
  let (dg, vertToNode, _) = G.graphFromEdges $ map (\(a,b)->(a,a,b))
                                             $ M.toList (map S.toList m)
      nodeName = (\(a,_,_)->a) . vertToNode
      (lb, ub) = A.bounds dg
      -- For each node, find it's successors and determine if it has a path back
      recDeps = [nodeName x | x <- [lb..ub]
                            , y <- dg A.! x
                            , G.path dg y x]
   in if length recDeps /= 0
         then Left recDeps
         else Right . map nodeName . G.topSort $ dg

processTypes :: [GTypeDef] -> Either CompileErr TyEnv
processTypes xs = do
  -- map of type names to their UserDefTy
  userTys <- either (Left . DupDef) Right
               $ insertCollectDups M.empty
               $ map gToUserDefTy xs
  let tyDeps = map typeDependencies userTys
  -- Find use of undefined types or recursive types
  _ <- case hasUndefined tyDeps of
            Just x -> (Left . UndefType) x
            Nothing -> either (Left . RecType) Right $ findCycles tyDeps
  let (errs, fns) = M.mapEitherWithKey (curry validateType) userTys
  if M.size errs /= 0
     then Left $ TypeDefErr $ M.elems errs
     else either (Left . DupDef)
                 (Right . TyEnv)
                 (insertCollectDups (toMap defaultEnv) $ concat . M.elems $ fns)

-- TODO: Cleanup. Fixup env helpers. Handle errors better
tiGlobal :: TyEnv -> Global -> Either [Text] TypedEx
tiGlobal env (Global ex gEnv) =
  let env' = addToEnv env (M.toList . toMap $ gEnv)
   in case env' of
           Left es -> Left es
           Right env'' -> seqInferTypedEx env'' ex

processSeq :: TyEnv
           -> [GGlobal]
           -> [GFunc]
           -> Either CompileErr (M.Map Text TypedEx)
processSeq (TyEnv env) gs fs = do
  globExs <- either (Left . DupDef) Right $
    insertCollectDups M.empty (map gglobToGlob gs ++ map gfuncToGlob fs)
  -- Convert the global map to a map of names to deps
  globDeps <- (\x -> maybe (Right x) (Left . UndefVar) $ hasUndefined x)
              $ map (\(Global e _) -> freeVars e) globExs
  processOrder <- either (Left . CallCycle) Right (findCycles globDeps)
  -- Fold over processOrder adding the type of the result to the environment
  -- for the next inference invocation and collecting the resulting typed
  -- expressions
  -- TODO: Cleanup the unwieldiness
  liftM snd $ foldrM (\n (envAcc, exAcc) ->
    let ex = M.findWithDefault (error "Should not happen since key is \
                                  \obtained from the map we're indexing into.")
                               n
                               globExs
        res = tiGlobal (TyEnv envAcc) ex
     in case res of
             Left err -> Left $ TypeInfErr err
             Right typedEx -> Right ( M.insert n (polyTy (tyOf typedEx)) envAcc
                                    , M.insert n typedEx exAcc
                                    )
    )
    (env, M.empty)
    processOrder

processConc :: [GProc] -> Either CompileErr a
processConc ps = undefined

processHw :: [GHwproc] -> Either CompileErr a
processHw hs = undefined

compile :: Text -> Either CompileErr Text
compile p = do
  prog <- either (Left . ParseErr) Right $ parseProgram p
  let (types, globs, funcs, procs, hwprocs) = splitTop prog
  -- The map of global names to their expressions and type environments
  -- resulting from type annotations
  progTyEnv <- processTypes types
  -- TODO: Extract sequential fragments embedded within processes and hardware
  -- processes. Give them names and include them in the following processing
  seqTys <- processSeq progTyEnv globs funcs
  -- Now that all the sequential bits are named and typed, closure convert
  let globalNames = M.keysSet seqTys
  let ccSeqEx = M.mapWithKey (\k v -> closureConversion globalNames
                                                        (k ++ "'lam'")
                                                        v
                             ) seqTys
  Right $ tshow ccSeqEx
  -- Now take the closure converted fragments and generate llvm IR (or asm if
  -- necessary) annotated with worst case stack and execution usage

