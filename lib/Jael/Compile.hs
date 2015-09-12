{-# Language NoImplicitPrelude #-}

module Jael.Compile where

import ClassyPrelude hiding (Foldable, Prim)
import Data.Functor.Foldable
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Expr
import Jael.Seq.Types

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefVar [Text]
                | CallCycle [Text]
                | RecType [Text]
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
-- that are part of cycles or a list of names that specify the order in which
-- the names should be processed
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
         else Right . reverse . map nodeName . G.topSort $ dg

compile :: Text -> Either CompileErr Text
compile p = do
  prog <- either (Left . ParseErr) Right $ parseProgram p
  let (types, globs, funcs, procs, hwprocs) = splitTop prog
  -- The map of global names to their expressions and type environments
  -- resulting from type annotations
  globExs <- either (Left . DupDef) Right $
    insertCollectDups M.empty (map gglobToGlob globs ++ map gfuncToGlob funcs)
  -- Convert the global map to a list mapping names to deps
  globDeps <- (\x -> maybe (Right x) (Left . UndefVar) $ hasUndefined x)
              $ map (\(Global e _) -> freeVars e) globExs
  processOrder <- either (Left . CallCycle) Right (findCycles globDeps)
  Right $ tshow processOrder

