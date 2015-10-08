{-# Language NoImplicitPrelude #-}

module Jael.Compile where

import ClassyPrelude hiding (Enum, Foldable, Prim)
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Err
import Jael.Grammar
import Jael.Parser
import Jael.Util
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Hw.Area
import Jael.Seq.AST
import Jael.Seq.Enum
import Jael.Seq.Expr
import Jael.Seq.Struct
import Jael.Seq.Types
import Jael.UserDefTy

-- splits the top level definition into its different types of components
splitTop :: GProg -> ( [(Text, Ex)]      -- a
                     , [(Text, Struct)]  -- b
                     , [(Text, Enum)]    -- c
                     , [(Text, HwArea)]  -- d
                     , [(Text, Session)] -- e
                     , [(Text, TopProc)] -- f
                     , [(Text, ())] --HwProc)]  -- g
                     )
splitTop (GProg xs) = foldr (\x (a, b, c, d, e, f, g) ->
  case x of
       (GTopDefGGlobal (GGlobal (LIdent n) y))
         -> ((pack n, gToEx y)                         :a,b,c,d,e,f,g)
       (GTopDefGFunc (GFunc (GFuncName (LIdent n)) as y))
         -> ((pack n, fargsToAbs y as)                 :a,b,c,d,e,f,g)
       (GTopDefGTypeDef (GTDefStruct (UIdent n) y))
         -> (a,(pack n, gToUserDefTy y)                  :b,c,d,e,f,g)
       (GTopDefGTypeDef (GTDefEnum (UIdent n) y))
         -> (a,b,(pack n, gToUserDefTy y)                  :c,d,e,f,g)
       (GTopDefGTypeDef (GTDefArea (UIdent n) i y))
         -> (a,b,c,(pack n, gToUserDefTy (HwAreaGrammar i y)):d,e,f,g)
       (GTopDefGTypeDef (GTDefProto (UIdent n) y))
         -> (a,b,c,d,(pack n, gToUserDefTy y)                  :e,f,g)
       (GTopDefGProcDef (GProcDef (GProcName (UIdent n)) ys p))
         -> (a,b,c,d,e,              (pack n, gToTopProc (ys, p)):f,g)
       (GTopDefGHwProc  _) -> (a,b,c,d,e,f,g)
  ) ([],[],[],[],[],[],[]) xs

dupDefs :: [Text] -> CompileErrM ()
dupDefs ns =
  let repeats = repeated ns
   in if length repeats == 0
         then return ()
         else throwError $ DupDef repeats

defErrs :: [Struct]
        -> [Enum]
        -> [HwArea]
        -> [Session]
        -> [TopProc]
        -> CompileErrM ()
defErrs ss es as zs ps =
  let errs = mapMaybe (liftA tshow . validate) ss
          ++ mapMaybe (liftA tshow . validate) es
          ++ mapMaybe (liftA tshow . validate) as
          ++ mapMaybe (liftA tshow . validate) zs
          ++ mapMaybe (liftA tshow . validate) ps
   in if length errs == 0
         then return ()
         else throwError $ TypeDefErr errs

shadowingDef :: [Text] -> [(Text, TopProc)] -> CompileErrM ()
shadowingDef ns ps =
  let nameSet = S.fromList ns
      redefMap = foldr (\(n, (TopProc _ p)) a ->
          let redefs = redefinedCoRecVar nameSet p
           in if S.size redefs /= 0
                 then M.insert n redefs a
                 else a
        ) M.empty ps
   in if M.size redefMap /= 0
         then throwError $ AmbigName redefMap
         else return ()

undefinedNames :: M.Map Text (S.Set Text) -> CompileErrM ()
undefinedNames depMap =
  case hasUndefined depMap of
       Just undefed -> throwError $ UndefName undefed
       Nothing -> return ()

nameCycle :: M.Map Text (S.Set Text) -> CompileErrM [Text]
nameCycle depMap =
  case findCycles depMap of
       Left cycles -> throwError $ DepCycle cycles
       Right order -> return order

processSeqTypes :: [(Text, Struct)]
                -> [(Text, Enum)]
                -> [(Text, HwArea)]
                -> CompileErrM TyEnv
processSeqTypes = undefined

processConcTypes :: [(Text, HwArea)]
                 -> [(Text, Session)]
                 -> [(Text, ())] --HwProc)]
                 -> CompileErrM ConcTyEnv
processConcTypes = undefined

compile :: Text -> CompileErrM Text
compile p = do
  prog <- case parseProgram p of
               Left e  -> throwError $ ParseErr e
               Right x -> return x

  let (exprs, structs, enums, areas, protocols, procs, hwprocs) = splitTop prog

  let topLevelNames = (map fst exprs)     ++
                      (map fst structs)   ++
                      (map fst enums)     ++
                      (map fst areas)     ++
                      (map fst protocols) ++
                      (map fst procs)     ++
                      (map fst hwprocs)

  -- Find duplicate name definitions
  dupDefs topLevelNames

  -- Check for errors in a definition
  defErrs (map snd structs)
          (map snd enums)
          (map snd areas)
          (map snd protocols)
          (map snd procs)

  -- Check for names that shadow other names
  shadowingDef topLevelNames procs

  let exprDepMap = M.map freeVars . M.fromList $ exprs

  -- Find uses of undefined variables
  undefinedNames exprDepMap
  exprOrder <- nameCycle exprDepMap

  let typeDepMap = (M.map typeDeps . M.fromList $ structs)
         `M.union` (M.map typeDeps . M.fromList $ enums)
         `M.union` (M.map typeDeps . M.fromList $ areas)

  -- Find uses of undefined types
  undefinedNames typeDepMap
  typeOrder <- nameCycle typeDepMap

  -- Sessions can't refer to other sessions by name (yet) so there is no need to
  -- look for uses of undefined names

  let procDepMap = (M.map procDeps . M.fromList $ procs)
  -- Processes on the other hand can continue with a process by name, and they
  -- can not be called recursively (recursion must be explicitly defined).
  undefinedNames procDepMap

  -- Processes are explicitly typed so the order in which they're processed does
  -- not matter. We still need to check for, and prevent, recursion between and
  -- within named processes
  _ <- nameCycle procDepMap

  -- Create an environment from sequential types
  seqTyEnv <- processSeqTypes structs enums areas
  -- Create an environment from concurrent types
  conTyEnv <- processConcTypes areas protocols hwprocs
  -- Uses variables to quiet warnings
  undefined exprOrder typeOrder seqTyEnv conTyEnv

