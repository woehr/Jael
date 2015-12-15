{-# Language RecordWildCards #-}

module Jael.Compile.Stage1 where

import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Util
import Jael.Compile.Common
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.AST
import Jael.Seq.Types
import Jael.Seq.UserDefinedType

gGlobToTop :: GExpr -> TopExpr S1Ex t
gGlobToTop e = TopGlob { tgExpr = gToS1Ex e }

gFuncToTop :: [GFuncArg] -> GType -> GExpr -> TopExpr S1Ex S1Ty
gFuncToTop as rt e = TopFunc { tfArgs = map (\(GFuncArg (LIdent n) t) ->
                                                (pack n, gToType t)
                                            )
                                            as
                             , tfRetTy = gToType rt
                             , tfExpr = gToS1Ex e
                             }

gToTopArea :: GAnyInt -> UIdent -> TopArea
gToTopArea i (UIdent t) = TopArea { taAddr = parseAnyInt i, taType = pack t }

-- splits the top level definition into its different types of components
splitTop :: GProg -> ( [(Text, S1TopExpr)]
                     , [(Text, UserDefinedType)]
                     , [(Text, TopArea)]
                     , [(Text, Either SessDefErr Session)]
                     , [(Text, Either ProcDefErr S1TopProc)]
                     )
splitTop (GProg xs) = foldr (\x (a, b, c, d, e) ->
  case x of
       (GTopDefGGlobal (GGlobal (LIdent n) y))
         -> ((pack n, gGlobToTop y)                      :a,b,c,d,e)
       (GTopDefGFunc (GFunc (GFuncName (LIdent n)) as retTy fnBody))
         -> ((pack n, gFuncToTop as retTy fnBody)        :a,b,c,d,e)
       (GTopDefGTypeDef (GTDefStruct (UIdent n) y))
         -> (a,(pack n, gStructToUDT y)                    :b,c,d,e)
       (GTopDefGTypeDef (GTDefEnum (UIdent n) y))
         -> (a,(pack n, gEnumToUDT y)                      :b,c,d,e)
       (GTopDefGTypeDef (GTDefArea (UIdent n) addr ty))
         -> (a,b,(pack n, gToTopArea addr ty)                :c,d,e)
       (GTopDefGTypeDef (GTDefProto (UIdent n) y))
         -> (a,b,c,(pack n, gToSession y)                      :d,e)
       (GTopDefGProcDef (GProcDef (GProcName (UIdent n)) ys p))
         -> (a,b,c,d,(pack n, gToTopProc (ys, p))                :e)
  ) ([],[],[],[],[]) xs

dupDefs :: [Text] -> CompileErrM ()
dupDefs ns =
  let repeats = repeated ns
   in unless (null repeats) (throwError $ DupDef repeats)

shadowingDef :: [Text] -> [(Text, S1TopProc)] -> CompileErrM ()
shadowingDef ns ps =
  let nameSet = S.fromList ns
      redefMap = foldr (\(n, TopProc _ p) a ->
          let redefs = redefinedCoRecVar nameSet p
           in if S.size redefs /= 0
                 then M.insert n redefs a
                 else a
        ) M.empty ps
   in unless (null redefMap)
        $ throwError $ AmbigName redefMap

nameChecks :: [Text] -> [(Text, S1TopProc)] -> CompileErrM ()
nameChecks ns procs = do
  -- Find duplicate name definitions
  dupDefs ns

  -- Checks for recursive process names that conflict
  shadowingDef ns procs

defErrs :: [UserDefinedType]
        -> CompileErrM ()
defErrs udts =
  let errs = mapMaybe (liftA (pack . show) . validateUDT) udts
   in unless (null errs)
        $ throwError $ TypeDefErr errs

undefinedNames :: M.Map Text (S.Set Text) -> CompileErrM ()
undefinedNames depMap =
  case hasUndefined depMap of
       Just undefed -> throwError $ UndefName undefed
       Nothing -> return ()

exprFreeVars :: TopExpr S1Ex t -> S.Set Text
exprFreeVars (TopGlob{..}) = freeVars tgExpr
exprFreeVars (TopFunc{..}) = freeVars tfExpr S.\\ S.fromList (map fst tfArgs)

nameCycle :: M.Map Text (S.Set Text) -> CompileErrM [Text]
nameCycle depMap =
  case findCycles depMap of
       Left cycles -> throwError $ DepCycle cycles
       Right order -> return order

-- Checks for circular dependencies in types, expressions, and processes
-- Returns the order in which expressions and types should be processed
dependencyAndUndefinedChecks :: M.Map Text (TopExpr S1Ex S1Ty)
                             -> M.Map Text UserDefinedType
                             -> M.Map Text Session
                             -> M.Map Text S1TopProc
                             -> CompileErrM [Text]
dependencyAndUndefinedChecks exprs udts protocols procs = do
  -- Find uses of undefined variables
  let exprDepMap = M.map exprFreeVars exprs
  undefinedNames exprDepMap
  exprOrder <- liftM reverse $ nameCycle exprDepMap

  -- Find uses of undefined types
  let typeDepMap = M.map typeDependencies udts
  undefinedNames typeDepMap
  _ <- liftM reverse $ nameCycle typeDepMap

  -- Find uses of undefined sessions
  let sessDepMap = M.map freeIndVars protocols
  undefinedNames sessDepMap
  _ <- nameCycle sessDepMap

  -- Processes on the other hand can continue with a process by name, and they
  -- can not be called recursively (recursion must be explicitly defined).
  let procDepMap = M.map procDeps procs
  undefinedNames procDepMap

  -- Processes are explicitly typed so the order in which they're processed does
  -- not matter. We still need to check for and prevent recursion between and
  -- within named processes
  _ <- nameCycle procDepMap

  return exprOrder

stage1 :: Text -> CompileErrM Stage1
stage1 p = do
  prog <- case parseProgram p of
               Left e  -> throwError $ ParseErr e
               Right x -> return x

  let (lExprs, lUDTs, lAreas, lEProtocols, lEProcs) = splitTop prog

  lProtocols <- either (throwError . undefined) return $ sequenceA (map undefined lEProtocols)
  lProcs     <- either (throwError . undefined) return $ sequenceA (map undefined lEProcs)

  nameChecks (map fst lExprs ++
              map fst lUDTs  ++
              map fst lAreas ++
              map fst lEProtocols ++
              map fst lEProcs)
             lProcs

  -- No more chance of duplicate names, so now we use a map.
  let (exprs, udts, areas, protocols, procs) =
        ( M.fromList lExprs
        , M.fromList lUDTs
        , M.fromList lAreas
        , M.fromList lProtocols
        , M.fromList lProcs
        )

  -- Check for errors in a definition
  defErrs (M.elems udts)

  exprOrder <- dependencyAndUndefinedChecks exprs udts protocols procs
  return Stage1
           { s1Exprs     = exprs
           , s1Udts      = udts
           , s1Areas     = areas
           , s1Protos    = protocols
           , s1Procs     = procs
           , s1ExprOrder = exprOrder
           }

