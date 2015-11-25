{-# Language RecordWildCards #-}

module Jael.Compile where

import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Util
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.CG_AST
import Jael.Seq.CG_Types
import Jael.Seq.Enum
import Jael.Seq.Env
import Jael.Seq.HM_AST
import Jael.Seq.HM_Types
import Jael.Seq.Struct
import Jael.UserDefTy

data CompileErr = ParseErr Text
                | DupDef [Text]
                | FuncArgDupDef Text
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr CGTypeErr
                | AmbigName (M.Map Text (S.Set Text))
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

type Program = ( [(Text, TopExpr CGEx CGTy)]
               , [(Text, Struct)]
               , [(Text, Enumer)]
               , [(Text, TopArea)]
               , [(Text, Session)]
               , [(Text, TopProc)]
               )

data TopExpr e t = TopGlob { tgExpr :: e }
                 | TopFunc { tfArgs  :: [(Text, t)]
                           , tfRetTy :: t
                           , tfExpr  :: e
                 } deriving (Show)

instance (SeqTypable e, SeqTypable t) => SeqTypable (TopExpr e t) where
  tyOf (TopGlob{..}) = tyOf tgExpr
  tyOf (TopFunc{..}) = typesToFun (map (tyOf . snd) tfArgs, tyOf tfRetTy)

gGlobToTop :: GExpr -> TopExpr CGEx t
gGlobToTop e = TopGlob { tgExpr = gToCGEx e }

gFuncToTop :: [GFuncArg] -> GType -> GExpr -> TopExpr CGEx CGTy
gFuncToTop as rt e = TopFunc { tfArgs = map (\(GFuncArg (LIdent n) t) ->
                                                (pack n, gToType t)
                                            )
                                            as
                             , tfRetTy = gToType rt
                             , tfExpr = gToCGEx e
                             }

exprFreeVars :: TopExpr CGEx t -> S.Set Text
exprFreeVars (TopGlob{..}) = freeVars tgExpr
exprFreeVars (TopFunc{..}) = freeVars tfExpr S.\\ S.fromList (map fst tfArgs)

recoverCGTop :: TopExpr TypedEx Ty -> TopExpr CGTypedEx CGTy
recoverCGTop = undefined

data TopArea = TopArea { taAddr :: Integer
                       , taType :: Text
                       } deriving (Show)

gToTopArea :: GAnyInt -> UIdent -> TopArea
gToTopArea i (UIdent t) = TopArea { taAddr = parseAnyInt i, taType = pack t }

-- splits the top level definition into its different types of components
splitTop :: GProg -> Program
splitTop (GProg xs) = foldr (\x (a, b, c, d, e, f) ->
  case x of
       (GTopDefGGlobal (GGlobal (LIdent n) y))
         -> ((pack n, gGlobToTop y)                    :a,b,c,d,e,f)
       (GTopDefGFunc (GFunc (GFuncName (LIdent n)) as retTy fnBody))
         -> ((pack n, gFuncToTop as retTy fnBody)      :a,b,c,d,e,f)
       (GTopDefGTypeDef (GTDefStruct (UIdent n) y))
         -> (a,(pack n, gToUserDefTy y)                  :b,c,d,e,f)
       (GTopDefGTypeDef (GTDefEnum (UIdent n) y))
         -> (a,b,(pack n, gToUserDefTy y)                  :c,d,e,f)
       (GTopDefGTypeDef (GTDefArea (UIdent n) addr ty))
         -> (a,b,c,(pack n, gToTopArea addr ty)              :d,e,f)
       (GTopDefGTypeDef (GTDefProto (UIdent n) y))
         -> (a,b,c,d,(pack n, dual $ gToUserDefTy y)           :e,f)
       (GTopDefGProcDef (GProcDef (GProcName (UIdent n)) ys p))
         -> (a,b,c,d,e,(pack n, gToTopProc (ys, p))              :f)
  ) ([],[],[],[],[],[]) xs

dupDefs :: [Text] -> CompileErrM ()
dupDefs ns =
  let repeats = repeated ns
   in unless (null repeats) (throwError $ DupDef repeats)

addToTyEnv :: TyEnv -> [(Text, PolyTy)] -> CompileErrM TyEnv
addToTyEnv env xs = either (throwError . DupDef) return (addToEnv env xs)

defErrs :: [Struct]
        -> [Enumer]
        -> [Session]
        -> [TopProc]
        -> CompileErrM ()
defErrs ss es zs ps =
  let errs = mapMaybe (liftA (pack . show) . validate) ss
          ++ mapMaybe (liftA (pack . show) . validate) es
          ++ mapMaybe (liftA (pack . show) . validate) zs
          ++ mapMaybe (liftA (pack . show) . validate) ps
   in unless (null errs)
        $ throwError $ TypeDefErr errs

shadowingDef :: [Text] -> [(Text, TopProc)] -> CompileErrM ()
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

processSeqTypes :: TyEnv
                -> M.Map Text Struct
                -> M.Map Text Enumer
                -> CompileErrM TyEnv
processSeqTypes env s e =
  let structItems = M.mapWithKey (curry envItems) s
      enumerItems = M.mapWithKey (curry envItems) e
   in addToTyEnv env (concat . M.elems $ structItems `M.union` enumerItems)

typeCheckTopExpr :: TyEnv -> TopExpr CGEx CGTy -> CompileErrM (TopExpr TypedEx Ty)
typeCheckTopExpr env (TopGlob{..}) =
  either (throwError . TypeInfErr)
         (\x -> return TopGlob{ tgExpr = x })
         (typeInf env tgExpr)
typeCheckTopExpr env (TopFunc{..}) =
  either (throwError . TypeInfErr)
         (\(as,rt,ex) -> return TopFunc{ tfArgs  = zip (map fst tfArgs) as
                                       , tfRetTy = rt
                                       , tfExpr  = ex }
         )
         (typeCheckFunc env (tfArgs, tfRetTy, tfExpr))

typeCheckSeq :: M.Map Text (TopExpr CGEx CGTy)
             -> [Text]
             -> TyEnv
             -> CompileErrM (M.Map Text (TopExpr TypedEx Ty))
typeCheckSeq exprs order env = do
  mapTypedTopExpr <- foldM (\(env', acc) def ->
    case M.lookup def exprs of
         Just x -> do
           typedTopEx <- typeCheckTopExpr env' x
           env'' <- addToTyEnv env' [(def, polyTy (tyOf typedTopEx))]
           return (env'', M.insert def typedTopEx acc)
         Nothing -> error "Any name in the order list should be in the map of\
                         \ top level expressions."
    ) (env, M.empty) order
  return (snd mapTypedTopExpr)

processConcTypes :: M.Map Text Session
                 -> CompileErrM ConcTyEnv
processConcTypes = undefined

nameChecks :: Program -> CompileErrM ()
nameChecks (exprs, structs, enums, areas, protocols, procs) = do
  let topLevelNames = map fst exprs     ++
                      map fst structs   ++
                      map fst enums     ++
                      map fst areas     ++
                      map fst protocols ++
                      map fst procs

  -- Find duplicate name definitions
  dupDefs topLevelNames

  -- Checks for recursive process names that conflict
  shadowingDef topLevelNames procs

-- Checks for circular dependencies in types, expressions, and processes
-- Returns the order in which expressions and types should be processed
dependencyAndUndefinedChecks :: M.Map Text (TopExpr CGEx CGTy)
                             -> M.Map Text Struct
                             -> M.Map Text Enumer
                             -> M.Map Text Session
                             -> M.Map Text TopProc
                             -> CompileErrM ([Text], [Text])
dependencyAndUndefinedChecks exprs structs enums protocols procs = do
  -- Find uses of undefined variables
  let exprDepMap = M.map exprFreeVars exprs
  undefinedNames exprDepMap
  exprOrder <- liftM reverse $ nameCycle exprDepMap

  -- Find uses of undefined types
  let typeDepMap = M.map typeDeps structs `M.union` M.map typeDeps enums
  undefinedNames typeDepMap
  typeOrder <- liftM reverse $ nameCycle typeDepMap

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

  return (typeOrder, exprOrder)

annotateSeqExprs :: M.Map Text Struct
                 -> M.Map Text Enumer
                 -> M.Map Text (TopExpr CGEx CGTy)
                 -> [Text]
                 -> CompileErrM (M.Map Text (TopExpr CGTypedEx CGTy))
annotateSeqExprs structs enums exprs exprOrder = do
  env <- processSeqTypes defaultEnv structs enums
  -- The TopExpr map is returned with CG types and expr replaced by HM ones
  hmTypedExprs <- typeCheckSeq exprs exprOrder env
  return $ M.map recoverCGTop hmTypedExprs

compile :: Text -> CompileErrM Text
compile p = do
  prog <- case parseProgram p of
               Left e  -> throwError $ ParseErr e
               Right x -> return x

  let components@(lExprs, lStructs, lEnums, lAreas, lProtocols, lProcs)
        = splitTop prog

  nameChecks components

  -- No more chance of duplicate names, so now we use a map.
  let (exprs, structs, enums, areas, protocols, procs) =
        ( M.fromList lExprs
        , M.fromList lStructs
        , M.fromList lEnums
        , M.fromList lAreas
        , M.fromList lProtocols
        , M.fromList lProcs
        )

  -- Check for errors in a definition
  defErrs (M.elems structs)
          (M.elems enums)
          (M.elems protocols)
          (M.elems procs)

  (_, exprOrder) <- dependencyAndUndefinedChecks
                              exprs structs enums protocols procs

  -- Type check all expressions in the order required by their dependencies
  typedExprs <- annotateSeqExprs structs enums exprs exprOrder

  -- Create an environment from concurrent types
  concTyEnv <- processConcTypes protocols
  return "Unimplemented"

