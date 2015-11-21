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
import Jael.Seq.Enum
import Jael.Seq.Env
import Jael.Seq.HM_Types
import Jael.Seq.Struct
import Jael.UserDefTy

data CompileErr = ParseErr Text
                | DupDef [Text]
                | UndefName (S.Set Text)
                | DepCycle [Text]
                | TypeDefErr [Text]
                | TypeInfErr [Text]
                | AmbigName (M.Map Text (S.Set Text))
  deriving (Eq, Show)

type CompileErrM = Either CompileErr

data TopGlob = TopGlob { tgExpr :: CGEx }
               deriving (Show)

gToTopGlob :: GExpr -> TopGlob
gToTopGlob e = TopGlob { tgExpr = gToCGEx e }

globFreeVars :: TopGlob -> S.Set Text
globFreeVars = freeVars . tgExpr

data TopFunc = TopFunc { tfArgs  :: [(Text, Ty)]
                       , tfRetTy :: Ty
                       , tfExpr  :: CGEx }
               deriving (Show)

gToTopFunc :: [GFuncArg] -> GType -> GExpr -> TopFunc
gToTopFunc as rt e = TopFunc { tfArgs = map (\(GFuncArg (LIdent n) t) ->
                                               (pack n, gToType t)
                                            )
                                           as
                             , tfRetTy = gToType rt
                             , tfExpr = gToCGEx e
                             }

funcFreeVars :: TopFunc -> S.Set Text
funcFreeVars (TopFunc{tfArgs=as, tfExpr=ex}) =
  freeVars ex S.\\ S.fromList (map fst as)

data TopArea = TopArea { taAddr :: Integer
                       , taType :: Text }
                       deriving (Show)

gToTopArea :: GAnyInt -> UIdent -> TopArea
gToTopArea i (UIdent t) = TopArea { taAddr = parseAnyInt i, taType = pack t }

-- splits the top level definition into its different types of components
splitTop :: GProg -> ( [(Text, TopGlob)] -- a
                     , [(Text, Struct)]  -- b
                     , [(Text, Enumer)]  -- c
                     , [(Text, TopArea)] -- d
                     , [(Text, Session)] -- e
                     , [(Text, TopProc)] -- f
                     , [(Text, TopFunc)] -- g
                     )
splitTop (GProg xs) = foldr (\x (a, b, c, d, e, f, g) ->
  case x of
       (GTopDefGGlobal (GGlobal (LIdent n) y))
         -> ((pack n, gToTopGlob y)                    :a,b,c,d,e,f,g)
       (GTopDefGTypeDef (GTDefStruct (UIdent n) y))
         -> (a,(pack n, gToUserDefTy y)                  :b,c,d,e,f,g)
       (GTopDefGTypeDef (GTDefEnum (UIdent n) y))
         -> (a,b,(pack n, gToUserDefTy y)                  :c,d,e,f,g)
       (GTopDefGTypeDef (GTDefArea (UIdent n) addr ty))
         -> (a,b,c,(pack n, gToTopArea addr ty)              :d,e,f,g)
       (GTopDefGTypeDef (GTDefProto (UIdent n) y))
         -> (a,b,c,d,(pack n, dual $ gToUserDefTy y)           :e,f,g)
       (GTopDefGProcDef (GProcDef (GProcName (UIdent n)) ys p))
         -> (a,b,c,d,e,(pack n, gToTopProc (ys, p))              :f,g)
       (GTopDefGFunc (GFunc (GFuncName (LIdent n)) as retTy fnBody))
         -> (a,b,c,d,e,f,(pack n, gToTopFunc as retTy fnBody)      :g)
  ) ([],[],[],[],[],[],[]) xs

dupDefs :: [Text] -> CompileErrM ()
dupDefs ns =
  let repeats = repeated ns
   in unless (null repeats)
        $ throwError $ DupDef repeats

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
                -> [(Text, Struct)]
                -> [(Text, Enumer)]
                -> CompileErrM TyEnv
processSeqTypes env s e =
  let newItems = concatMap envItems s ++ concatMap envItems e
   in case addToEnv env newItems of
           Left dups  -> throwError $ DupDef dups
           Right env' -> return env'

typeCheckSeq :: M.Map Text (Either TopGlob TopFunc)
             -> [Text]
             -> TyEnv
             -> CompileErrM TyEnv
typeCheckSeq exprs = flip $ foldM (\acc def ->
    case M.lookup def exprs of
         Just (Left glob)  -> undefined
         Just (Right func) -> undefined
         Nothing -> error "Any name in the order list should be in the map of\
                         \ top level expressions."
  )

processConcTypes :: [(Text, Session)]
                 -> CompileErrM ConcTyEnv
processConcTypes = undefined

compile :: Text -> CompileErrM Text
compile p = do
  prog <- case parseProgram p of
               Left e  -> throwError $ ParseErr e
               Right x -> return x

  let (globs, structs, enums, areas, protocols, procs, funcs) = splitTop prog

  let topLevelNames = map fst globs     ++
                      map fst structs   ++
                      map fst enums     ++
                      map fst areas     ++
                      map fst protocols ++
                      map fst procs     ++
                      map fst funcs

  -- Find duplicate name definitions
  dupDefs topLevelNames

  -- Check for errors in a definition
  defErrs (map snd structs)
          (map snd enums)
          (map snd protocols)
          (map snd procs)

  -- Checks for recursive process names that conflict
  shadowingDef topLevelNames procs

  let globDepMap = M.map globFreeVars . M.fromList $ globs
  let funcDepMap = M.map funcFreeVars . M.fromList $ funcs
  let exprDepMap = globDepMap `M.union` funcDepMap

  -- Find uses of undefined variables
  undefinedNames exprDepMap
  exprOrder <- nameCycle exprDepMap

  let typeDepMap = (M.map typeDeps . M.fromList $ structs)
         `M.union` (M.map typeDeps . M.fromList $ enums)

  -- Find uses of undefined types
  undefinedNames typeDepMap
  typeOrder <- nameCycle typeDepMap

  let sessDepMap = M.map freeIndVars . M.fromList $ protocols
  -- Find uses of undefined sessions
  undefinedNames sessDepMap
  _ <- nameCycle sessDepMap

  let procDepMap = M.map procDeps . M.fromList $ procs
  -- Processes on the other hand can continue with a process by name, and they
  -- can not be called recursively (recursion must be explicitly defined).
  undefinedNames procDepMap

  -- Processes are explicitly typed so the order in which they're processed does
  -- not matter. We still need to check for and prevent recursion between and
  -- within named processes
  _ <- nameCycle procDepMap

  -- Run
  let globAndFuncMap = M.fromList $ map (second Left)  globs
                                 ++ map (second Right) funcs
  seqTyEnv <- processSeqTypes defaultEnv structs enums
          >>= typeCheckSeq globAndFuncMap exprOrder

  -- Create an environment from concurrent types
  conTyEnv <- processConcTypes protocols
  -- Uses variables to quiet warnings
  undefined exprOrder typeOrder seqTyEnv conTyEnv

