{-# Language NoImplicitPrelude #-}

module Jael.Compile where

import ClassyPrelude hiding (Enum, Foldable, Prim)
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
import Jael.Seq.Expr (fargsToAbs, gToEx)
import Jael.Seq.Struct
import Jael.Seq.Types
import Jael.UserDefTy

-- splits the top level definition into its different types of components
splitTop :: GProg -> ( [(Text, Ex)]      -- a
                     , [(Text, Struct)]  -- b
                     , [(Text, Enum)]    -- c
                     , [(Text, HwArea)]  -- d
                     , [(Text, Session)] -- e
                     , [(Text, Proc)]    -- f
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
       (GTopDefGProcDef _) -> (a,b,c,d,e,f,g)
       (GTopDefGHwProc  _) -> (a,b,c,d,e,f,g)
  ) ([],[],[],[],[],[],[]) xs

dupDefs :: [Text] -> CompileErrM ()
dupDefs ns =
  let repeats = repeated ns
   in if length repeats == 0
         then return ()
         else throwError $ DupDef repeats

defErrs :: [Struct] -> [Enum] -> [HwArea] -> [Session] -> CompileErrM ()
defErrs = undefined

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

  -- Find duplicate name definitions
  dupDefs $ (map fst exprs)     ++
            (map fst structs)   ++
            (map fst enums)     ++
            (map fst areas)     ++
            (map fst protocols) ++
            (map fst procs)     ++
            (map fst hwprocs)

  -- Check for errors in a types definition
  defErrs (map snd structs)
          (map snd enums)
          (map snd areas)
          (map snd protocols)

  -- Create an environment from sequential types
  seqTyEnv <- processSeqTypes structs enums areas
  -- Create an environment from concurrent types
  conTyEnv <- processConcTypes areas protocols hwprocs
  undefined hwprocs seqTyEnv conTyEnv -- Uses variables to quiet warnings

