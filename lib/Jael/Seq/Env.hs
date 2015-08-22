{-# Language NoImplicitPrelude #-}

module Jael.Seq.Env
( defaultEnv
, addToEnv
) where

import ClassyPrelude
import Jael.Seq.AlgDataTy
import Jael.Seq.AST
import Jael.Seq.Builtin
import Jael.Util

defaultEnv :: TyEnv
defaultEnv = mkBuiltinEnv builtinFuncs builtinStructs -- builtinEnums

validateBuiltinStructs :: [Struct] -> [(Text, PolyTy)]
validateBuiltinStructs =
  concatMap (\s@(Struct n _ _) -> case validateAdt s of
                                       Left errs -> structError n (tshow errs)
                                       Right xs -> xs
            )
  
structError :: Text -> Text -> a
structError n xs = error . unpack $ "Builtin struct `" ++ n
                                  ++ "` did not validate:\n" ++ xs

mkBuiltinEnv :: TyEnv -> [Struct] -> TyEnv
mkBuiltinEnv fs ss = case addToEnv fs (validateBuiltinStructs ss) of
                          Left dups -> error $
                            unpack . intercalate "\n" $ "Duplicates in environment when adding builtins:" : dups
                          Right env -> env

addToEnv :: TyEnv -> [(Text, PolyTy)] -> Either [Text] TyEnv
addToEnv = insertCollectDups

