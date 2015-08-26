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
defaultEnv = mkBuiltinEnv builtinFuncs builtinStructs builtinEnums

validateBuiltinAdt :: AlgDataTy a => [a] -> [(Text, PolyTy)]
validateBuiltinAdt =
  concatMap (\s -> case validateAdt s of
                        Left errs -> validationError s (tshow errs)
                        Right xs -> xs
            )
  
validationError :: AlgDataTy a => a -> Text -> b
validationError n xs = error . unpack $ "Builtin struct `" ++ tshow n
                                     ++ "` did not validate:\n" ++ xs

mkBuiltinEnv :: TyEnv -> [Struct] -> [Enumer] -> TyEnv
mkBuiltinEnv fs ss es =
  let structFns = validateBuiltinAdt ss
      enumerFns = validateBuiltinAdt es
   in case join $ liftA (`addToEnv` enumerFns) (addToEnv fs structFns) of
           Left dups -> error $ unpack . intercalate "\n" $
             "Duplicates in environment when adding builtins:" : dups
           Right env -> env

addToEnv :: TyEnv -> [(Text, PolyTy)] -> Either [Text] TyEnv
addToEnv = insertCollectDups

