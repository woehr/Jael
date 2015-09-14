{-# Language NoImplicitPrelude #-}

module Jael.Seq.Env
( defaultEnv
, addToEnv
) where

import ClassyPrelude
import Jael.Seq.Builtin
import Jael.Seq.Types
import Jael.Seq.UserDefTy
import Jael.Util

defaultEnv :: TyEnv
defaultEnv = mkBuiltinEnv builtinFuncs builtinTypes

validateBuiltins :: [(Text, UserDefTy)] -> [(Text, PolyTy)]
validateBuiltins =
  concatMap (\x -> case validateType x of
                        Left errs -> validationError x (tshow errs)
                        Right xs -> xs
            )

validationError :: (Text, UserDefTy) -> Text -> b
validationError n xs = error . unpack $ "Builtin struct `" ++ tshow n
                                     ++ "` did not validate:\n" ++ xs

mkBuiltinEnv :: TyEnv -> [(Text, UserDefTy)] -> TyEnv
mkBuiltinEnv env ts =
  let fns = validateBuiltins ts
   in case addToEnv env fns of
      Left dups -> error $ unpack . intercalate "\n" $
        "Duplicates in environment when adding builtins:" : dups
      Right env' -> env'

addToEnv :: TyEnv -> [(Text, PolyTy)] -> Either [Text] TyEnv
addToEnv (TyEnv env) = (liftA TyEnv) . insertCollectDups env

