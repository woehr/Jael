module Jael.Seq.Env
( defaultEnv
, addToEnv
) where

import Jael.Seq.Builtin
import Jael.Seq.HM_Types
import Jael.Seq.Prm
import Jael.Seq.UserDefinedType
import Jael.Util

defaultEnv :: TyEnv
defaultEnv =
  case builtinErrs of
       Just es -> error $ unpack . intercalate "\n"
                        $ "Errors validating builtins:" : es
       Nothing -> case addToEnv builtinFuncs $
                         prmFuncs ++
                         concatMap seqEnvItems builtinTypes
                  of
                       Left dups -> error $ unpack . intercalate "\n"
                                          $ "Duplicates in environment when\
                                            \ adding builtins:" : dups
                       Right env' -> env'

userDefErr :: (Text, UserDefinedType) -> Maybe Text
userDefErr (n, t) =
  case validateUDT t of
       Just e -> Just $ "Error validating " <> n <> ":\n\t" <> (pack . show) e
       Nothing -> Nothing

builtinErrs :: Maybe [Text]
builtinErrs = mapM userDefErr builtinTypes

addToEnv :: TyEnv -> [(Text, PolyTy)] -> Either [Text] TyEnv
addToEnv (TyEnv env) = liftA TyEnv . insertCollectDups env

