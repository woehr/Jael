module Jael.Seq.Env
( defaultEnv
, addToEnv
) where

import Jael.Seq.Builtin
import Jael.Seq.Prm
import Jael.Seq.Types
import Jael.UserDefTy
import Jael.Util

defaultEnv :: TyEnv
defaultEnv =
  case builtinErrs of
       Just es -> error $ unpack . intercalate "\n"
                        $ "Errors validating builtins:" : es
       Nothing -> case addToEnv builtinFuncs $
                         prmFuncs ++
                         concatMap envItems builtinStruct ++
                         concatMap envItems builtinEnums
                  of
                       Left dups -> error $ unpack . intercalate "\n"
                                          $ "Duplicates in environment when\
                                            \ adding builtins:" : dups
                       Right env' -> env'

userDefErr :: UserDefTy a => (Text, a) -> Maybe Text
userDefErr (n, t) =
  case validate t of
       Just e -> Just $ "Error validating " <> n <> ":\n\t" <> (pack . show) e
       Nothing -> Nothing

builtinErrs :: Maybe [Text]
builtinErrs = liftA2 (++) (mapM userDefErr builtinStruct)
                          (mapM userDefErr builtinEnums)

addToEnv :: TyEnv -> [(Text, PolyTy)] -> Either [Text] TyEnv
addToEnv (TyEnv env) = liftA TyEnv . insertCollectDups env

