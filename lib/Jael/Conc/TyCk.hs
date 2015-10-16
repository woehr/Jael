{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
import Control.Monad.Except
import qualified Data.Map as M
import Jael.Util
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

type SessTyErrM = Either SessTyErr

data SessTyErr = UnusedLin (M.Map Chan Session)
               | UndefinedChan Chan
               | RedefinedName Text
               | ProtocolMismatch Chan Session
  deriving (Eq, Show)

addIfNotRedefinition :: Text -> EnvValue -> ConcTyEnv -> SessTyErrM ConcTyEnv
addIfNotRedefinition k v env@(ConcTyEnv {lin=lEnv, base=bEnv, duals=dEnv}) =
 let add = \kv m -> case addIfUnique kv m of
                         Just m -> return m
                         Nothing -> throwError $ RedefinedName k
 in case v of
         Linear s -> add (k, s) lEnv >>= (\lEnv' -> return $ env{lin  =lEnv'})
         Base   t -> add (k, t) bEnv >>= (\bEnv' -> return $ env{base =bEnv'})
         Dual   c -> add (k, c) dEnv >>= (\dEnv' -> return $ env{duals=dEnv'})

updateSession :: Text -> Session -> ConcTyEnv -> ConcTyEnv
updateSession k v env@(ConcTyEnv {lin=linEnv}) =
  case M.lookup k linEnv of
       Just _ -> env{lin=M.insert k v linEnv}
       Nothing -> error "It is expected that if a channel's session is being\
                       \ updated that it already exists in the environment."

-- Separate the arguments of a TopProc into linear sessions and base types
separateArgs :: [(Text, TyOrSess)] -> ([(Chan, Session)], [(Text, Ty)])
separateArgs xs = foldr
  (\(n, x) (ss, ts) -> case x of
                    TorSTy t   -> (ss, (n,t):ts)
                    TorSSess s -> ((n,s):ss, ts)
  ) ([],[]) xs

tyCheckTopProc :: TopProc -> Maybe SessTyErr
tyCheckTopProc (TopProc as p) =
  -- Separate arguments into linear and base types
  let (ls, bs) = separateArgs as
      env = ConcTyEnv
              { lin = M.fromList ls
              , duals = M.empty
              , base = M.fromList bs
              }
   -- Just the error or throw away the returned env and return Nothing
   in either Just (const Nothing) $ tyCkProc env p

-- Type checks a process. Returns either a type checking error or an updated
-- environment
tyCkProc :: ConcTyEnv -> Proc -> SessTyErrM ConcTyEnv
tyCkProc env (PGet c name p) = do
  -- Get the session the channel c is suppose to implement
  sess <- case M.lookup c (lin env) of
               Just s  -> return s
               Nothing -> throwError $ UndefinedChan c
  -- Check that the session implements a "get", update the session in the
  -- environment, and introduce the new name
  env' <- case sess of
               SGetTy   v s -> do
                  env'  <- addIfNotRedefinition name (Base v) env
                  -- c has to already be in the environment so an insert replaces
                  -- the old session with the updated one
                  return $ updateSession c s env'
               SGetSess v s -> do
                  env'  <- addIfNotRedefinition name (Linear v) env
                  return $ updateSession c s env'
               s -> throwError $ ProtocolMismatch c s
  return env'

tyCkProc env PNil =
  let unusedLinear = M.filter (\x -> x /= SEnd) (lin env)
   in if null unusedLinear
         then return env
         else throwError $ UnusedLin unusedLinear

