{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
--import Data.Functor.Foldable
import qualified Data.Map as M
--import qualified Data.Set as S
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.Types

data SessTyErr = SessTyErr
  deriving (Eq, Show)

-- Separate the arguments of a TopProc into linear sessions and base types
separateArgs :: [(Text, TyOrSess)] -> ([(Chan, Session)], [(Text, Ty)])
separateArgs xs = foldr
  (\(n, x) (ss, ts) -> case x of
                    TorSTy t   -> (ss, (n,t):ts)
                    TorSSess s -> (((n, Positive), s):ss, ts)
  ) ([],[]) xs

tyCheckTopProc :: TopProc -> Maybe [SessTyErr]
tyCheckTopProc (TopProc as p) =
  -- Separate arguments into linear and base types
  let (ls, bs) = separateArgs as
      env = ConcTyEnv
              { lin = M.fromList ls
              , base = M.fromList bs
              }
   in case tyCkProc env p of
           Left err -> Just [err]
           Right env' -> Nothing -- TODO: Unused linear resources are an error

-- Type checks a process. Returns either a type checking error or an updated
-- environment
tyCkProc :: ConcTyEnv -> Proc -> Either SessTyErr ConcTyEnv
tyCkProc env (PGet c v p) = undefined

