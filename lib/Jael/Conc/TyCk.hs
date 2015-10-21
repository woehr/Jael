{-# Language NoImplicitPrelude #-}

module Jael.Conc.TyCk where

import ClassyPrelude hiding (Chan, Foldable)
import Control.Monad.Except
import qualified Data.Map as M
import Jael.Util
import Jael.Conc.Env
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Seq.AST
import Jael.Seq.Env
import Jael.Seq.TI
import Jael.Seq.Types

type SessTyErrM = Either SessTyErr

data SessTyErr = UnusedLin (M.Map Chan Session)
               | UnusedSeq (M.Map Text Ty)
               | UndefinedChan Chan
               | RedefinedName Text
               | ProtocolMismatch Chan Session
               | TypeMismatch Chan Ty
               | DuplicateSeqEnvItem [Text]
               | SeqTIErrs [Text]
               | UnknownLabel Text
               | FreshNonParallel Text
               | DualChanArgs (Text, Text)
               | PrintThing Text
  deriving (Eq, Show)

preconditionError :: a
preconditionError = error "Some assumption about a precondition has been violated."

-- Adds (or replaces) the session, v, with channel name, k, to the environment
-- but first unfolds it until the session is no longer a SCoInd, SVar, or
-- SDualVar and updates the environment as necessary
unfoldSession :: Text -> Session -> ConcTyEnv -> ConcTyEnv
unfoldSession k (SCoInd var s) env@(ConcTyEnv{recs=recVars}) =
  unfoldSession k s env{recs=M.insert (k, var) s recVars}

unfoldSession k (SVar var) env@(ConcTyEnv{recs=recVars, aliases=als}) =
  case M.lookup (k, var) recVars of
       Just s -> unfoldSession k s env
       Nothing -> unfoldSession k (M.findWithDefault preconditionError var als)
                    env{recs=M.filterWithKey (\(c,_) _ -> c /= k) recVars}

unfoldSession k (SDualVar var) env@(ConcTyEnv{recs=recVars, aliases=als})
  | M.member (k, var) recVars = error "A recursion variable should not be dualed."
  | otherwise = unfoldSession k (dual $ M.findWithDefault preconditionError var als) env

unfoldSession k v env@(ConcTyEnv{lin=linEnv}) = env{lin=M.insert k v linEnv}

addIfNotRedefinition :: Text -> EnvValue -> ConcTyEnv -> SessTyErrM ConcTyEnv
addIfNotRedefinition k v env@(ConcTyEnv {lin=lEnv, base=bEnv, duals=dEnv}) =
 let add = \kv m -> case addIfUnique kv m of
                         Just m' -> return m'
                         Nothing -> throwError $ RedefinedName k
 in case v of
         Linear s -> add (k, s) lEnv >>= (\lEnv' -> return $
                                           unfoldSession k s env{lin  =lEnv'})
         Base   t -> add (k, t) bEnv >>= (\bEnv' -> return $ env{base =bEnv'})
         Dual   c -> add (k, c) dEnv >>= (\dEnv' -> return $ env{duals=dEnv'})

updateSession :: Text -> Session -> ConcTyEnv -> ConcTyEnv
updateSession k v env@(ConcTyEnv {lin=linEnv}) =
  case M.lookup k linEnv of
       Just _ -> unfoldSession k v env
       Nothing -> error "It is expected that if a channel's session is being\
                       \ updated that it already exists in the environment."

channelLookup :: Chan -> ConcTyEnv -> SessTyErrM Session
channelLookup c (ConcTyEnv{lin=linEnv}) =
  case M.lookup c linEnv of
       Just s  -> return s
       Nothing -> throwError $ UndefinedChan c

mkSeqEnv :: ConcTyEnv -> SessTyErrM TyEnv
mkSeqEnv (ConcTyEnv{base=bEnv, seqEnv=sEnv}) =
  case addToEnv sEnv $ M.toList $ M.map polyTy bEnv of
    Left errs -> throwError $ DuplicateSeqEnvItem errs
    Right env -> return env

-- Separate the arguments of a TopProc into linear sessions and base types
separateArgs :: [(Text, TyOrSess)] -> ([(Chan, Session)], [(Text, Ty)])
separateArgs xs = foldr
  (\(n, x) (ss, ts) -> case x of
                    TorSTy t   -> (ss, (n,t):ts)
                    TorSSess s -> ((n,s):ss, ts)
  ) ([],[]) xs

tyCheckTopProc :: TyEnv
               -> M.Map Text Session
               -> M.Map Text [(Text, TyOrSess)]
               -> TopProc
               -> Maybe SessTyErr
tyCheckTopProc sEnv sessNames namedProcs (TopProc as p) =
  -- Separate arguments into linear and base types
  let (ls, bs) = separateArgs as
  -- Do some processing of the top level sessions so type checking can match on
  -- what we expect the session to be. The updateSession function does the same
  -- thing if the session after unfolding is SCoInd, SVar, or SDualVar
      env = ConcTyEnv
              { lin = M.empty
              , recs = M.empty
              , duals = M.empty
              , base = M.fromList bs
              , seqEnv = sEnv
              , aliases = sessNames
              }
   -- Just the error or throw away the returned env and return Nothing
   in either Just (const Nothing) $ tyCkProc (foldr (uncurry unfoldSession) env ls) p
      --Just $ PrintThing $ tshow $ tyCkProc env p

-- Type checks a process. Returns either a type checking error or an updated
-- environment
tyCkProc :: ConcTyEnv -> Proc -> SessTyErrM ConcTyEnv
tyCkProc env (PGet c name p) = do
  -- Get the session the channel c is suppose to implement
  sess <- channelLookup c env
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
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' p

tyCkProc env@(ConcTyEnv{lin=lEnv}) (PPut c chanOrExpr pCont) = do
  sess <- channelLookup c env
  env' <- case (sess, chanOrExpr) of
               (SPutTy v sCont, Right putExpr) -> do
                  completeSeqEnv <- mkSeqEnv env
                  case seqInfer completeSeqEnv (ELet "'" putExpr $ EVar "'") of
                       Left errs -> throwError $ SeqTIErrs errs
                       Right ty -> if ty /= v
                                      then throwError $ TypeMismatch c ty
                                      else return $ updateSession c sCont env
               (SPutSess v sCont, Left putChan) -> do
                  putSess <- channelLookup putChan env
                  if putSess /= v
                     then throwError $ ProtocolMismatch c putSess
                     else return $ updateSession c sCont env{lin=M.delete putChan lEnv}
               _ -> throwError $ ProtocolMismatch c sess
  tyCkProc env' pCont

tyCkProc env (PSel chan lab pCont) = do
  sess <- channelLookup chan env
  env' <- case sess of
               SSelect ls -> case lookup lab ls of
                                  Just s -> return $ updateSession chan s env
                                  Nothing -> throwError $ UnknownLabel lab
               _ -> throwError $ ProtocolMismatch chan sess
  tyCkProc env' pCont

tyCkProc env PNil =
  let unusedLinear = M.filter (/= SEnd) (lin env)
   in if null unusedLinear
         then return env
         else throwError $ UnusedLin unusedLinear

