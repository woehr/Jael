{-# Language TypeFamilies #-}

module Jael.Conc.Proc where

import qualified Data.Functor.Foldable as F
import qualified Data.Set as S
import Jael.Grammar
import Jael.Conc.Session
import Jael.Seq.AST
import Jael.Seq.Types
import Jael.Util

data ProcDefErr = PDEDupRecVar Text
                | PDESessDefErr SessDefErr
                | PDEDupArgs (S.Set Text)
                  deriving (Eq, Show)

type Channel = Text
type Var = Text
type Label = Text

type ChanEx a = Either Channel a

data TyOrSess t s = TorSTy t
                  | TorSSess s
                  deriving (Eq, Show)

type S1TyOrSess = TyOrSess S1Ty Session
type S2TyOrSess = TyOrSess S2Ty Session

data TopProc t s p = TopProc [(Text, TyOrSess t s)] p
  deriving (Eq, Show)

type S1TopProc = TopProc S1Ty Session S1Proc
type S2TopProc = TopProc S2Ty Session S2Proc

-- The expression extracted from the process
data S2PEx = S2PEx { peExpr :: S2TyEx
                   } deriving (Eq, Show)

data S1Proc = S1PGetChan Channel   Channel S1Proc
            | S1PGetVal  Channel   Text    S1Proc
            | S1PGetIgn  Channel           S1Proc
            | S1PPutChan Channel   Channel S1Proc
            | S1PPutVal  Channel   S1Ex    S1Proc
            | S1PNewVal  Text      S1Ex    S1Proc
            | S1PNewChan Text Text Session S1Proc
            | S1PPar     [S1Proc]
            | S1PCase    Channel [(Label, S1Proc)]
            | S1PSel     Channel   Label  S1Proc
            | S1PCoRec   Text [(Var, ChanEx S1Ex)] S1Proc
            | S1PFwd     Channel Channel
            | S1PNamed   Text [ChanEx S1Ex]
            | S1PNil
            deriving (Eq, Show)

data S1ProcF a = S1PGetChanF Channel   Channel a
               | S1PGetValF  Channel   Text    a
               | S1PGetIgnF  Channel           a
               | S1PPutChanF Channel   Channel a
               | S1PPutValF  Channel   S1Ex    a
               | S1PNewValF  Text      S1Ex    a
               | S1PNewChanF Text Text Session a
               | S1PParF     [a]
               | S1PCaseF    Channel [(Label, a)]
               | S1PSelF     Channel   Label  a
               | S1PCoRecF   Text [(Var, ChanEx S1Ex)] a
               | S1PFwdF     Channel Channel
               | S1PNamedF   Text [ChanEx S1Ex]
               | S1PNilF
               deriving (Functor, Show)

type instance F.Base S1Proc = S1ProcF

instance F.Foldable S1Proc where
  project (S1PGetChan x y z)   = S1PGetChanF x y z
  project (S1PGetVal x y z)    = S1PGetValF x y z
  project (S1PGetIgn x y)      = S1PGetIgnF x y
  project (S1PPutChan x y z)   = S1PPutChanF x y z
  project (S1PPutVal x y z)    = S1PPutValF x y z
  project (S1PNewVal x y z)    = S1PNewValF x y z
  project (S1PNewChan w x y z) = S1PNewChanF w x y z
  project (S1PPar x)           = S1PParF x
  project (S1PCase x y)        = S1PCaseF x y
  project (S1PSel x y z)       = S1PSelF x y z
  project (S1PCoRec x y z)     = S1PCoRecF x y z
  project (S1PFwd x y)         = S1PFwdF x y
  project (S1PNamed x y)       = S1PNamedF x y
  project (S1PNil)             = S1PNilF

instance F.Unfoldable S1Proc where
  embed (S1PGetChanF x y z)   = S1PGetChan x y z
  embed (S1PGetValF x y z)    = S1PGetVal x y z
  embed (S1PGetIgnF x y)      = S1PGetIgn x y
  embed (S1PPutChanF x y z)   = S1PPutChan x y z
  embed (S1PPutValF x y z)    = S1PPutVal x y z
  embed (S1PNewValF x y z)    = S1PNewVal x y z
  embed (S1PNewChanF w x y z) = S1PNewChan w x y z
  embed (S1PParF x)           = S1PPar x
  embed (S1PCaseF x y)        = S1PCase x y
  embed (S1PSelF x y z)       = S1PSel x y z
  embed (S1PCoRecF x y z)     = S1PCoRec x y z
  embed (S1PFwdF x y)         = S1PFwd x y
  embed (S1PNamedF x y)       = S1PNamed x y
  embed (S1PNilF)             = S1PNil

data S2Proc = S2PGetChan Channel   Channel S2Proc
            | S2PGetVal  Channel   Text    S2Proc
            | S2PGetIgn  Channel           S2Proc
            | S2PPutChan Channel   Channel S2Proc
            | S2PPutVal  Channel   S2PEx   S2Proc
            | S2PNewVal  Text      S2PEx   S2Proc
            | S2PNewChan Text Text Session S2Proc
            | S2PPar     [S2Proc]
            | S2PCase    Channel [(Label, S2Proc)]
            | S2PSel     Channel   Label  S2Proc
            | S2PCoRec   Text    [(Var, ChanEx S2PEx)] S2Proc
            | S2PFwd     Channel Channel
            | S2PNamed   Text    [ChanEx S2PEx]
            | S2PNil
            deriving (Eq, Show)

data S2ProcF a = S2PGetChanF Channel   Channel a
               | S2PGetValF  Channel   Text    a
               | S2PGetIgnF  Channel           a
               | S2PPutChanF Channel   Channel a
               | S2PPutValF  Channel   S2PEx   a
               | S2PNewValF  Text      S2PEx   a
               | S2PNewChanF Text Text Session a
               | S2PParF     [a]
               | S2PCaseF    Channel [(Label, a)]
               | S2PSelF     Channel   Label  a
               | S2PCoRecF   Text    [(Var, ChanEx S2PEx)] a
               | S2PFwdF     Channel Channel
               | S2PNamedF   Text    [ChanEx S2PEx]
               | S2PNilF
               deriving (Functor, Show)

type instance F.Base S2Proc = S2ProcF

instance F.Foldable S2Proc where
  project (S2PGetChan x y z)   = S2PGetChanF x y z
  project (S2PGetVal x y z)    = S2PGetValF x y z
  project (S2PGetIgn x y)      = S2PGetIgnF x y
  project (S2PPutChan x y z)   = S2PPutChanF x y z
  project (S2PPutVal x y z)    = S2PPutValF x y z
  project (S2PNewVal x y z)    = S2PNewValF x y z
  project (S2PNewChan w x y z) = S2PNewChanF w x y z
  project (S2PPar x)           = S2PParF x
  project (S2PCase x y)        = S2PCaseF x y
  project (S2PSel x y z)       = S2PSelF x y z
  project (S2PCoRec x y z)     = S2PCoRecF x y z
  project (S2PFwd x y)         = S2PFwdF x y
  project (S2PNamed x y)       = S2PNamedF x y
  project (S2PNil)             = S2PNilF

instance F.Unfoldable S2Proc where
  embed (S2PGetChanF x y z)   = S2PGetChan x y z
  embed (S2PGetValF x y z)    = S2PGetVal x y z
  embed (S2PGetIgnF x y)      = S2PGetIgn x y
  embed (S2PPutChanF x y z)   = S2PPutChan x y z
  embed (S2PPutValF x y z)    = S2PPutVal x y z
  embed (S2PNewValF x y z)    = S2PNewVal x y z
  embed (S2PNewChanF w x y z) = S2PNewChan w x y z
  embed (S2PParF x)           = S2PPar x
  embed (S2PCaseF x y)        = S2PCase x y
  embed (S2PSelF x y z)       = S2PSel x y z
  embed (S2PCoRecF x y z)     = S2PCoRec x y z
  embed (S2PFwdF x y)         = S2PFwd x y
  embed (S2PNamedF x y)       = S2PNamed x y
  embed (S2PNilF)             = S2PNil

data ProcGramState = ProcGramState
                   { pgsRecDefs :: S.Set Text
                   } deriving (Show)

type ProcGramM = ExceptT ProcDefErr (State ProcGramState)

gToProc :: GProc -> Either ProcDefErr S1Proc
gToProc g =
  let (retVal, _) = runState (runExceptT (gToProc' g))
                             ProcGramState{pgsRecDefs = S.empty}
   in retVal

gChanToText :: GChan -> Text
gChanToText (GChan (LIdent x)) = pack x
gChanToText (GChanScoped (LScopedIdent x)) = pack x

gChoiceToProc :: [GConcChoice] -> Either ProcDefErr [(Text, S1Proc)]
gChoiceToProc = sequenceA . map (\(GConcChoice (LIdent x) p) -> liftA ((,) (pack x)) (gToProc p))

gToInitList :: [GRecInitializer] -> ProcGramM [(Text, ChanEx S1Ex)]
gToInitList as =
  let as' = map (\i -> case i of
        (GRecInitializerChan (LIdent x) c) ->
          (pack x, Left $ gChanToText c)
        (GRecInitializerExpr (LIdent x) y) -> (pack x, Right $ gToS1Ex y)
        ) as
      dups = repeated $ map fst as'
   in do
      unless (null dups) $ throwError (PDEDupArgs $ S.fromList dups)
      return as'

gProcParamToEx :: GProcParam -> ChanEx S1Ex
gProcParamToEx (GProcParamChan c) = Left (gChanToText c)
gProcParamToEx (GProcParamExpr x) = Right $ gToS1Ex x

gToProc' :: GProc -> ProcGramM S1Proc
gToProc' (GProcNew (LIdent x) (LIdent y) s p) =
  case gToSession s of
       Left err -> throwError $ PDESessDefErr err
       Right s' -> liftM (S1PNewChan (pack x) (pack y) s') $ gToProc' p

gToProc' (GProcLet (LIdent x) y p) = liftA (S1PNewVal (pack x) (gToS1Ex y)) (gToProc' p)
gToProc' (GProcGetExpr c (LIdent y) p) = liftA (S1PGetVal (gChanToText c) (pack y)) (gToProc' p)
gToProc' (GProcGetChan c (LIdent y) p) = liftA (S1PGetChan (gChanToText c) (pack y)) (gToProc' p)
gToProc' (GProcGetIgn  c p) = liftA (S1PGetIgn (gChanToText c)) (gToProc' p)
gToProc' (GProcPutExpr c ex p) = liftA (S1PPutVal (gChanToText c) (gToS1Ex ex)) (gToProc' p)
gToProc' (GProcPutChan c1 c2 p) = liftA (S1PPutChan (gChanToText c1) (gChanToText c2)) (gToProc' p)
gToProc' (GProcSel c (LIdent y) p) = liftA (S1PSel (gChanToText c) (pack y)) (gToProc' p)
gToProc' (GProcRec (GProcName (UIdent x)) i p) = do
  let x' = pack x
  recs <- gets pgsRecDefs
  when (x' `S.member` recs) $ throwError (PDEDupRecVar x')
  modify (\s -> s{pgsRecDefs=S.insert x' recs})
  liftA2 (S1PCoRec x') (gToInitList i) (gToProc' p)

gToProc' (GProcNamed (GProcName (UIdent x)) params) = return $ S1PNamed (pack x) (map gProcParamToEx params)
gToProc' (GProcInact) = return $ S1PNil
gToProc' (GProcFwd c1 c2) = return $ S1PFwd (gChanToText c1) (gChanToText c2)

gToProc' (GProcCho c ys)  = liftM (S1PCase $ gChanToText c) $ mapM
                              (\(GConcChoice (LIdent l) p) -> do
                                origSt <- get
                                liftM (pack l,) (gToProc' p) <* modify (const origSt)
                              ) ys
gToProc' (GProcPar e1 es) = liftM S1PPar $ mapM
                              (\(GParElem e) -> do
                                origSt <- get
                                gToProc' e <* modify (const origSt)
                              ) (e1 : es)

gToProcArg :: GProcArg -> Either ProcDefErr (Text, S1TyOrSess)
gToProcArg (GProcArgType (LIdent i) x) =
  return (pack i, TorSTy $ gToType x)
gToProcArg (GProcArgSess (LIdent i) x) =
  either (throwError . PDESessDefErr) (\s -> return (pack i, TorSSess s)) (gToSession x)

gToTopProc :: ([GProcArg], GProc) -> Either ProcDefErr S1TopProc
gToTopProc (as, p) = do
  as' <- sequence $ map gToProcArg as
  p' <- gToProc p
  let dups = repeated . map fst $ as'
  unless (null dups) $ throwError (PDEDupArgs $ S.fromList dups)
  return $ TopProc as' p'

procDeps :: S1TopProc -> S.Set Text
procDeps (TopProc _ p) = F.cata alg p
  where alg (S1PNamedF n _)   = S.singleton n
        alg (S1PCoRecF n _ x) = S.delete n x
        alg (S1PNewChanF _ _ _ x) = x
        alg (S1PNewValF  _ _ x) = x
        alg (S1PGetChanF _ _ x) = x
        alg (S1PGetValF  _ _ x) = x
        alg (S1PGetIgnF    _ x) = x
        alg (S1PPutValF  _ _ x) = x
        alg (S1PPutChanF _ _ x) = x
        alg (S1PSelF     _ _ x) = x
        alg (S1PCaseF _ xs) = S.unions (map snd xs)
        alg (S1PParF    xs) = S.unions xs
        alg _ = S.empty

coRecNames :: S1Proc -> S.Set Text
coRecNames = F.cata alg
  where alg (S1PCoRecF n _ x) = n `S.insert` x
        alg (S1PParF     xs) = S.unions xs
        alg (S1PCaseF _  xs) = S.unions $ map snd xs
        alg (S1PNewChanF _ _ _ x) = x
        alg (S1PNewValF    _ _ x) = x
        alg (S1PGetChanF   _ _ x) = x
        alg (S1PGetValF    _ _ x) = x
        alg (S1PGetIgnF      _ x) = x
        alg (S1PPutChanF   _ _ x) = x
        alg (S1PPutValF    _ _ x) = x
        alg (S1PSelF       _ _ x) = x
        alg _ = S.empty

-- Given a set of names and a process, return the set of names that are both
-- in the first set and the set of co-recursive variable names
redefinedCoRecVar :: S.Set Text -> S1Proc -> S.Set Text
redefinedCoRecVar ns p = ns `S.intersection` coRecNames p
