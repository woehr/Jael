{-# Language TypeFamilies #-}

module Jael.Conc.Proc where

import Data.Functor.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Seq.CG_AST
import Jael.Seq.HM_Types
import Jael.Conc.Session
import Jael.UserDefTy
import Jael.Util

data ProcDefErr = ProcDefErr
  { pErrFreeVars :: S.Set Text
  , pErrDupArgs :: S.Set Text
  , pErrCoRecVarCapture :: M.Map Text (S.Set Text)
  , pErrAmbiguousRecName :: S.Set Text
  } deriving (Eq, Show)

type Channel = Text
type Var  = Text
type Label = Text

type ChanEx = Either Channel CGEx

data TyOrSess = TorSTy Ty
              | TorSSess Session
              deriving (Show)

data TopProc = TopProc [(Text, TyOrSess)] Proc
  deriving (Show)

instance UserDefTy TopProc where
  type TGrammar TopProc = ([GProcArg], GProc)
  type TError   TopProc = ProcDefErr
  type TEnvItem TopProc = ()

  gToUserDefTy = gToTopProc
  validate = validateTopProc
  typeDeps _ = S.empty
  envItems _ = []

data Proc = PGetChan Channel Channel Proc
          | PGetVal  Channel Text    Proc
          | PGetIgn  Channel         Proc
          | PPutChan Channel Channel Proc
          | PPutVal  Channel CGEx    Proc
          | PNewVal  Text    CGEx    Proc
          | PNewChan Text Text Session Proc
          | PPar     [Proc]
          | PCase    Channel [(Label, Proc)]
          | PSel     Channel Label Proc
          | PCoRec   Text [(Var, ChanEx)] Proc
          | PFwd     Channel Channel
          | PNamed   Text [ChanEx]
          | PNil
          deriving (Eq, Show)

data ProcF a = PGetChanF Channel Channel a
             | PGetValF  Channel Text a
             | PGetIgnF  Channel      a
             | PPutChanF Channel Channel a
             | PPutValF  Channel CGEx   a
             | PNewValF  Text CGEx a
             | PNewChanF Text Text Session a
             | PParF [a]
             | PCaseF Channel [(Label, a)]
             | PSelF Channel Label a
             | PCoRecF Text [(Var, ChanEx)] a
             | PFwdF Channel Channel
             | PNamedF Text [ChanEx]
             | PNilF
             deriving (Functor, Show)

type instance Base Proc = ProcF

instance F.Foldable Proc where
  project (PGetChan x y z) = PGetChanF x y z
  project (PGetVal x y z)  = PGetValF x y z
  project (PGetIgn x y)    = PGetIgnF x y
  project (PPutChan x y z) = PPutChanF x y z
  project (PPutVal x y z)  = PPutValF x y z
  project (PNewVal x y z)  = PNewValF x y z
  project (PNewChan w x y z) = PNewChanF w x y z
  project (PPar x)       = PParF x
  project (PCase x y)    = PCaseF x y
  project (PSel x y z)   = PSelF x y z
  project (PCoRec x y z) = PCoRecF x y z
  project (PFwd x y)     = PFwdF x y
  project (PNamed x y)   = PNamedF x y
  project (PNil)         = PNilF

instance F.Unfoldable Proc where
  embed (PGetChanF x y z) = PGetChan x y z
  embed (PGetValF x y z)  = PGetVal x y z
  embed (PGetIgnF x y)    = PGetIgn x y
  embed (PPutChanF x y z) = PPutChan x y z
  embed (PPutValF x y z)  = PPutVal x y z
  embed (PNewValF x y z) = PNewVal x y z
  embed (PNewChanF w x y z) = PNewChan w x y z
  embed (PParF x)       = PPar x
  embed (PCaseF x y)    = PCase x y
  embed (PSelF x y z)   = PSel x y z
  embed (PCoRecF x y z) = PCoRec x y z
  embed (PFwdF x y)     = PFwd x y
  embed (PNamedF x y)   = PNamed x y
  embed (PNilF)         = PNil

gChanToText :: GChan -> Text
gChanToText (GChan (LIdent x)) = pack x
gChanToText (GChanScoped (LScopedIdent x)) = pack x

gChoiceToProc :: [GConcChoice] -> [(Text, GProc)]
gChoiceToProc = map (\(GConcChoice (GChoiceLabel (LIdent x)) p) -> (pack x, p))

gToInitList :: [GRecInitializer] -> [(Text, ChanEx)]
gToInitList = map (\i -> case i of
    (GRecInitializerChan (LIdent x) c) ->
      (pack x, Left $ gChanToText c)
    (GRecInitializerExpr (LIdent x) y) -> (pack x, Right $ gToCGEx y)
  )

gParElemToProc :: GParElem -> GProc
gParElemToProc (GParElem p) = p

gProcParamToEx :: GProcParam -> ChanEx
gProcParamToEx (GProcParamChan c) = Left (gChanToText c)
gProcParamToEx (GProcParamExpr x) = Right $ gToCGEx x

gToProc :: GProc -> Proc
gToProc = ana coalg
  where coalg :: GProc -> Base Proc GProc
        coalg (GProcNew (LIdent x) (LIdent y) s p
              ) = PNewChanF (pack x) (pack y) (gToSession s) p
        coalg (GProcLet (LIdent x) y p
              ) = PNewValF (pack x) (gToCGEx y) p
        coalg (GProcGetExpr c (LIdent y) p
              ) = PGetValF (gChanToText c) (pack y) p
        coalg (GProcGetChan c (LIdent y) p
              ) = PGetChanF (gChanToText c) (pack y) p
        coalg (GProcGetIgn  c p
              ) = PGetIgnF (gChanToText c) p
        coalg (GProcPutExpr c ex p
              ) = PPutValF (gChanToText c) (gToCGEx ex) p
        coalg (GProcPutChan c1 c2 p
              ) = PPutChanF (gChanToText c1) (gChanToText c2) p
        coalg (GProcSel c (GChoiceLabel (LIdent y)) p
              ) = PSelF (gChanToText c) (pack y) p
        coalg (GProcCho c ys
              ) = PCaseF (gChanToText c) (gChoiceToProc ys)
        coalg (GProcRec (GProcName (UIdent x)) i p
              ) = PCoRecF (pack x) (gToInitList i) p
        coalg (GProcNamed (GProcName (UIdent x)) params
              ) = PNamedF (pack x) (map gProcParamToEx params)
        coalg (GProcInact
              ) = PNilF
        coalg (GProcFwd c1 c2
              ) = PFwdF (gChanToText c1) (gChanToText c2)
        coalg (GProcPar e1 es
              ) = PParF (gParElemToProc e1 : map gParElemToProc es)

procDeps :: TopProc -> S.Set Text
procDeps (TopProc _ p) = cata alg p
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PNamedF n _)   = S.singleton n
        alg (PCoRecF n _ x) = S.delete n x
        alg (PNewChanF _ _ _ x) = x
        alg (PNewValF _ _ x) = x
        alg (PGetChanF    _ _ x) = x
        alg (PGetValF    _ _ x) = x
        alg (PGetIgnF    _ x) = x
        alg (PPutValF    _ _ x) = x
        alg (PPutChanF    _ _ x) = x
        alg (PSelF    _ _ x) = x
        alg (PCaseF _ xs) = S.unions (map snd xs)
        alg (PParF    xs) = S.unions xs
        alg _ = S.empty

-- TODO: Maybe create a type class for things that can have free variables to
-- avoid having a new function name like this.
cexFreeVars :: ChanEx -> S.Set Text
cexFreeVars (Left c)   = S.singleton c
cexFreeVars (Right ex) = freeVars ex

procFreeVars :: Proc -> S.Set Text
procFreeVars = cata alg
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PNamedF _ as) = S.unions $ map cexFreeVars as
        alg (PCoRecF _ as p) = S.unions (map (cexFreeVars . snd) as)
                              `S.union` (p S.\\ S.fromList (map fst as))
        alg (PNewChanF v v' _ p) = v `S.delete` (v' `S.delete` p)
        alg (PNewValF  v e  p) = v `S.delete` (p `S.union` freeVars e)
        alg (PGetChanF  c v p) = c `S.insert` (v `S.insert` p)
        alg (PGetValF  c e p) = c `S.insert` (e `S.insert` p)
        alg (PGetIgnF  c p) = c `S.insert` p
        alg (PPutChanF  c v p) = c `S.insert` (v `S.insert` p)
        alg (PPutValF  c e p) = c `S.insert` (freeVars e `S.union` p)
        alg (PSelF  c _ p) = c `S.insert` p
        alg (PCaseF c  xs) = c `S.insert` S.unions (map snd xs)
        alg (PParF xs) = S.unions xs
        alg (PFwdF x y) = S.fromList [x, y]
        alg _ = S.empty

coRecCapturedVars :: Proc -> M.Map Text (S.Set Text)
coRecCapturedVars = para alg
  where alg :: Base Proc (Proc, M.Map Text (S.Set Text))
            -> M.Map Text (S.Set Text)
        alg (PCoRecF n as (p, m)) =
          let free = procFreeVars p S.\\ S.fromList (map fst as)
           in if S.size free /= 0
                 then M.insert n free m
                 else m
        alg (PParF    xs) = M.unions $ map snd xs
        alg (PCaseF _ xs) = M.unions $ map (snd . snd) xs
        alg (PNewChanF _ _ _ (_, x)) = x
        alg (PNewValF _ _ (_, x)) = x
        alg (PGetChanF  _ _ (_, x)) = x
        alg (PGetValF  _ _ (_, x)) = x
        alg (PGetIgnF  _ (_, x)) = x
        alg (PPutChanF  _ _ (_, x)) = x
        alg (PPutValF  _ _ (_, x)) = x
        alg (PSelF  _ _ (_, x)) = x
        alg _ = M.empty

recDupArgs :: Proc -> S.Set Text
recDupArgs = cata alg
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PCoRecF _ as x) = (S.fromList . repeated $ map fst as) `S.union` x
        alg (PParF     xs) = S.unions xs
        alg (PCaseF _  xs) = S.unions $ map snd xs
        alg (PNewChanF _ _ _ x) = x
        alg (PNewValF _ _ x) = x
        alg (PGetChanF  _ _ x) = x
        alg (PGetValF  _ _ x) = x
        alg (PGetIgnF  _ x) = x
        alg (PPutChanF  _ _ x) = x
        alg (PPutValF  _ _ x) = x
        alg (PSelF  _ _ x) = x
        alg _ = S.empty

ambigCoRecDef :: Proc -> S.Set Text
ambigCoRecDef = para alg
  where alg :: Base Proc (Proc, S.Set Text) -> S.Set Text
        alg (PCoRecF n _ (p, x)) = if n `S.member` coRecNames p
                                      then n `S.insert` x
                                      else x
        alg (PParF     xs) = S.unions $ map snd xs
        alg (PCaseF _  xs) = S.unions $ map (snd . snd) xs
        alg (PNewChanF _ _ _ (_, x)) = x
        alg (PNewValF _ _ (_, x)) = x
        alg (PGetChanF  _ _ (_, x)) = x
        alg (PGetValF  _ _ (_, x)) = x
        alg (PGetIgnF  _ (_, x)) = x
        alg (PPutChanF  _ _ (_, x)) = x
        alg (PPutValF  _ _ (_, x)) = x
        alg (PSelF  _ _ (_, x)) = x
        alg _ = S.empty

coRecNames :: Proc -> S.Set Text
coRecNames = cata alg
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PCoRecF n _ x) = n `S.insert` x
        alg (PParF     xs) = S.unions xs
        alg (PCaseF _  xs) = S.unions $ map snd xs
        alg (PNewChanF _ _ _ x) = x
        alg (PNewValF _ _ x) = x
        alg (PGetChanF  _ _ x) = x
        alg (PGetValF  _ _ x) = x
        alg (PGetIgnF  _ x) = x
        alg (PPutChanF  _ _ x) = x
        alg (PPutValF  _ _ x) = x
        alg (PSelF  _ _ x) = x
        alg _ = S.empty

-- Given a set of names and a process, return the set of names that are both
-- in the first set and the set of co-recursive variable names
redefinedCoRecVar :: S.Set Text -> Proc -> S.Set Text
redefinedCoRecVar ns p = ns `S.intersection` coRecNames p

gToProcArg :: GProcArg -> (Text, TyOrSess)
gToProcArg (GProcArgType (LIdent i) x) =
  (pack i, TorSTy (gToType x))
gToProcArg (GProcArgSess (LIdent i) x) =
  (pack i, TorSSess (gToSession x))

gToTopProc :: ([GProcArg], GProc) -> TopProc
gToTopProc (as, p) = TopProc (map gToProcArg as) (gToProc p)

validateTopProc :: TopProc -> Maybe ProcDefErr
validateTopProc (TopProc as p) =
  let dupArgs = S.fromList (repeated $ map fst as) `S.union` recDupArgs p
      free = procFreeVars p S.\\ S.fromList (map fst as)
      varCapt = coRecCapturedVars p
      ambigNames = ambigCoRecDef p
   in if S.size dupArgs    /= 0 ||
         S.size free       /= 0 ||
         M.size varCapt    /= 0 ||
         S.size ambigNames /= 0
         then Just ProcDefErr
                     { pErrDupArgs = dupArgs
                     , pErrFreeVars = free
                     , pErrCoRecVarCapture = M.map (S.\\ free) varCapt
                     , pErrAmbiguousRecName = ambigNames
                     }
         else Nothing

