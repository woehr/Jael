{-# Language NoImplicitPrelude #-}
{-# Language TypeFamilies #-}

module Jael.Conc.Proc where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Seq.AST
import Jael.Seq.Expr
import Jael.Seq.Types
import Jael.Conc.Session
import Jael.UserDefTy
import Jael.Util

data ProcDefErr = ProcDefErr
  { pErrFreeVars :: S.Set Text
  , pErrDupArgs :: S.Set Text
  , pErrCoRecVarCapture :: M.Map Text (S.Set Text)
  , pErrAmbiguousRecName :: S.Set Text
  } deriving (Eq, Show)

type Chan = Text
type Var  = Text
type Label = Text

type ChanEx = Either Chan Ex

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

data Proc = PGet Chan Var Proc
          | PPut Chan ChanEx Proc
          | PNewVal Text Ex Proc
          | PNewChan Text Text Session Proc
          | PPar [Proc]
          | PCase Chan [(Label, Proc)]
          | PSel Chan Label Proc
          | PCoRec Text [(Var, ChanEx)] Proc
          | PFwd Chan Chan
          | PNamed Text [ChanEx]
          | PNil
          deriving (Eq, Show)

data ProcF a = PGetF Chan Var a
             | PPutF Chan ChanEx a
             | PNewValF Text Ex a
             | PNewChanF Text Text Session a
             | PParF [a]
             | PCaseF Chan [(Label, a)]
             | PSelF Chan Label a
             | PCoRecF Text [(Var, ChanEx)] a
             | PFwdF Chan Chan
             | PNamedF Text [ChanEx]
             | PNilF
             deriving (Functor, Show)

type instance Base Proc = ProcF

instance Foldable Proc where
  project (PGet x y z)   = PGetF x y z
  project (PPut x y z)   = PPutF x y z
  project (PNewVal x y z) = PNewValF x y z
  project (PNewChan w x y z) = PNewChanF w x y z
  project (PPar x)       = PParF x
  project (PCase x y)    = PCaseF x y
  project (PSel x y z)   = PSelF x y z
  project (PCoRec x y z) = PCoRecF x y z
  project (PFwd x y)     = PFwdF x y
  project (PNamed x y)   = PNamedF x y
  project (PNil)         = PNilF

instance Unfoldable Proc where
  embed (PGetF x y z)   = PGet x y z
  embed (PPutF x y z)   = PPut x y z
  embed (PNewValF x y z) = PNewVal x y z
  embed (PNewChanF w x y z) = PNewChan w x y z
  embed (PParF x)       = PPar x
  embed (PCaseF x y)    = PCase x y
  embed (PSelF x y z)   = PSel x y z
  embed (PCoRecF x y z) = PCoRec x y z
  embed (PFwdF x y)     = PFwd x y
  embed (PNamedF x y)   = PNamed x y
  embed (PNilF)         = PNil

gScopedToText :: [GScopeElem] -> Text
gScopedToText = intercalate "::" . map (\(GScopeElem (LIdent x)) -> pack x)

gChoiceToProc :: [GConcChoice] -> [(Text, GProc)]
gChoiceToProc = map (\(GConcChoice (GChoiceLabel (LIdent x)) p) -> (pack x, p))

gToChanEx :: GChanOrExpr -> ChanEx
gToChanEx (GChanOrExprC (GChan (GScopedIdent c))) = Left (gScopedToText c)
gToChanEx (GChanOrExprE ex) = Right (gToEx ex)

gToInitList :: [GRecInitializer] -> [(Text, ChanEx)]
gToInitList = map (\(GRecInitializer (LIdent x) y) -> (pack x, gToChanEx y))

gParElemToProc :: GParElem -> GProc
gParElemToProc (GParElem p) = p

gProcParamToEx :: GProcParam -> ChanEx
gProcParamToEx (GProcParam x) = gToChanEx x

gToProc :: GProc -> Proc
gToProc = ana coalg
  where coalg :: GProc -> Base Proc GProc
        coalg (GProcNew (LIdent x) (LIdent y) s p
              ) = PNewChanF (pack x) (pack y) (gToSession s) p
        coalg (GProcLet (LIdent x) y p
              ) = PNewValF (pack x) (gToEx y) p
        coalg (GProcGet (GChan (GScopedIdent xs)) (LIdent y) p
              ) = PGetF (gScopedToText xs) (pack y) p
        coalg (GProcPut (GChan (GScopedIdent xs)) cex p
              ) = PPutF (gScopedToText xs) (gToChanEx cex) p
        coalg (GProcSel (GChan (GScopedIdent xs)) (GChoiceLabel (LIdent y)) p
              ) = PSelF (gScopedToText xs) (pack y) p
        coalg (GProcCho (GChan (GScopedIdent xs)) ys
              ) = PCaseF (gScopedToText xs) (gChoiceToProc ys)
        coalg (GProcRec (GProcName (UIdent x)) inits p
              ) = PCoRecF (pack x) (gToInitList inits) p
        coalg (GProcNamed (GProcName (UIdent x)) params
              ) = PNamedF (pack x) (map gProcParamToEx params)
        coalg (GProcInact
              ) = PNilF
        coalg (GProcPar e1 es
              ) = PParF (gParElemToProc e1 : map gParElemToProc es)

procDeps :: TopProc -> S.Set Text
procDeps (TopProc _ p) = cata alg p
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PNamedF n _)   = S.singleton n
        alg (PCoRecF n _ x) = n `S.delete` x
        alg (PNewChanF _ _ _ x) = x
        alg (PNewValF _ _ x) = x
        alg (PGetF    _ _ x) = x
        alg (PPutF    _ _ x) = x
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
                              `S.union` (p S.\\ (S.fromList $ map fst as))
        alg (PNewChanF v v' _ p) = v `S.delete` (v' `S.delete` p)
        alg (PNewValF  v e  p) = v `S.delete` (p `S.union` freeVars e)
        alg (PGetF  c v p) = c `S.insert` (v `S.insert` p)
        alg (PPutF  c e p) = c `S.insert` (cexFreeVars e `S.union` p)
        alg (PSelF  c _ p) = c `S.insert` p
        alg (PCaseF c  xs) = c `S.insert` S.unions (map snd xs)
        alg (PParF xs) = S.unions xs
        alg _ = S.empty

coRecCapturedVars :: Proc -> M.Map Text (S.Set Text)
coRecCapturedVars = para alg
  where alg :: Base Proc (Proc, (M.Map Text (S.Set Text)))
            -> M.Map Text (S.Set Text)
        alg (PCoRecF n as (p, m)) =
          let free = (procFreeVars p) S.\\ (S.fromList $ map fst as)
           in if S.size free /= 0
                 then M.insert n free m
                 else m
        alg (PParF    xs) = M.unions $ map snd xs
        alg (PCaseF _ xs) = M.unions $ map (snd . snd) xs
        alg (PNewChanF _ _ _ (_, x)) = x
        alg (PNewValF _ _ (_, x)) = x
        alg (PGetF  _ _ (_, x)) = x
        alg (PPutF  _ _ (_, x)) = x
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
        alg (PGetF  _ _ x) = x
        alg (PPutF  _ _ x) = x
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
        alg (PGetF  _ _ (_, x)) = x
        alg (PPutF  _ _ (_, x)) = x
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
        alg (PGetF  _ _ x) = x
        alg (PPutF  _ _ x) = x
        alg (PSelF  _ _ x) = x
        alg _ = S.empty

-- Given a set of names and a process, return the set of names that are both
-- in the first set and the set of co-recursive variable names
redefinedCoRecVar :: S.Set Text -> Proc -> S.Set Text
redefinedCoRecVar ns p = ns `S.intersection` coRecNames p

gToProcArg :: GProcArg -> (Text, TyOrSess)
gToProcArg (GProcArg (LIdent i) (GSessTy x)) =
  (pack i, TorSTy (gToType x))
gToProcArg (GProcArg (LIdent i) (GSessSess x)) =
  (pack i, TorSSess (gToSession x))

gToTopProc :: ([GProcArg], GProc) -> TopProc
gToTopProc (as, p) = TopProc (map gToProcArg as) (gToProc p)

validateTopProc :: TopProc -> Maybe ProcDefErr
validateTopProc (TopProc as p) =
  let dupArgs = S.fromList (repeated $ map fst as) `S.union` recDupArgs p
      free = (procFreeVars p) S.\\ (S.fromList $ map fst as)
      varCapt = coRecCapturedVars p
      ambigNames = ambigCoRecDef p
   in if S.size dupArgs    /= 0 ||
         S.size free       /= 0 ||
         M.size varCapt    /= 0 ||
         S.size ambigNames /= 0
         then Just $ ProcDefErr
                      { pErrDupArgs = dupArgs
                      , pErrFreeVars = free
                      , pErrCoRecVarCapture = M.map (S.\\ free) varCapt
                      , pErrAmbiguousRecName = ambigNames
                      }
         else Nothing

