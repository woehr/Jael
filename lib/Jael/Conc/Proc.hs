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
import Jael.Util

data PNewType = PNTNamed Text
              | PNTSession Session
              -- Not a type but is how we'll introducing seq values into the
              -- process tree for now
              | PNTExpr Ex
                deriving (Eq, Show)

data ProcDefErr = ProcDefErr
  { pErrFreeVars :: S.Set Text
  , pErrDupArgs :: S.Set Text
  , pErrNonExplicitCoRecVarCapture :: M.Map Text (S.Set Text)
  } deriving (Eq, Show)

type Chan = Text
type Var  = Text
type Label = Text

data TyOrSess = TorSTy Ty
              | TorSSess Session
              deriving (Show)

data TopProc = TopProc [(Text, TyOrSess)] Proc
  deriving (Show)

data Proc = PGet Chan Var Proc
          | PPut Chan Ex Proc
          | PNew Text PNewType Proc
          | PRepl Chan Var Proc
          | PPar [Proc]
          | PCase Chan [(Label, Proc)]
          | PSel Chan Label Proc
          | PCoRec Text [(Var, Ex)] Proc
          | PFwd Chan Chan
          | PNamed Text [Ex]
          | PNil
          deriving (Eq, Show)

data ProcF a = PGetF Chan Var a
             | PPutF Chan Ex a
             | PNewF Text PNewType a
             | PReplF Chan Var a
             | PParF [a]
             | PCaseF Chan [(Label, a)]
             | PSelF Chan Label a
             | PCoRecF Text [(Var, Ex)] a
             | PFwdF Chan Chan
             | PNamedF Text [Ex]
             | PNilF
             deriving (Functor, Show)

type instance Base Proc = ProcF

instance Foldable Proc where
  project (PGet x y z)   = PGetF x y z
  project (PPut x y z)   = PPutF x y z
  project (PNew x y z)   = PNewF x y z
  project (PRepl x y z)  = PReplF x y z
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
  embed (PNewF x y z)   = PNew x y z
  embed (PReplF x y z)  = PRepl x y z
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

gToInitList :: [GRecInitializer] -> [(Text, Ex)]
gToInitList = map (\(GRecInitializer (LIdent x) ex) -> (pack x, gToEx ex))

gParElemToProc :: GParElem -> GProc
gParElemToProc (GParElem p) = p

gProcParamToEx :: GProcParam -> Ex
gProcParamToEx (GProcParam x) = gToEx x

gToProc :: GProc -> Proc
gToProc = ana coalg
  where coalg :: GProc -> Base Proc GProc
        coalg (GProcNew (GUserChan (LIdent x)) (GSessOrIdentIdent (UIdent y)) p
              ) = PNewF (pack x) (PNTNamed $ pack y) p
        coalg (GProcNew (GUserChan (LIdent x)) (GSessOrIdentSess y) p
              ) = PNewF (pack x) (PNTSession $ gToSession y) p
        coalg (GProcLet (LIdent x) y p
              ) = PNewF (pack x) (PNTExpr $ gToEx y) p
        coalg (GProcGet (GChan (GScopedIdent xs)) (LIdent y) p
              ) = PGetF (gScopedToText xs) (pack y) p
        coalg (GProcPut (GChan (GScopedIdent xs)) ex p
              ) = PPutF (gScopedToText xs) (gToEx ex) p
        coalg (GProcSel (GChan (GScopedIdent xs)) (GChoiceLabel (LIdent y)) p
              ) = PSelF (gScopedToText xs) (pack y) p
        coalg (GProcCho (GChan (GScopedIdent xs)) ys
              ) = PCaseF (gScopedToText xs) (gChoiceToProc ys)
        coalg (GProcRec (GProcName (UIdent x)) inits p
              ) = PCoRecF (pack x) (gToInitList inits) p
        coalg (GProcNamed (GProcName (UIdent x)) params
              ) = PNamedF (pack x) (map gProcParamToEx params)
        coalg (GProcInact GUnit
              ) = PNilF
        coalg (GProcPar e1 es
              ) = PParF (gParElemToProc e1 : map gParElemToProc es)

procDeps :: TopProc -> S.Set Text
procDeps (TopProc _ p) = cata alg p
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PNamedF n _) = S.singleton n
        alg (PCoRecF n _ x) = n `S.delete` x
        alg (PNewF _ _ x) = x
        alg (PGetF _ _ x) = x
        alg (PPutF _ _ x) = x
        alg (PSelF _ _ x) = x
        alg (PCaseF _ xs) = S.unions (map snd xs)
        alg (PParF xs) = S.unions xs
        alg (PReplF _ _ x) = x
        alg _ = S.empty

procFreeVars :: Proc -> S.Set Text
procFreeVars = cata alg
  where alg :: Base Proc (S.Set Text) -> S.Set Text
        alg (PNamedF _ as) = S.unions $ map freeVars as
        alg (PCoRecF _ as p) = S.unions (map (freeVars . snd) as)
                              `S.union` (foldr S.delete p (map fst as))
        alg (PNewF v (PNTNamed _) p) = v `S.delete` p
        alg (PNewF v (PNTSession _) p) = v `S.delete` p
        alg (PNewF v (PNTExpr e) p) = v `S.delete` (p `S.union` freeVars e)
        alg (PGetF c v p) = c `S.insert` (v `S.insert` p)
        alg (PPutF c e p) = c `S.insert` (freeVars e `S.union` p)
        alg (PSelF c _ p) = c `S.insert` p
        alg (PCaseF c xs) = c `S.insert` S.unions (map snd xs)
        alg (PParF xs) = S.unions xs
        alg (PReplF c v p) = c `S.insert` (v `S.insert` p)
        alg _ = S.empty

coRecCapturedVars :: Proc -> M.Map Text (S.Set Text)
coRecCapturedVars = para alg
  where alg :: Base Proc (Proc, (M.Map Text (S.Set Text)))
            -> M.Map Text (S.Set Text)
        alg (PCoRecF n as (p, m)) =
          M.insert n (foldr S.delete (procFreeVars p) (map fst as)) m
        alg (PParF xs) = M.unions (map snd xs)
        alg (PCaseF _ xs) = M.unions (map (snd . snd) xs)
        alg (PNewF _ _ (_, x)) = x
        alg (PGetF _ _ (_, x)) = x
        alg (PPutF _ _ (_, x)) = x
        alg (PSelF _ _ (_, x)) = x
        alg (PReplF _ _ (_, x)) = x
        alg _ = M.empty

gProcArgToList :: GProcArg -> (Text, TyOrSess)
gProcArgToList (GProcArg (LIdent i) (GSessTy x)) =
  (pack i, TorSTy (gToType x))
gProcArgToList (GProcArg (LIdent i) (GSessSess x)) =
  (pack i, TorSSess (gToSession x))

gToTopProc :: [GProcArg] -> GProc -> TopProc
gToTopProc as p = TopProc (map gProcArgToList as) (gToProc p)

validateTopProc :: TopProc -> Maybe ProcDefErr
validateTopProc (TopProc as p) =
  let dupArgs = S.fromList $ repeated (map fst as)
      free = foldr S.delete (procFreeVars p) (map fst as)
      necrvc = coRecCapturedVars p
   in if S.size dupArgs /= 0 ||
         S.size free    /= 0 ||
         M.size necrvc  /= 0
         then Just $ ProcDefErr
                      { pErrDupArgs = dupArgs
                      , pErrFreeVars = free
                      , pErrNonExplicitCoRecVarCapture = necrvc
                      }
         else Nothing

