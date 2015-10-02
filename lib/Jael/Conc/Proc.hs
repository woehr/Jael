{-# Language NoImplicitPrelude #-}
{-# Language TypeFamilies #-}

module Jael.Conc.Proc where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import Jael.Grammar
import Jael.Seq.AST
import Jael.Seq.Expr
import Jael.Conc.Session

data PNewType = PNTNamed Text
              | PNTSession Session
              -- Not a type but is how we'll introducing seq values into the
              -- process tree for now
              | PNTExpr Ex
                deriving (Eq, Show)

newtype Chan = Chan Text
  deriving (Eq, Show)

type Var  = Text
type Label = Text

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

scopedToText :: [GScopeElem] -> Text
scopedToText = intercalate "::" . map (\(GScopeElem (LIdent x)) -> pack x)

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
              ) = PGetF (Chan $ scopedToText xs) (pack y) p
        coalg (GProcPut (GChan (GScopedIdent xs)) ex p
              ) = PPutF (Chan $ scopedToText xs) (gToEx ex) p
        coalg (GProcSel (GChan (GScopedIdent xs)) (GChoiceLabel (LIdent y)) p
              ) = PSelF (Chan $ scopedToText xs) (pack y) p
        coalg (GProcCho (GChan (GScopedIdent xs)) ys
              ) = PCaseF (Chan $ scopedToText xs) (gChoiceToProc ys)
        coalg (GProcRec (GProcName (UIdent x)) inits p
              ) = PCoRecF (pack x) (gToInitList inits) p
        coalg (GProcNamed (GProcName (UIdent x)) params
              ) = PNamedF (pack x) (map gProcParamToEx params)
        coalg (GProcInact GUnit
              ) = PNilF
        coalg (GProcPar e1 es
              ) = PParF (gParElemToProc e1 : map gParElemToProc es)

