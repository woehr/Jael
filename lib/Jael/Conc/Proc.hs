{-# Language NoImplicitPrelude, TypeFamilies #-}

module Jael.Conc.Proc where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import Jael.Seq.AST

newtype Chan = Chan Text
  deriving (Show)

type Var  = Text

data Proc = PGet Chan Var Proc
          | PPutC Chan Chan Proc
          | PPutV Chan Ex Proc
          | PNu Text Proc
          | PRepl Chan Var Proc
          | PPar Proc Proc
          | PCase (M.Map Text Proc)
          | PSel Text Proc
          | PCoRec Text [Var] Proc
          | PFwd Chan Chan
          | PNil
          deriving (Show)

data ProcF a = PGetF Chan Var a
             | PPutCF Chan Chan a
             | PPutVF Chan Ex a
             | PNuF Text a
             | PReplF Chan Var a
             | PParF a a
             | PCaseF (M.Map Text a)
             | PSelF Text a
             | PCoRecF Text [Var] a
             | PFwdF Chan Chan
             | PNilF
             deriving (Functor, Show)

type instance Base Proc = ProcF

instance Foldable Proc where
  project (PGet x y z) = PGetF x y z
  project (PPutC x y z) = PPutCF x y z
  project (PPutV x y z) = PPutVF x y z
  project (PNu x y) = PNuF x y
  project (PRepl x y z) = PReplF x y z
  project (PPar x y) = PParF x y
  project (PCase x) = PCaseF x
  project (PSel x y) = PSelF x y
  project (PCoRec x y z) = PCoRecF x y z
  project (PFwd x y) = PFwdF x y
  project (PNil) = PNilF

instance Unfoldable Proc where
  embed (PGetF x y z) = PGet x y z
  embed (PPutCF x y z) = PPutC x y z
  embed (PPutVF x y z) = PPutV x y z
  embed (PNuF x y) = PNu x y
  embed (PReplF x y z) = PRepl x y z
  embed (PParF x y) = PPar x y
  embed (PCaseF x) = PCase x
  embed (PSelF x y) = PSel x y
  embed (PCoRecF x y z) = PCoRec x y z
  embed (PFwdF x y) = PFwd x y
  embed (PNilF) = PNil

