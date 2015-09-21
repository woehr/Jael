{-# Language NoImplicitPrelude, TypeFamilies #-}

module Jael.Conc.Session where

import ClassyPrelude hiding (Chan, Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import Jael.Seq.Types

data Session = SRx Ty Session
             | STx Ty Session
             | SRxChan Session Session
             | STxChan Session Session
             | SChoice (M.Map Text Session)
             | SSelect (M.Map Text Session)
             | SRepl Session
             | SCoInd Text Session
             | SEnd
             deriving (Show)

data SessionF a = SRxF Ty a
                | STxF Ty a
                | SRxChanF a a
                | STxChanF a a
                | SChoiceF (M.Map Text a)
                | SSelectF (M.Map Text a)
                | SReplF a
                | SCoIndF Text a
                | SEndF
               deriving (Functor, Show)

type instance Base Session = SessionF

instance Foldable Session where
  project (SRx x y) = SRxF x y
  project (STx x y) = STxF x y
  project (SRxChan x y) = SRxChanF x y
  project (STxChan x y) = STxChanF x y
  project (SChoice x) = SChoiceF x
  project (SSelect x) = SSelectF x
  project (SRepl x) = SReplF x
  project (SCoInd x y) = SCoIndF x y
  project (SEnd) = SEndF

instance Unfoldable Session where
  embed (SRxF x y) = SRx x y
  embed (STxF x y) = STx x y
  embed (SRxChanF x y) = SRxChan x y
  embed (STxChanF x y) = STxChan x y
  embed (SChoiceF x) = SChoice x
  embed (SSelectF x) = SSelect x
  embed (SReplF x) = SRepl x
  embed (SCoIndF x y) = SCoInd x y
  embed (SEndF) = SEnd

