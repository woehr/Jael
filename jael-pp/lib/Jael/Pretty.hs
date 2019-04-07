{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Jael.Pretty
  ( module Jael.Pretty
  , module X
  )
where

import           Jael.Pretty.TreeDiff          as X

import           Data.Functor.Foldable                    ( cata )
import qualified Data.Map                      as M
import           Data.Row.Variants                        ( Forall )
import           Data.Text.Prettyprint.Doc
import           Data.OpenADT

import           Jael.Types
import           Jael.Pretty.Helpers

--------------------- Recursion scheme based pretty class ----------------------

class Pretty' (f :: * -> *) where
  pretty' :: f (Doc ann) -> Doc ann

instance (Pretty' (VarF r), Forall r Functor) => Pretty (OpenADT r) where
  pretty = cata pretty'

instance (Forall r Pretty') => Pretty' (VarF r) where
  pretty' = varFAlg @Pretty' pretty'

----------------------------- Expr (and related) ------------------------------

--instance Pretty JInt where
--  pretty JInt{..} = case intFormat of
--    BinInt -> let s = showIntAtBase 2 intToDigit intValue ""
--      in pretty $ "0b" <> replicate (fromInteger $ intLength - length s) '0' <> s
--    OctInt -> let s = showOct intValue "" in pretty $ "0o" <> replicate (fromInteger $ intLength - length s) '0' <> s
--    HexInt -> let s = showHex intValue "" in pretty $ "0x" <> replicate (fromInteger $ intLength - length s) '0' <> s
--    DecInt -> if intValue >= 0
--      then pretty intValue
--      else "~" <> pretty (-intValue)

---------------------------------- Patterns -----------------------------------

instance Pretty' POrF where
  pretty' (POrF' xs) = encloseSep lparen rparen "||" xs

instance (Pretty b) => Pretty' (PConF b) where
  pretty' (PConF' b xs) = if null xs
    then pretty b
    else ppApp b xs

instance (Pretty v) => Pretty' (PVarF v) where
  pretty' (PVarF' v) = vc <> pretty v

instance (Pretty v) => Pretty' (PAtF v) where
  pretty' (PAtF' v x) = vc <> pretty v <> "@" <+> ppParens [x]

instance (Pretty l) => Pretty' (PLitF l) where
  pretty' (PLitF' l) = pretty l

instance Pretty' PWildF where
  pretty' _ = "_"

instance Pretty' PTupF where
  pretty' (PTupF' xs) = ppParens xs

instance Pretty' PArrF where
  pretty' (PArrF' xs) = ppBrackets xs

instance (Pretty v) => Pretty' (PRecF v) where
  pretty' (PRecF' (Row (M.toList -> []) TailEmpty)) = "{}"
  pretty' (PRecF' (Row (M.toList -> []) (TailVar t))) = braces (pretty t)
  pretty' (PRecF' (Row (M.toList -> rs) (TailVar v))) = ppBracesPipe (fmap ppRow [ (x,y) | (x,b) <- rs, y <- b ]) (pretty v)
  pretty' (PRecF' (Row (M.toList -> rs) TailEmpty)) = ppRec (fmap ppRow [ (x,y) | (x,b) <- rs, y <- b ])
