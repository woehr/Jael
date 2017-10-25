{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
--{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Jael.New.Core
where

--import qualified Control.Comonad.Trans.Cofree as C
--import qualified Data.Bimap as M
--import qualified Data.Text as T

--import Jael.New.Expr
--import Jael.New.Type
--import Jael.New.Parser

data PrimOp = PrimRec
            deriving (Eq, Show)

data CorePattern = CorePattern
  deriving (Eq, Show)

data CoreF p t s c = CTAbsF s c
                   | CTAppF c t
                   | CAbsF  s t c
                   | CAppF  c c
                   | CLetF  s t c c
                   | CCaseF c [(CorePattern, c)]
                   | CVarF  s
                   | CIntF  Integer
                   | CPrimF PrimOp
                   | CImpossibleF
                   deriving (Eq, Foldable, Functor, Show, Traversable)

$(deriveEq1   ''CoreF)
$(deriveShow1 ''CoreF)
