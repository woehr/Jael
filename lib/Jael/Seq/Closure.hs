{-# Language NoImplicitPrelude #-}

module Jael.Seq.Closure where

import ClassyPrelude
import Jael.Seq.AST

data CCFun = CCFun [Text] ExCC
             deriving (Eq, Show)

data ExCC = ECVar Text
          | ECUnit
          | ECInt Integer
          | ECBool Bool
          | ECApp Text [ExCC]
          | ECClos Text ExCC
            deriving (Eq, Show)

closureConversion :: TypedEx -> (ExCC, [(Text, CCFun)])
closureConversion = undefined

