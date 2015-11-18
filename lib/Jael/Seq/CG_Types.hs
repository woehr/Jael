module Jael.Seq.CG_Types where

import Jael.Seq.HM_Types

data CGBasicType = CGUnit
                 | CGInt { cgIntMin :: Integer
                         , cgIntMax :: Integer
                         }
                 | CGBool
                 | CGBit { cgBoolSize :: Integer }
                 deriving (Eq, Show)

data CGTy = CGTySimple CGBasicType
          | CGTyStruct
          | CGTyEnumer
          | CGTyTup [CGTy]
          deriving (Eq, Show)

cgTypeToHmType :: CGTy -> Ty
cgTypeToHmType = undefined

