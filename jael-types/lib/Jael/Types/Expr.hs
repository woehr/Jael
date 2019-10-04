{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language PatternSynonyms #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module Jael.Types.Expr where

import           Control.DeepSeq                          ( NFData )
import           Data.Data                                ( Data
                                                          , Typeable
                                                          )
import           Data.UniformPair                         ( Pair(..) )
import           GHC.Generics                             ( Generic )
import           Data.Eq.Deriving                         ( deriveEq1 )
import           Text.Show.Deriving                       ( deriveShow1 )

import           Data.OpenADT
import           Data.Row                          hiding ( Row )

import qualified Data.Text                     as T

import           Jael.Types.Row

type RecLabel = T.Text

data IntFormat = BinInt | OctInt | HexInt | DecInt
                 deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JInt = JInt
  { intFormat    :: IntFormat
  , intValue     :: Integer
  , intNumDigits :: Integer
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

data Primitive x
  = PrimOpNot   x
  | PrimOpIff   x x
  | PrimOpImp   x x
  | PrimOpOr    x x
  | PrimOpAnd   x x
  | PrimOpEq    x x
  | PrimOpNe    x x
  | PrimOpGt    x x
  | PrimOpGe    x x
  | PrimOpLt    x x
  | PrimOpLe    x x
  | PrimOpAdd   x x
  | PrimOpSub   x x
  | PrimOpTimes x x
  | PrimOpDiv   x x
  | PrimOpMod   x x
  | PrimArrEmpty
  | PrimArrSet   Integer x
  | PrimArrUnset Integer x
  | PrimArrAt    Integer
  | PrimRecExtendF' RecLabel x x
  | PrimRecRenameF' RecLabel RecLabel x
  | PrimRecRemoveF' x RecLabel
  | PrimRecSelectF' x RecLabel
  | PrimImpossible x
  | PrimUnimplemented
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

deriveEq1   ''Primitive
deriveShow1 ''Primitive

-- Core syntax

data ETAbsF v x = ETAbsF' v x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data ETAppF t x = ETAppF' t x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data EAbsF p x = EAbsF' p x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data EAppF x = EAppF' x [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data ELetF p x = ELetF' [(p, x)] x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data EIfF x = EIfF' x x x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype EVarF v x = EVarF' v
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype EIntF x = EIntF' Integer
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype EPrimF x = EPrimF' (Primitive x)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

-- Typed Core

data EAbsTF t p x = EAbsTF' [(p, t)] x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data ELetTF t p x = ELetTF' [((p, t), x)] x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

-- Built-in Products

newtype ETupF x = ETupF'   [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype EArrF x = EArrF'   [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype ERecF v x = ERecF' (Row v x)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

-- Sugar

newtype ELamCaseF p x = ELamCaseF' [(p, x)]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data ECaseF p x = ECaseF' x [(p, x)]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data EMultiIfF x = EMultiIfF' [Pair x] (Maybe x)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

-- Typed Sugar

data ELamCaseTF t p x = ELamCaseTF' t [(p, x)]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

deriveEq1 ''ETAbsF
deriveEq1 ''ETAppF
deriveEq1 ''EAbsF
deriveEq1 ''EAppF
deriveEq1 ''ELetF
deriveEq1 ''EIfF
deriveEq1 ''EVarF
deriveEq1 ''EIntF
deriveEq1 ''EPrimF
deriveEq1 ''EAbsTF
deriveEq1 ''ELetTF
deriveEq1 ''ETupF
deriveEq1 ''EArrF
deriveEq1 ''ERecF
deriveEq1 ''ELamCaseF
deriveEq1 ''ECaseF
deriveEq1 ''EMultiIfF
deriveEq1 ''ELamCaseTF

deriveShow1 ''ETAbsF
deriveShow1 ''ETAppF
deriveShow1 ''EAbsF
deriveShow1 ''EAppF
deriveShow1 ''ELetF
deriveShow1 ''EIfF
deriveShow1 ''EVarF
deriveShow1 ''EIntF
deriveShow1 ''EPrimF
deriveShow1 ''EAbsTF
deriveShow1 ''ELetTF
deriveShow1 ''ETupF
deriveShow1 ''EArrF
deriveShow1 ''ERecF
deriveShow1 ''ELamCaseF
deriveShow1 ''ECaseF
deriveShow1 ''EMultiIfF
deriveShow1 ''ELamCaseTF

mkVarPattern ''ETAbsF      "eTAbsF"      "ETAbs"      "ETAbsF"
mkVarPattern ''ETAppF      "eTAppF"      "ETApp"      "ETAppF"
mkVarPattern ''EAbsF       "eAbsF"       "EAbs"       "EAbsF"
mkVarPattern ''EAppF       "eAppF"       "EApp"       "EAppF"
mkVarPattern ''ELetF       "eLetF"       "ELet"       "ELetF"
mkVarPattern ''EIfF        "eIfF"        "EIf"        "EIfF"
mkVarPattern ''EVarF       "eVarF"       "EVar"       "EVarF"
mkVarPattern ''EIntF       "eIntF"       "EInt"       "EIntF"
mkVarPattern ''EPrimF      "ePrimF"      "EPrim"      "EPrimF"
mkVarPattern ''EAbsTF      "eAbsTF"      "EAbsT"      "EAbsTF"
mkVarPattern ''ELetTF      "eLetTF"      "ELetT"      "ELetTF"
mkVarPattern ''ETupF       "eTupF"       "ETup"       "ETupF"
mkVarPattern ''EArrF       "eArrF"       "EArr"       "EArrF"
mkVarPattern ''ERecF       "eRecF"       "ERec"       "ERecF"
mkVarPattern ''ELamCaseF   "eLamCaseF"   "ELamCase"   "ELamCaseF"
mkVarPattern ''ECaseF      "eCaseF"      "ECase"      "ECaseF"
mkVarPattern ''EMultiIfF   "eMultiIfF"   "EMultiIf"   "EMultiIfF"
mkVarPattern ''ELamCaseTF  "eLamCaseTF"  "ELamCaseT"  "ELamCaseTF"
