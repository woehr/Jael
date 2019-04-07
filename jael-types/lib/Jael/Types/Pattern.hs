{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language PatternSynonyms #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module Jael.Types.Pattern where

import           Control.DeepSeq                          ( NFData )
import           Data.Data                                ( Data
                                                          , Typeable
                                                          )
import           GHC.Generics                             ( Generic )
import           Data.Eq.Deriving                         ( deriveEq1 )
import           Text.Show.Deriving                       ( deriveShow1 )

import           Data.OpenADT
import           Data.Row                          hiding ( Row )

import qualified Data.Text                     as T

import           Jael.Types.Row

newtype POrF x = POrF' [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data PConF s x = PConF' s [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype PVarF v x = PVarF' v
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data PAtF v x = PAtF' v x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype PLitF l x = PLitF' l
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data PWildF x = PWildF'
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype PTupF x = PTupF' [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype PArrF x = PArrF' [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype PRecF v x = PRecF' (Row v x)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

type PatternRowF v l = ( "pOrF" .== POrF
                      .+ "pConF" .== PConF T.Text
                      .+ "pVarF" .== PVarF v
                      .+ "pAtF" .== PAtF v
                      .+ "pLitF" .== PLitF l
                      .+ "pWildF" .== PWildF
                      .+ "pTupF" .== PTupF
                      .+ "pArrF" .== PArrF
                      .+ "pRecF" .== PRecF v
                       )

type PatternF v l = VarF    (PatternRowF v l)
type Pattern  v l = OpenADT (PatternRowF v l)

deriveEq1 ''POrF
deriveEq1 ''PConF
deriveEq1 ''PVarF
deriveEq1 ''PAtF
deriveEq1 ''PLitF
deriveEq1 ''PWildF
deriveEq1 ''PTupF
deriveEq1 ''PArrF
deriveEq1 ''PRecF

deriveShow1 ''POrF
deriveShow1 ''PConF
deriveShow1 ''PVarF
deriveShow1 ''PAtF
deriveShow1 ''PLitF
deriveShow1 ''PWildF
deriveShow1 ''PTupF
deriveShow1 ''PArrF
deriveShow1 ''PRecF

mkVarPattern ''POrF   "pOrF"   "POr"   "POrF"
mkVarPattern ''PConF  "pConF"  "PCon"  "PConF"
mkVarPattern ''PVarF  "pVarF"  "PVar"  "PVarF"
mkVarPattern ''PAtF   "pAtF"   "PAt"   "PAtF"
mkVarPattern ''PLitF  "pLitF"  "PLit"  "PLitF"
mkVarPattern ''PWildF "pWildF" "PWild" "PWildF"
mkVarPattern ''PTupF  "pTupF"  "PTup"  "PTupF"
mkVarPattern ''PArrF  "pArrF"  "PArr"  "PArrF"
mkVarPattern ''PRecF  "pRecF"  "PRec"  "PRecF"
