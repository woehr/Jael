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

module Jael.Types.Type where

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

data TAllF v x = TAllF' v x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data TFunF x = TFunF' (Maybe T.Text) x x
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data TConF x = TConF' T.Text [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype TTupF x = TTupF' [x]
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data TArrF x = TArrF' x Integer
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype TVarF v x = TVarF' v
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

newtype TRecF v x = TRecF' (Row v x)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

deriveEq1 ''TAllF
deriveEq1 ''TFunF
deriveEq1 ''TConF
deriveEq1 ''TTupF
deriveEq1 ''TArrF
deriveEq1 ''TVarF
deriveEq1 ''TRecF

deriveShow1 ''TAllF
deriveShow1 ''TFunF
deriveShow1 ''TConF
deriveShow1 ''TTupF
deriveShow1 ''TArrF
deriveShow1 ''TVarF
deriveShow1 ''TRecF

mkVarPattern ''TAllF  "tAllF" "TAll" "TAllF"
mkVarPattern ''TFunF  "tFunF" "TFun" "TFunF"
mkVarPattern ''TConF  "tConF" "TCon" "TConF"
mkVarPattern ''TTupF  "tTupF" "TTup" "TTupF"
mkVarPattern ''TArrF  "tArrF" "TArr" "TArrF"
mkVarPattern ''TVarF  "tVarF" "TVar" "TVarF"
mkVarPattern ''TRecF  "tRecF" "TRec" "TRecF"
