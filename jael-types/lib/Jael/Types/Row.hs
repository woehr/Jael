{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language TemplateHaskell #-}

module Jael.Types.Row where

import           Control.DeepSeq                          ( NFData )
import           Data.Data                                ( Data
                                                          , Typeable
                                                          )
import           GHC.Generics                             ( Generic )
import           Data.Eq.Deriving                         ( deriveEq1 )
import           Text.Show.Deriving                       ( deriveShow1 )

import qualified Data.Map                      as M
import qualified Data.Text                     as T

data RowTail v = TailEmpty | TailVar v
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

data Row v x = Row (M.Map T.Text [x]) (RowTail v)
  deriving (Data, Eq, Functor, Generic, NFData, Show, Typeable)

deriveEq1 ''Row
deriveShow1 ''Row
