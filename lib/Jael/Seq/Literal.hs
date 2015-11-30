module Jael.Seq.Literal where

import Jael.Seq.Types

data BitValue = BVSet
              | BVNotSet
              deriving (Eq, Show)

data Literal = LUnit
             | LInt Integer
             | LBool Bool
             | LBit [BitValue] -- MSB at front of list
             deriving (Eq, Show)

instance HMTypable Literal where
  hmTyOf (LUnit)   = HMTyUnit
  hmTyOf (LInt _)  = HMTyInt
  hmTyOf (LBool _) = HMTyBool
  hmTyOf (LBit _)  = HMTyBit

