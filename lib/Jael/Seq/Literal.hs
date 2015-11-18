module Jael.Seq.Literal where

import Jael.Seq.HM_Types

data BitValue = BVSet
              | BVNotSet
              deriving (Eq, Show)

data Literal = LUnit
             | LInt Integer
             | LBool Bool
             | LBit [BitValue] -- MSB at front of list
             deriving (Eq, Show)

instance SeqTypable Literal where
  tyOf (LUnit) = TUnit
  tyOf (LInt _) = TInt
  tyOf (LBool _) = TBool
  tyOf (LBit _) = TBit

