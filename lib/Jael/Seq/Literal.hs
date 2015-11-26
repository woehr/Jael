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
  tyOf (LUnit)   = TySimple TyUnit
  tyOf (LInt _)  = TySimple TyInt
  tyOf (LBool _) = TySimple TyBool
  tyOf (LBit _)  = TySimple TyBit

