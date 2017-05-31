{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language QuasiQuotes #-}

module Jael.Constants
( constantType
, constantScheme
) where

import Prelude hiding (div, mod)
import Jael.Types
import Jael.Quotes
import Jael.Util

import qualified Language.Fixpoint.Types as F

constantType :: Constant -> Type
constantType c = let (Scheme _ _ t) = constantScheme c in removeAnn t

constantScheme :: Constant -> QScheme
constantScheme CUnit     = unit
constantScheme (CBool b) = boolConst b
constantScheme (CInt  i) = intConst (value i)
constantScheme CAdd = add
constantScheme CSub = sub
constantScheme CMul = mul
constantScheme CDiv = div
constantScheme CMod = mod
constantScheme COr  = lor
constantScheme CAnd = land
constantScheme CEq  = eq
constantScheme CNe  = ne
constantScheme CGe  = ge
constantScheme CLe  = le
constantScheme CGt  = gt
constantScheme CLt  = lt
constantScheme CIff = iff
constantScheme CImp = imp
constantScheme CBitCat = cat
constantScheme CNot = lnot

unit :: QScheme
unit = [rtype| {v:Void | v == void } |]

intConst :: Integer -> QScheme
intConst x =
  let _fpInt = F.expr x
  in  [rtype| { v:Int | v == h__fpInt } |]

boolConst :: Bool -> QScheme
boolConst b =
  let _fpBool = if b then F.PTrue else F.PFalse
  in  [rtype| { v:Bool | v <-> h__fpBool } |]

iff, imp, add, sub, mul, div, mod, lor, land, eq, ne, ge, le, gt, lt, cat, lnot :: QScheme

iff  = [rtype| x:Bool -> y:Bool -> {v:Bool | v <-> (x <-> y)} |]

imp  = [rtype| x:Bool -> y:Bool -> {v:Bool | v <-> x --> y} |]

add  = [rtype| x:Int -> y:Int -> {v:Int | v == x + y} |]

sub  = [rtype| x:Int -> y:Int -> {v:Int | v == x - y }|]

mul  = undefined -- [rtype| |]

div  = undefined -- [rtype| |]

mod  = undefined -- [rtype| |]

lor  = [rtype| x:Bool -> y:Bool -> {v:Bool | v <-> x || y}|]

land = [rtype| x:Bool -> y:Bool -> {v:Bool | v <-> x && y} |]

eq   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x == y} |]

ne   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x != y} |]

ge   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x >= y} |]

le   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x <= y} |]

gt   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x > y} |]

lt   = [rtype| x:Int -> y:Int -> {v:Bool | v <-> x < y} |]

cat  = [rtype| x:Bits -> y:Bits -> {v:Bits | size(v) == size(x) + size(y)} |]

lnot = [rtype| x:Bool -> {v:Bool | v <-> !x} |]
