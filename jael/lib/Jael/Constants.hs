{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}

module Jael.Constants where

import           Jael.AST
import           Jael.Expr
import           Jael.Prelude                      hiding ( div
                                                          , mod
                                                          )
import           Jael.QType
import           Jael.Quotes
import           Jael.Type

intConst :: Integer -> ParseType
intConst x = let _fpInt = show x in [rtype| (| v:Int | v == `_fpInt` |) |]

boolConst :: Bool -> ParseType
boolConst b =
  let _fpBool = if b then trueConst else falseConst
  in  [rtype| (| v:Bool | v <-> `_fpBool` |) |]

iff, imp, add, sub, mul, div, mod, lor :: ParseType
land, eq, ne, ge, le, gt, lt, lnot :: ParseType
imposs, unimpl :: ParseType

iff = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> (x <-> y)|) |]
imp = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x --> y|) |]
add = [rtype| x:Int -> y:Int -> (|v:Int | v == x + y|) |]
sub = [rtype| x:Int -> y:Int -> (|v:Int | v == x - y |)|]
mul = undefined -- [rtype| |]
div = undefined -- [rtype| |]
mod
  = [rtype| (| v:Int | v >= 0 |) -> y:(| v:Int | v > 0 |) -> (| v:Int | 0 <= v && v < y |) |]
lor = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x || y|)|]
land = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x && y|) |]
eq = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x == y|) |]
ne = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x != y|) |]
ge = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x >= y|) |]
le = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x <= y|) |]
gt = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x > y|) |]
lt = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x < y|) |]
lnot = [rtype| x:Bool -> (|v:Bool | v <-> !x|) |]
imposs = [rtype| forall a. (| v:a | false |) -> a |]
unimpl = [rtype| forall a. a |]

arrEmpty, arrSet, arrUnset, arrAt :: ParseType
arrEmpty = [rtype| (| v:[a;1] | true |) |]
arrSet
  = [rtype| i:(| v:Int | v >= 0 |) -> a -> (| v:[a;1] | true |) -> (| v:[a;1] | true |) |]
arrUnset
  = [rtype| i:(| v:Int | v >= 0 |) -> (| v:[a;1] | true |) -> (| v:[a;1] | true |) |]
arrAt =
  [rtype| i:(| v:Int | v >= 0 |) -> (| v:[a;1] | true |) -> a |]

litQType :: Literal -> ParseType
litQType = \case
  -- The integer value doesn't matter if we get rid of the refinement
  LInt _ -> intConst 0

opQType :: Operator -> ParseType
opQType = \case
  OpNot   -> lnot
  OpIff   -> iff
  OpImp   -> imp
  OpOr    -> lor
  OpAnd   -> land
  OpEq    -> eq
  OpNe    -> ne
  OpGt    -> gt
  OpGe    -> ge
  OpLt    -> lt
  OpLe    -> le
  OpAdd   -> add
  OpSub   -> sub
  OpTimes -> mul
  OpDiv   -> div
  OpMod   -> mod

primQType :: Primitive -> ParseType
primQType = \case
  PrimArrEmpty -> arrEmpty
  PrimArrSet   -> arrSet
  PrimArrUnset -> arrUnset
  PrimArrAt    -> arrAt
  PrimImposs   -> imposs
  PrimUnimpl   -> unimpl

litType :: Literal -> Type'
litType = unrefined . litQType

opType :: Operator -> Type'
opType = unrefined . opQType

primType :: Primitive -> Type'
primType = unrefined . primQType
