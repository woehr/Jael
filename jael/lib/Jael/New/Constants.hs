{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Jael.New.Constants where

import Prelude hiding (div, mod)
import Jael.New.Expr
import Jael.New.Parser
import Jael.New.QType
import Jael.New.Quotes
import Jael.New.Type

import qualified Data.Text as T
import           Text.Trifecta
import           Text.Trifecta.Delta

unit :: QType T.Text (Expr () Pattern T.Text)
unit = [rtype| (| v:Void | v == void |) |]

intConst :: Integer -> QType T.Text (Expr () Pattern T.Text)
intConst x =
  let ji = case parseString anyint (Directed "" 0 0 0 0) (show x) of
             Success i -> i
             Failure e -> error $ "Should always work:\n" ++ show e
      _fpInt = EConst . CInt $ ji
  in  [rtype| (| v:Int | v == h__fpInt |) |]

boolConst :: Bool -> QType T.Text (Expr () Pattern T.Text)
boolConst b =
  let _fpBool = if b then ETrue else EFalse
  in  [rtype| (| v:Bool | v <-> h__fpBool |) |]

iff, imp, add, sub, mul, div, mod, lor :: QType T.Text (Expr () Pattern T.Text)
land, eq, ne, ge, le, gt, lt, cat, lnot :: QType T.Text (Expr () Pattern T.Text)

iff  = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> (x <-> y)|) |]
imp  = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x --> y|) |]
add  = [rtype| x:Int -> y:Int -> (|v:Int | v == x + y|) |]
sub  = [rtype| x:Int -> y:Int -> (|v:Int | v == x - y |)|]
mul  = undefined -- [rtype| |]
div  = undefined -- [rtype| |]
mod  = undefined -- [rtype| |]
lor  = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x || y|)|]
land = [rtype| x:Bool -> y:Bool -> (|v:Bool | v <-> x && y|) |]
eq   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x == y|) |]
ne   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x != y|) |]
ge   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x >= y|) |]
le   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x <= y|) |]
gt   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x > y|) |]
lt   = [rtype| x:Int -> y:Int -> (|v:Bool | v <-> x < y|) |]
cat  = [rtype| x:Bits -> y:Bits -> (|v:Bits | size(v) == size(x) + size(y)|) |]
lnot = [rtype| x:Bool -> (|v:Bool | v <-> !x|) |]

constQType :: Constant -> QType T.Text (Expr () Pattern T.Text)
constQType = \case
    -- The integer value doesn't matter if we get rid of the refinement
    CInt _  -> intConst 0
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

constType :: Constant -> Type
constType = hoistFix unQType . constQType
