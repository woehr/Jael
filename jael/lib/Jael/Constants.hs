{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language QuasiQuotes #-}

module Jael.Constants
where

import Jael.Prelude
import Jael.Types
import Jael.Quotes

import qualified Language.Fixpoint.Types as F

unit :: QScheme
unit = [qtype| {vv:Void | vv == void } |]

intConst :: Integer -> QScheme
intConst x =
  let _fpInt = F.expr x
  in  [qtype| { v:Int | v == h__fpInt } |]

add :: QScheme
add = [qtype| x:Int -> y:Int -> {vv:Int | vv == x + y} |]
