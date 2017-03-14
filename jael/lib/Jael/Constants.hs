{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language QuasiQuotes #-}

module Jael.Constants
where

import Jael.Types
import Jael.Quotes

--intConst :: Integer -> QScheme
--intConst x = Scheme [] $ "v" .: TIntF .| var "v" .= x

add :: QType
add = [qtype| x:Int -> y:Int -> {vv:Int | vv == x + y} |]
