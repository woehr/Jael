{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language QuasiQuotes #-}

module Jael.Constants
where

import Jael.Types
import Jael.Quotes

--import qualified Language.Fixpoint.Types as F

--var :: T.Text -> F.Symbol
--var = F.symbol

--intConst :: Integer -> QScheme
--intConst x = Scheme [] $ "v" .: TIntF .| var "v" .= x

add :: QType
add = [qtype| x:Int -> y:Int -> {vv:Int | vv == x + y} |]
