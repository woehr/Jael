{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Jael.Quotes where

import Data.List.Split
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import Text.Regex.Applicative

import qualified Data.Set as S

import Jael.AST
import Jael.Prelude hiding (lift)

rtype :: QuasiQuoter
rtype = QuasiQuoter
  { quoteExp  = \s -> do
      let emptyString = return (LitE (StringL "")) :: Q Exp
      let appMappend x a = [| $x <> $a |]
      let subs = S.fromList (quotedVars s)
      let liftToVar x = if S.member x subs
            then return (VarE (mkName x))
            else lift x
      let stringExp = foldr appMappend emptyString (fmap liftToVar (splitOn "`" s))
      [| parseToQType $stringExp |]
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

quotedVar :: RE Char String
quotedVar = many anySym *> sym '`' *> some (psym (/= '`')) <* sym '`'

quotedVars :: String -> [String]
quotedVars s = go s []
  where go :: String -> [String] -> [String]
        go x xs = case findShortestPrefix quotedVar x of
          Just (m, r) -> go r (m:xs)
          Nothing     -> xs
