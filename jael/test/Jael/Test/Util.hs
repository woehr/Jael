{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}

module Jael.Test.Util where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Text as T

import Text.Trifecta
import Text.Trifecta.Delta

import Jael.New.Expr
import Jael.New.DataDecl
import Jael.New.Parser
import Jael.New.QType
import Jael.New.Type

parseThrow :: Parser a -> String -> a
parseThrow p t = case parseString (p <* eof) (Directed "test" 0 0 0 0) t of
                   Failure e -> error . show . _errDoc $ e
                   Success x -> x

unSpanQType :: T -> QType T.Text (Expr () P T.Text)
unSpanQType = cata alg . removeAnn where
  alg :: C.CofreeF (TypeF v) (Refinement E) (QType v (Expr () P T.Text))
      -> QType v (Expr () P T.Text)
  alg (r C.:< t) = Fix $ fmap (fmap removeAnn) r C.:< t

parseQType :: String -> QType T.Text (Expr () P T.Text)
parseQType =  unSpanQType . parseThrow pType0

parseType :: String -> Type
parseType = (hoistFix unQType) . parseQType

parseData :: String -> DataDecl (QType T.Text (Expr () P T.Text))
parseData = fmap unSpanQType . parseThrow pData

parseExpr' :: String -> E
parseExpr' = parseThrow pExpr0

parseExpr :: String -> Expr () Pattern T.Text
parseExpr = hoistFix (mapExprP removeAnn) . removeAnn . parseExpr'

parsePattern :: String -> P
parsePattern = parseThrow pPattern0
