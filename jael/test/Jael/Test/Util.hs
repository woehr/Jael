{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Jael.Test.Util where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Text as T

import Text.Trifecta
import Text.Trifecta.Delta

import Jael.New.Expr
import Jael.New.DataDecl
import Jael.New.Misc
import Jael.New.Parser
import Jael.New.QType
import Jael.New.Type

$(deriveEq1   ''C.CofreeF)
$(deriveShow1 ''C.CofreeF)

parseThrow :: Parser a -> String -> a
parseThrow p t = case parseString (p <* eof) (Directed "test" 0 0 0 0) t of
                   Failure e -> error . show . _errDoc $ e
                   Success x -> x

unSpanQType :: T -> QType (Expr () P)
unSpanQType = cata alg . removeAnn where
  alg :: C.CofreeF TypeF (Refinement E) (QType (Expr () P))
      -> QType (Expr () P)
  alg (r C.:< t) = Fix $ fmap (fmap removeAnn) r C.:< t

parseQType :: String -> QType (Expr () P)
parseQType =  unSpanQType . parseThrow pType0

parseType :: String -> Type
parseType = unQType . parseQType where

parseData :: String -> (T.Text, DataDecl (QType (Expr () P)))
parseData = fmap (fmap unSpanQType) . parseThrow pData where

parseExpr :: String -> Expr () Pattern
parseExpr = exprMap id removeAnn . removeAnn . parseThrow pExpr0

parsePattern :: String -> P
parsePattern = parseThrow pPattern0
