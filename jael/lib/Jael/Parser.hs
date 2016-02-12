module Jael.Parser where

import qualified Data.Text as T
import qualified Jael.Grammar as G

type ParseFun a = [G.Token] -> G.Err a

parseProgram :: T.Text -> Either T.Text G.Prog
parseProgram = runParser G.pProg

runParser :: ParseFun a -> T.Text -> Either T.Text a
runParser p t = case p . G.myLexer . T.unpack $ t of
                     G.Bad err  -> Left (T.pack err)
                     G.Ok  tree -> Right tree

