module Jael.Parser where

import Jael.Grammar

type ParseFun a = [Token] -> Err a

parseProgram :: Text -> Either Text GProg
parseProgram = runParser pGProg

runParser :: ParseFun a -> Text -> Either Text a
runParser p t = case p . myLexer . unpack $ t of
                     Bad err  -> Left (pack err)
                     Ok  tree -> Right tree

