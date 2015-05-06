{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude

import Jael.Grammar
--import Jael.Grammar.Abs (Prog)
--import Jael.Grammar.ErrM (Err)
--import Jael.Grammar.Par (pProg)
import Jael.AST0

data Env = Env {}

type ParseFun a = [Token] -> Err a

runParser :: ParseFun a -> Text -> Err a
runParser p t = p (myLexer . unpack $ t) 

parseProgram :: Text -> Either Text Prog
parseProgram t = case runParser pProg t of
                      Bad err  -> Left (pack err)
                      Ok  tree -> Right tree

pass1 :: Prog -> Either Text AST0
pass1 = toAST0

writeOutput :: Env -> AST0 -> Either Text (IO ())
writeOutput e ast0 = Right $ hPutStrLn stdout $ tshow ast0

-- Compiles the text of a program.
-- Returns an error or an IO action that produces the desired output.
compile :: Env -> Text -> Either Text (IO ())
compile e p =  parseProgram p >>= pass1 >>= writeOutput e

main :: IO ()
main = do
  inp <- hGetContents stdin
  case compile Env {} inp of
       Left  err -> hPutStrLn stdout err
       Right res -> res

