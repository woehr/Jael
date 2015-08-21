{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST

data Env = Env {}

main :: IO ()
main = do
  inp <- hGetContents stdin
  case compile Env {} inp of
       Left  err -> hPutStrLn stdout err
       Right res -> res

-- Compiles the text of a program.
-- Returns an error or an IO action that produces the desired output.
compile :: Env -> Text -> Either Text (IO ())
compile e p =  parseProgram p >>= pass1 >>= writeOutput e

pass1 :: GProg -> Either Text Ex
pass1 _ = undefined

writeOutput :: Env -> Ex -> Either Text (IO ())
writeOutput _ ex = Right $ hPutStrLn stdout $ tshow ex

