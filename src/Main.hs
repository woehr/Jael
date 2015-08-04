{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import qualified Data.Map as M
import Jael.Seq.Grammar
import Jael.Seq.AST

data Env = Env {}
type ParseFun a = [Token] -> Err a

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

parseProgram :: Text -> Either Text GExpr
parseProgram t = case runParser pGExpr t of
                      Bad err  -> Left (pack err)
                      Ok  tree -> Right tree

runParser :: ParseFun a -> Text -> Err a
runParser p t = p (myLexer . unpack $ t)

pass1 :: GExpr -> Either Text Ex
pass1 g = do
  let env = M.fromList
        [ ("+", PolyTy [] $ TFun TInt (TFun TInt TInt))
        , ("*", PolyTy [] $ TFun TInt (TFun TInt TInt))
        ]

  -- Run type inference on each top level function and collect the results
  let inferenceResult = runSeqTI $ seqTypeInference env $ toSeqEx g

  case inferenceResult of
       Left  x -> Left (tshow (intercalate "\n\n" x) ++ "\n\n"
                          ++ "env'   : " ++ tshow env ++ "\n\n"
                          )
       Right x -> Left ("Successful inference\n\n"
                             ++ "env'   : " ++ tshow env ++ "\n\n"
                             ++ "result : " ++ tshow x
                             )

writeOutput :: Env -> Ex -> Either Text (IO ())
writeOutput env ex = Right $ hPutStrLn stdout $ tshow ex

