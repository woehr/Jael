{-# Language NoImplicitPrelude #-}

import ClassyPrelude
import qualified Data.Map as M
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
pass1 g = undefined {-do
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
                             -}

writeOutput :: Env -> Ex -> Either Text (IO ())
writeOutput env ex = Right $ hPutStrLn stdout $ tshow ex

