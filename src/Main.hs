{-# LANGUAGE NoImplicitPrelude #-}

import ClassyPrelude
import qualified Data.Map as M
import Jael.Grammar
import Jael.AST0

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

parseProgram :: Text -> Either Text GProg
parseProgram t = case runParser pGProg t of
                      Bad err  -> Left (pack err)
                      Ok  tree -> Right tree

runParser :: ParseFun a -> Text -> Err a
runParser p t = p (myLexer . unpack $ t) 

pass1 :: GProg -> Either Text AST0
pass1 g = do
  let env = M.fromList
        [ ("+", PolyTy [] $ TFun TInt (TFun TInt TInt))
        , ("*", PolyTy [] $ TFun TInt (TFun TInt TInt))
        ]
      checkMain :: AST0 -> Either Text AST0
      checkMain a = if member "main" a
                       then Right a
                       else Left  "No main function"

  -- Verify AST0 correctness (has main ... and what else?)
  ast <- toAST0 g >>= checkMain

  -- Add the types of the top level functions to the inference environment
  let env' = M.foldrWithKey (\k v acc -> M.insert k (PolyTy [] v) acc) env (M.map fst ast)

  -- Run type inference on each top level function and collect the results
  let inferenceResult = mapM (
        \(expectedTy, topLevelExpr) -> do
          inferredTy <- runTI $ typeInference env' topLevelExpr
          case mgu expectedTy inferredTy of
               Left err -> Left (err ++ "\n\n"
                                 ++ "Expected \"" ++ tshow expectedTy ++ "\" "
                                 ++ "but inferred \"" ++ tshow inferredTy ++ "\""
                                 )
               -- Need to make sure inferred return type and types of arguments
               -- are consistent with what the user supplies.
               Right _  -> Right ?
      ) ast

  -- If inference has succeeded we need to make sure that the expected type
  -- of the function (supplied by the user) match the inferred types



  case inferenceResult of
       Left err      -> Left (err ++ "\n\n"
                             ++ "AST  : " ++ tshow ast ++ "\n\n"
                             ++ "env' : " ++ tshow env'
                             )
       Right mapOfTy -> Left ("Successful inference\n\n"
                             ++ "AST     : " ++ tshow ast ++ "\n\n"
                             ++ "env'    : " ++ tshow env' ++ "\n\n"
                             ++ "mapOfTy : " ++ tshow mapOfTy
                             )

writeOutput :: Env -> AST0 -> Either Text (IO ())
writeOutput e ast0 = Right $ hPutStrLn stdout $ tshow ast0
