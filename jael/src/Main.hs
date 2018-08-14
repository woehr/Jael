{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           UI.Butcher.Monadic
import qualified Data.Text                     as T
import qualified Data.Map                      as M

import           Jael.AST
import           Jael.Prelude
import           Jael.DataDecl
import           Jael.Pattern
import           Jael.Type
import           Jael.HMInfer
import           Jael.Expr
import           Jael.QType
import           Jael.Constants

main :: IO ()
main = mainFromCmdParserWithHelpDesc $ \_helpDesc -> do
  addCmdSynopsis "cmd synopsis"
  addCmdHelpStr "help string"

  path <- addParamString "FILE" (paramHelpStr "The file to compile")

  addCmdImpl (compile path)

compile :: FilePath -> IO ()
compile path = do
  inp <- readFile path
  let
    AST {..} = parseToAST inp
    dataDecls = fmap (fmap (fmap unrefined)) _astDataDef
    env = M.fromList (fmap (second (generalize' M.empty . primType)) prims)
      `M.union` M.fromList (concatMap (dataConTypes . snd) dataDecls)

    exprDecls :: [(ParsePattern, E')]
    exprDecls =
      second
          (overExpr (id :: Type' -> Type')
                    (expandOr :: ParsePattern -> [ParsePattern'])
                    (id :: T.Text -> T.Text)
          )
        <$> _astExpr
    typedExprs = snd $ foldl'
      (\(env', es) e ->
        let e' = infer env' (snd e)
        in  case e' of
              Left err -> error (show err)
              Right (t, te) ->
                let env'' = M.insert (fst e) t env'
                in  (env'', (fst e, t, te) : es)
      )
      (env, [])
      (fmap
        (\(p, e) -> case p of
          PVar x -> (x, e)
          _      -> error "Non-var pattern"
        )
        exprDecls
      )

  --putStrLn $ pShow dataDecls
  --putStrLn $ pShow exprDecls
  putStrLn $ pShow typedExprs
