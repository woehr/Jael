{-# Language NoImplicitPrelude #-}

import Prelude ()
import BasePrelude
import Jael.Expr
import Text.PrettyPrint.Leijen.Text

main :: IO ()
main = do
  putStrLn . show . pretty $ badTE

--main = hGetContents stdin
--         >>= \x -> putStrLn $ either tshow id $ compile (pack x)

