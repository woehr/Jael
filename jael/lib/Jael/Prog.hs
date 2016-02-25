module Jael.Prog where

import qualified Data.Map as M
import qualified Data.Text as T
import           Jael.Expr
import           Jael.Type

data Prog e = Prog { pExprs :: M.Map T.Text e
                   } deriving (Eq, Show)

type ParsedProg = Prog (TypedExpr (Maybe Type))

emptyProg :: Prog a
emptyProg = Prog M.empty
