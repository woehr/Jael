module Jael.Prog where

import qualified Data.Map as M
import qualified Data.Text as T
import           Jael.Expr
import           Jael.Session
--import           Jael.Type

data Prog e s = Prog { pExprs :: M.Map T.Text e
                     , pSessAliases :: M.Map T.Text s
                     } deriving (Eq, Show)

type ParsedProg = Prog MaybeTypedExpr Session

emptyProg :: Prog a b
emptyProg = Prog M.empty M.empty
