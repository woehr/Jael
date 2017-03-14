{-# Language TemplateHaskell #-}

module Jael.Quotes where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax as S
import qualified Data.Map as M
import qualified Data.Text as T
import           Jael.Prelude
import qualified Jael.Grammar as G
import           Jael.Types
import qualified Jael.Uniqify as U

-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> S.lift (T.unpack txt)

qtype :: QuasiQuoter
qtype = QuasiQuoter
  { quoteExp  = \s -> do
      let et = runParser G.pType (T.pack s)
      gt <- case et of
        Left e -> fail $ T.unpack e
        Right x -> return x
      let t = jaelify gt
      let (ut, _) = runState (U.uniqifyReft t)
                 U.UniqifyS
                   -- A distinct program variable pool from the uniqify default
                   { U.pvPool = [T.pack $ "b" ++ show x | x <- [(0::Integer)..]]
                   , U.vvPool = [T.pack $ "v" ++ show x | x <- [(0::Integer)..]]
                   , U.varMap = M.empty
                   , U.subMap = M.empty
                   }
      dataToExpQ (\a -> liftText <$> cast a) ut
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }
