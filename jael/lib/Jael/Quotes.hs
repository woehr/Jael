{-# Language TemplateHaskell #-}

module Jael.Quotes where

import           Data.Generics (extQ)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as S
import qualified Text.PrettyPrint.Leijen.Text as P

import           Jael.Classes.TIOps
import qualified Jael.Grammar as G
import           Jael.Types
import qualified Jael.Uniqify as U

rtype :: QuasiQuoter
rtype = QuasiQuoter
  { quoteExp  = \s -> do
      let et = runParser G.pType (T.pack s)
      gt <- case et of
        Left e -> fail $ T.unpack e
        Right x -> return x
      let t = jaelify gt
      let (m, ut) = U.uniqifyQType (Scheme (ftv t) M.empty t) M.empty
      traceM (show . P.pretty $ t)
      traceM (show . P.pretty $ ut)
      dataToExpQ (liftText `extQ` subVar m) ut
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable

liftText :: Typeable a => a -> Maybe (Q Exp)
liftText a = (\t -> AppE (VarE 'T.pack) <$> S.lift (T.unpack t)) <$> cast a

subVar :: M.Map T.Text T.Text -> F.Expr -> Maybe (Q Exp)
subVar m (F.EVar s)
  -- If before uniqification the variable started with h_
  | Just ('h':'_':t) <- fmap T.unpack $ M.lookup (F.symbolText s) m
  -- Insert the haskell variable named t
  = Just $ varE (mkName t)
subVar _ _ = Nothing
