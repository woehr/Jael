{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Jael.New.Quotes where

import qualified Control.Comonad.Trans.Cofree as C
import           Data.Generics (extQ)
import qualified Data.Text as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as S
import           Text.Trifecta
import           Text.Trifecta.Delta

import Jael.New.Expr
import Jael.New.Parser
import Jael.New.QType

unSpanT :: Cofree (QTypeF T.Text (Cofree (ExprF () (Cofree (PatternF T.Text) a) T.Text) a)) a
        ->         QType  T.Text (        Expr  ()          Pattern             T.Text)
unSpanT = hoistFix (\(r C.:< t) -> fmap (fmap $ removeAnn . hoistCofree (mapExprP removeAnn)) r C.:< t) . removeAnn

rtype :: QuasiQuoter
rtype = QuasiQuoter
  { quoteExp  = \s -> do
      let mt = parseString (whiteSpace *> pType0) (Directed "" 0 0 0 0) s
      t <- case mt of
        Failure e -> fail $ show e
        Success x -> return $ unSpanT x
      --traceM $ show t
      dataToExpQ (liftText `extQ` subVar) t
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

-- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable

liftText :: Typeable a => a -> Maybe (Q Exp)
liftText a = (\t -> AppE (VarE 'T.pack) <$> S.lift (T.unpack t)) <$> cast a

subVar :: Expr () Pattern T.Text -> Maybe (Q Exp)
subVar (EVar s)
  -- If before uniqification the variable started with h_
  | ('h':'_':t) <- T.unpack s
  -- Insert the haskell variable named t
  = Just $ varE (mkName t)
subVar _ = Nothing
