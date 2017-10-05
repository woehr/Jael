{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
--{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

module Jael.New.Core
( CoreF(..)
, UntypedCore
, TypedCore
, toCore
, VarMap
) where

import qualified Control.Comonad.Trans.Cofree as C
import qualified Data.Bimap as M
import qualified Data.Text as T

import Jael.New.Expr
import Jael.New.Type
import Jael.New.Parser

data PrimOp = PrimRec
            deriving (Eq, Show)

data CorePattern = CorePattern
  deriving (Eq, Show)

data CoreF v t c = CTAbsF v c
                 | CTAppF c t
                 | CAbsF  v t c
                 | CAppF  c c
                 | CLetF  v t c c
                 | CCaseF c [(CorePattern, c)]
                 | CVarF  v
                 | CIntF  Integer
                 | CPrimF PrimOp
                 | CImpossibleF
                 deriving (Eq, Foldable, Functor, Show, Traversable)

type Core v t    = Cofree (CoreF v t) (Maybe E)
type UntypedCore = Core Integer ()
type TypedCore   = Core Integer Type

$(deriveEq1   ''CoreF)
$(deriveShow1 ''CoreF)

type VarMap = M.Bimap Integer T.Text

data ToCoreS = ToCoreS
  { coreStVarMap :: VarMap
  , coreStMaxInt :: Integer
  } deriving (Eq, Show)

type ToCoreM = State ToCoreS

initState :: ToCoreS
initState = ToCoreS
  { coreStVarMap = M.empty
  , coreStMaxInt = 0
  }

-- Insert v with a fresh integer if it is not in the map, return the existing
-- mapping otherwise.
lookupInsertVar :: T.Text -> ToCoreM Integer
lookupInsertVar v = do
  s@(ToCoreS{..}) <- get
  case M.lookupR v coreStVarMap of
    Just i -> return i
    Nothing ->
      let v' = coreStMaxInt
       in put s { coreStMaxInt = v' + 1
                , coreStVarMap = M.insert v' v coreStVarMap}
            >> return v'

--type Core v t    = Cofree (CoreF v t) E
--type E           = Cofree ExprF Span
toCore :: E -> (UntypedCore, VarMap)
toCore = second coreStVarMap . flip runState initState . cata alg . duplicate
  where
    alg :: C.CofreeF (ExprF () p) E (ToCoreM UntypedCore) -> ToCoreM UntypedCore

    alg (x C.:< EAbsF ps e) = undefined
    alg (x C.:< ELamCaseF alt) = undefined

    alg (x C.:< EAppF e es) = do
      e' <- e
      es' <- sequence es
      let (_ :< c) = foldl' (\acc v -> Nothing :< CAppF acc v) e' es'
      return $ Just x :< c

    alg (x C.:< ETupF es) = undefined
    alg (x C.:< ELetF bs e) = undefined
    alg (x C.:< ERecF fs) = undefined
    alg (x C.:< ERecUpF fs e) = undefined
    alg (x C.:< ERecExtF top bot) = undefined
    alg (x C.:< ERecResF e l) = undefined
    alg (x C.:< ERecSelF e l) = undefined
    alg (x C.:< ECaseF scrut alts) = undefined
    alg (x C.:< EIfF b t e) = undefined
    alg (x C.:< EMultiIfF gs def) = undefined

    alg (x C.:< EVarF v) = lookupInsertVar v >>= return . (Just x :<) . CVarF
    alg (x C.:< EConstF (CInt jint)) = undefined
    alg (x C.:< EConstF (CChar c)) = undefined
    alg (x C.:< EUnaryOpF op) = undefined
    alg (x C.:< EBinOpF op) = undefined
