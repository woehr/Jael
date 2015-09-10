{-# Language DeriveFunctor, NoImplicitPrelude, TypeFamilies #-}

module Jael.Seq.Closure where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Seq.AST
import Jael.Seq.Types

data ExCC = CVar Text
          | CPrm Prm
          | CLit Lit
          | CApp ExCC
          | CClos Text
          deriving (Eq, Show)

data ExCCF a = CVarF Text
             | CPrmF Prm
             | CLitF Lit
             | CAppF a a
             | CClosF Text
             deriving (Eq, Functor, Show)

type instance Base ExCC = ExCCF

data TypedCC = TypedCC (Ann Ty ExCCF TypedCC)
  deriving (Show)

data TypedCCF a = TypedCCF (Ann Ty ExCCF a)
  deriving (Functor, Show)

type instance Base TypedCC = TypedCCF

instance Foldable TypedCC where
  project (TypedCC (Ann {ann=t, unAnn=e})) = TypedCCF (Ann {ann=t, unAnn=e})

instance Unfoldable TypedCC where
  embed (TypedCCF (Ann {ann=t, unAnn=e})) = TypedCC (Ann {ann=t, unAnn=e})

mkTypedCC :: Ty -> ExCCF TypedCC -> TypedCC
mkTypedCC t e = TypedCC $ Ann {ann=t, unAnn=e}

data CCFun = CCFun
  { funArg :: Text
  , funEnv :: M.Map Text Ty
  , funExp :: TypedCC
  }
  deriving (Show)

data CConvR = CConvR
  { ccrGlobals :: S.Set Text
  , ccrLiftLabel :: Text
  }
  deriving (Show)

data CConvS = CConvS
  { ccsCount :: Integer
  , ccsFns :: M.Map Text CCFun
  }
  deriving (Show)

newtype CConvM a = CConvM ((CConvR, CConvS) -> (a, CConvS))

instance Monad CConvM where
  (CConvM p) >>= f = CConvM $ \x@(r, _) ->
    let (v, s') = p x
        (CConvM n) = f v
     in n (r, s')

  return v = CConvM $ \(_, s) -> (v, s)

instance Applicative CConvM where
  pure = return
  (<*>) = ap

instance Functor CConvM where
  fmap = liftM

runCC :: CConvR -> CConvM TypedCC -> (TypedCC, M.Map Text CCFun)
runCC r m = let (CConvM f) = m
                (v, s) = f ( r
                           , CConvS{ccsCount=(-1), ccsFns=M.empty}
                           )
             in (v, ccsFns s)

cconvRead :: CConvM CConvR
cconvRead = CConvM id

cconvState :: CConvM CConvS
cconvState = CConvM $ \(_, s) -> (s, s)

-- Increments the state's count. Returns the new value.
cconvInc :: CConvM Integer
cconvInc = CConvM $ \(_, s@(CConvS{ccsCount=c})) -> (c+1, s{ccsCount=c+1})

tellFn :: (Text, CCFun) -> CConvM ()
tellFn (k, v) = CConvM $ \(_, s@CConvS{ccsFns=fs}) -> ((), s{ccsFns=M.insert k v fs})

getEnvs :: CConvM (M.Map Text (M.Map Text Ty))
getEnvs = cconvState >>= return . M.map funEnv . ccsFns

genLamName :: CConvM Text
genLamName = do
  c <- cconvInc
  r <- cconvRead
  return $ ccrLiftLabel r ++ tshow c

letConversion :: TypedEx -> TypedEx
letConversion = cata alg
  where alg (TypedExF (Ann {ann=t, unAnn=(ELetF x e1 e2)})) =
          mkTyped t $ EAppF (mkTyped (TFun (tyOf e1) (tyOf e2))
                                     (EAbsF x e2)
                            )
                            e1
        alg x = embed x

-- Return the set of free variables of the expression (types don't matter).
-- In the case of an application, if the first expression is a variable, it is
-- not considered free since to type check it would have had to be in the type
-- environment, that is, it's global and doesn't have to be captured
freeVars :: M.Map Text (M.Map Text Ty) -> TypedCC -> M.Map Text Ty
freeVars envs = cata (alg envs)
  where alg :: M.Map Text (M.Map Text Ty) -> TypedCCF (M.Map Text Ty) -> M.Map Text Ty
        alg _ (TypedCCF (Ann {ann=t, unAnn=(CVarF x)})) = M.singleton x t
        alg _ (TypedCCF (Ann {unAnn=(CAppF f e )})) = f `M.union` e
        alg e (TypedCCF (Ann {unAnn=(CClosF n)})) = M.findWithDefault M.empty n e
        alg _ _ = M.empty

doCc :: TypedEx -> CConvM TypedCC
doCc = cata alg
  where alg :: TypedExF (CConvM TypedCC) -> CConvM TypedCC
        alg (TypedExF (Ann {ann=t, unAnn=(EAbsF a e)})) = do
          ccEx <- e
          globals <- cconvRead >>= \x -> return (ccrGlobals x)
          lamName <- genLamName
          envs <- getEnvs
          let fvs = foldr M.delete (freeVars envs ccEx) $ (S.insert a globals)
          tellFn (lamName, CCFun { funArg=a
                                 , funEnv=fvs
                                 , funExp=ccEx
                                 })
          return $ mkTypedCC t (CClosF lamName)
        alg (TypedExF (Ann {ann=t, unAnn=(EAppF f e)})) = liftM2 (\x y -> mkTypedCC t $ CAppF x y) f e
        alg (TypedExF (Ann {ann=t, unAnn=(EVarF x)})) = return $ mkTypedCC t $ CVarF x
        alg (TypedExF (Ann {ann=t, unAnn=(EPrmF x)})) = return $ mkTypedCC t $ CPrmF x
        alg (TypedExF (Ann {ann=t, unAnn=(ELitF x)})) = return $ mkTypedCC t $ CLitF x
        alg (TypedExF (Ann {ann=_, unAnn=(ELetF _ _ _)})) = error "Should have been let converted"

closureConversion :: S.Set Text -> Text -> TypedEx -> (TypedCC, M.Map Text CCFun)
closureConversion globals liftLabel e =
   runCC CConvR { ccrGlobals = globals
                , ccrLiftLabel = liftLabel
                } $ doCc . letConversion $ e

