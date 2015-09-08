{-# Language DeriveFunctor, NoImplicitPrelude, TypeFamilies #-}

module Jael.Seq.Closure where

import ClassyPrelude hiding (Foldable)
import Data.Functor.Foldable
import Data.List.NonEmpty as NE (NonEmpty((:|)), (<|), fromList)
import qualified Data.Set as S
import Jael.Seq.AST
import Jael.Seq.UserDefTy
import Jael.Seq.Types

data CCFun = CCFun [Text] ExCC
             deriving (Eq, Show)

data ExCC = ECVar Text
          | ECFun Text
          | ECUnit
          | ECInt Integer
          | ECBool Bool
          | ECApp ExCC [ExCC]
          | ECClos Text ExCC -- code and an environment
            deriving (Eq, Show)

data ExCCF a = ECVarF Text
             | ECFunF Text
             | ECUnitF
             | ECIntF Integer
             | ECBoolF Bool
             | ECAppF a [a]
             | ECClosF Text a
             deriving (Show, Functor)

type instance Base ExCC = ExCCF

instance Foldable ExCC where
  project (ECVar x)    = ECVarF x
  project (ECFun x)    = ECFunF x
  project (ECUnit)     = ECUnitF
  project (ECInt x)    = ECIntF x
  project (ECBool x)   = ECBoolF x
  project (ECApp x y)  = ECAppF x y
  project (ECClos x y) = ECClosF x y

instance Unfoldable ExCC where
  embed (ECVarF x)    = ECVar x
  embed (ECFunF x)    = ECFun x
  embed (ECUnitF)     = ECUnit
  embed (ECIntF x)    = ECInt x
  embed (ECBoolF x)   = ECBool x
  embed (ECAppF x y)  = ECApp x y
  embed (ECClosF x y) = ECClos x y

data CConvR = CConvR
  { ccrGlobals :: S.Set Text
  , ccrEnvPrefix :: Text
  , ccrLiftLabel :: Text
  }
  deriving (Show)

data CConvW = CConvW
  { ccwFns :: [(Text, CCFun)]
  , ccwEnvs :: [UserDefTy]
  }
  deriving (Show)

data CConvS = CConvS
  { ccsBoundVars :: NonEmpty (S.Set Text)
  , ccsCount :: Integer
  }
  deriving (Show)

newtype CConvM a = CConvM ((CConvR, CConvW, CConvS) -> (a, CConvW, CConvS))

instance Monad CConvM where
  (CConvM p) >>= f = CConvM $ \x@(r, _, _) ->
    let (v, w', s') = p x
        (CConvM n) = f v
     in n (r, w', s')

  return v = CConvM $ \(_, w, s) -> (v, w, s)

instance Applicative CConvM where
  pure = return
  (<*>) = ap

instance Functor CConvM where
  fmap = liftM

runCC :: CConvR -> CConvM ExCC -> (ExCC, [(Text, CCFun)], [UserDefTy])
runCC r m = let (CConvM f) = m
                (v, w, _) = f ( r
                              , CConvW{ccwFns=[], ccwEnvs=[]}
                              , CConvS{ccsBoundVars=S.empty:|[], ccsCount=(-1)})

             in (v, ccwFns w, ccwEnvs w)

cconvRead :: CConvM CConvR
cconvRead = CConvM id

cconvState :: CConvM CConvS
cconvState = CConvM $ \(_, w, s) -> (s, w, s)

-- Increments the state's count. Returns the new value.
cconvInc :: CConvM Integer
cconvInc = CConvM $ \(_, w, s@(CConvS{ccsCount=c})) -> (c+1, w, s{ccsCount=c+1})

tellFn :: (Text, CCFun) -> CConvM ()
tellFn x = CConvM $ \(_, w@(CConvW{ccwFns=fs}), s) -> ((), w{ccwFns=x:fs}, s)

genLamName :: CConvM Text
genLamName = do
  c <- cconvInc
  r <- cconvRead
  return $ ccrLiftLabel r ++ tshow c

getLamName :: CConvM Text
getLamName = do
  s <- cconvState
  r <- cconvRead
  return $ ccrLiftLabel r ++ tshow (ccsCount s)

-- Make the environment name from the lambda name
mkEnvName :: Text -> CConvM Text
mkEnvName lamName = cconvRead >>= \x -> return $ ccrEnvPrefix x ++ lamName

-- Make an accessor for the given lambda and variable
mkAccessor :: Text -> Text -> CConvM Text
mkAccessor lamName var = mkEnvName lamName >>= \x -> return $ (lowerFirst x) ++ "::" ++ var

newEnv :: S.Set Text -> CConvM Text
newEnv fields = do
  envName <- getLamName >>= mkEnvName
  CConvM $ \(_, w@(CConvW{ccwEnvs=envs}), s) -> (envName, w{ccwEnvs=envs}, s)

pushArgs :: S.Set Text -> CConvM ()
pushArgs x = CConvM $ \(_, w, s@(CConvS{ccsBoundVars=xs})) -> ((), w, s{ccsBoundVars=x<|xs})

-- Partial function. It's an error to pop more times than pushed and doing so
-- will result in NE.fromList raising an error iff xs==[]
popArgs :: CConvM (S.Set Text)
popArgs = CConvM $ \(_, w, s@(CConvS{ccsBoundVars=x:|xs})) -> (x, w, s{ccsBoundVars=NE.fromList xs})

isBound :: Text -> CConvM Bool
isBound x = CConvM $ \(_, w, s@(CConvS{ccsBoundVars=(v:|_)})) -> (x `S.member` v, w, s)

letConversion :: TypedEx -> TypedEx
letConversion = cata alg
  where alg (TypedExF (Ann {ann=t, unAnn=(ELetF x e1 e2)})) =
          mkTyped t $ EAppF (mkTyped (TFun (tyOf e1) (tyOf e2))
                                     (EAbsF x e2)
                            )
                            e1
        alg x = embed x

-- remove from x the elements of y
remove :: Ord a => Set a -> Set a -> Set a
remove x y = S.foldr S.delete x y

-- Return the set of free variables of the expression (types don't matter).
-- In the case of an application, if the first expression is a variable, it is
-- not considered free since to type check it would have had to be in the type
-- environment, that is, it's global and doesn't have to be captured
freeVars :: TypedEx -> S.Set Text
freeVars = para alg
  where alg (TypedExF (Ann {unAnn=(EAbsF x (_, e))})) = e `remove` S.singleton x
        alg (TypedExF (Ann {unAnn=(EVarF x)})) = S.singleton x
        alg (TypedExF (Ann {unAnn=(EAppF (p, e1) (_, e2))})) =
          case p of
               (TypedEx (Ann {unAnn=(EVarF _)})) -> e2
               _                                 -> e1 `S.union` e2
        alg (TypedExF (Ann {unAnn=(ELetF x (_, e1) (_, e2))})) = e1 `S.union` (e2 `remove` S.singleton x)
        alg _ = S.empty

collectArgs :: TypedEx -> [TypedEx] -> (TypedEx, [TypedEx])
collectArgs (TypedEx (Ann {unAnn=(EAppF f e)})) as = collectArgs f (e:as)
collectArgs f as = (f, as)

mashLams :: TypedEx -> [Text] -> ([Text], TypedEx)
mashLams (TypedEx (Ann {unAnn=(EAbsF x e)})) as = let (as', e') = mashLams e as
                                                   in (x:as', e')
mashLams e as = (as, e)

-- Lifts lambda with args as and expression e to the top level and returns the
-- expression that replaces it
liftLam :: ([Text], TypedEx) -> CConvM ExCC
liftLam (as, e) = do
  -- The name of the lambda whose scope contains the lambda being processed
  curName <- getLamName
  -- New name for the lambda being processed
  newName <- genLamName
  fvs <- cconvRead >>= (\r -> return $ freeVars e `remove` (ccrGlobals r `S.union` S.fromList as))
  if S.size fvs == 0
     then do
       pushArgs $ S.fromList as
       ccExpr <- doCc e
       _ <- popArgs
       tellFn (newName, CCFun as ccExpr)
       CConvM $ \(_, w, s) -> (ECFun newName, w, s)
     else do
       -- Create an environment for this lambda. The values of the free
       -- variables come from either the outer lambda's arguments or environment
       curArgs <- popArgs
       pushArgs curArgs
       pushArgs $ S.fromList as
       -- The name of the environment
       envName <- newEnv fvs
       -- The expression that constructs the enironment
       -- The generated structure has a constructor function of the same name
       -- but with the first letter lowered
       -- Its fields are the names of the free variables in ascending order
       env <- (mapM (\x ->
                      if x `S.member` curArgs
                         then return $ ECVar x
                         else mkAccessor curName x >>= \y -> return $ ECApp (ECFun y) [ECVar "'env"]
                    )
                    (S.toAscList fvs)
              ) >>= return . ECApp (ECFun $ lowerFirst envName)
       ccExpr <- doCc e
       tellFn (newName, CCFun ("'env":as) ccExpr)
       CConvM $ \(_, w, s) -> (ECClos newName env, w, s)

doCc :: TypedEx -> CConvM ExCC
doCc (TypedEx (Ann {unAnn=(EVarF v)})) = do
  x <- isBound v
  if x
     then return $ ECVar v
     else getLamName >>= flip mkAccessor v >>= \n -> return $ ECApp (ECFun n) [ECVar "'env"]

doCc (TypedEx (Ann {unAnn=(EUnitF)}))   = return $ ECUnit
doCc (TypedEx (Ann {unAnn=(EIntF i)}))  = return $ ECInt i
doCc (TypedEx (Ann {unAnn=(EBoolF b)})) = return $ ECBool b

doCc x@(TypedEx (Ann {unAnn=(EAppF _ _)})) =
  let (f, as) = collectArgs x []
   in do as' <- mapM doCc as
         f' <- case f of
                    (TypedEx (Ann {unAnn=(EVarF n)})) -> return $ ECFun n
                    _ -> doCc f
         return $ ECApp f' as'

doCc x@(TypedEx (Ann {unAnn=(EAbsF _ _)})) = liftLam $ mashLams x []

doCc x@(TypedEx (Ann {unAnn=(ELetF _ _ _)})) = doCc (letConversion x)

closureConversion :: S.Set Text -> Text -> Text -> TypedEx -> (ExCC, [(Text, CCFun)], [UserDefTy])
closureConversion globals envPref liftLabel e =
   runCC CConvR { ccrGlobals = globals
                , ccrEnvPrefix = envPref
                , ccrLiftLabel = liftLabel
                } $ doCc e

