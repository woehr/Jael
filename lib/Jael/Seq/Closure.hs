{-# Language NoImplicitPrelude #-}

module Jael.Seq.Closure where

import ClassyPrelude
import Data.List.NonEmpty as NE (NonEmpty((:|)), (<|), fromList)
import qualified Data.Set as S
import Jael.Seq.AlgDataTy
import Jael.Seq.AST

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

data CConvR = CConvR
  { ccrGlobals :: S.Set Text
  , ccrEnvPrefix :: Text
  , ccrLiftLabel :: Text
  }
  deriving (Show)

data CConvW = CConvW
  { ccwFns :: [(Text, CCFun)]
  , ccwEnvs :: [Struct]
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

runCC :: CConvR -> CConvM ExCC -> (ExCC, [(Text, CCFun)], [Struct])
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
letConversion (TELet t x e1 e2) =
  TEApp t
        (TEAbs (TFun (tyOf e1) (tyOf e2))
               x
               (letConversion e2)
        )
        (letConversion e1)
letConversion (TEApp t e1 e2) = TEApp t (letConversion e1) (letConversion e2)
letConversion (TEAbs t x e) = TEAbs t x (letConversion e)
letConversion x = x

-- remove from x the elements of y
remove :: Ord a => Set a -> Set a -> Set a
remove x y = S.foldr S.delete x y

-- Return the set of free variables of the expression (types don't matter).
-- In the case of an application, if the first expression is a variable, it is
-- not considered free since to type check it would have had to be in the type
-- environment, that is, it's global and doesn't have to be captured
freeVars :: TypedEx -> S.Set Text
freeVars (TEAbs _ x e) = freeVars e `remove` S.singleton x
freeVars (TEVar _ v) = S.singleton v
freeVars (TEApp _ (TEVar _ _) e) = freeVars e
freeVars (TEApp _ f e) = freeVars f `S.union` freeVars e
freeVars (TELet _ x e1 e2) = freeVars e1 `S.union` (freeVars e2 `remove` S.singleton x)
freeVars _ = S.empty

collectArgs :: TypedEx -> [TypedEx] -> (TypedEx, [TypedEx])
collectArgs (TEApp _ f e) as = collectArgs f (e:as)
collectArgs f as = (f, as)

mashLams :: TypedEx -> [Text] -> ([Text], TypedEx)
mashLams (TEAbs _ x e) as = let (as', e') = mashLams e as
                             in (x:as', e')
mashLams e             as = (as, e)

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
doCc (TEVar  _ v)   = do
  x <- isBound v
  if x
     then return $ ECVar v
     else getLamName >>= flip mkAccessor v >>= \n -> return $ ECApp (ECFun n) [ECVar "'env"]

doCc (TEUnit _)     = return $ ECUnit
doCc (TEInt  _ i)   = return $ ECInt i
doCc (TEBool _ b)   = return $ ECBool b

doCc x@(TEApp _ _ _) =
  let (f, as) = collectArgs x []
   in do as' <- mapM doCc as
         f' <- case f of
                    TEVar _ n -> return $ ECFun n
                    _         -> doCc f
         return $ ECApp f' as'

doCc l@(TEAbs _ _ _) = liftLam $ mashLams l []

doCc x@(TELet _ _ _ _) = doCc (letConversion x)

closureConversion :: S.Set Text -> Text -> Text -> TypedEx -> (ExCC, [(Text, CCFun)], [Struct])
closureConversion globals envPref liftLabel e =
   runCC CConvR { ccrGlobals = globals
                , ccrEnvPrefix = envPref
                , ccrLiftLabel = liftLabel
                } $ doCc e

