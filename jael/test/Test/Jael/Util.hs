module Test.Jael.Util where

import qualified Data.Map as M
import           Jael.Grammar
import           Jael.Parser
import           Jael.Seq.AST
import           Jael.Seq.Env
import           Jael.Seq.TI.HM
import           Jael.Seq.TI.S2
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType

shouldNotParse :: ParseFun a -> Text -> Assertion
shouldNotParse p t = either (\_ -> return ())
                            (\_ -> assertFailure "Expression parsed successful")
                            (runParser p t)

checkParsedTree :: (Eq a, Show a) => ParseFun a -> (Text, a) -> Assertion
checkParsedTree p (tx, tr) = either (assertFailure . unpack)
                                    (tr @=?)
                                    (runParser p tx)

checkTDefErr :: (Eq b, Show b)
             => ParseFun a
             -> (a -> Either b c)
             -> (Text, b)
             -> Assertion
checkTDefErr p validator (def, expected) =
  either assertFailure id $ do
    gDef <- either (Left . show) Right $ runParser p def
    case validator gDef of
         Left actual -> Right $ assertEqual "" expected actual
         Right _ -> Left "Expected definition error"

checkParsedTypes :: Show b
                 => ParseFun a
                 -> (a -> Either b [(Text, HMPolyTy)])
                 -> (Text, [(Text, HMPolyTy)])
                 -> Assertion
checkParsedTypes p validator (def, expected) =
  either (assertFailure . unpack) id $ do
    gDef <- runParser p def
    tys <- either (Left . tshow) Right $ validator gDef
    Right $ expected `envListEq` tys

checkInference :: Text -> (Text, HMTy) -> Assertion
checkInference testTypes (tx, expected) =
  either (assertFailure . unpack) id $ do
    (GProg prog) <- runParser pGProg testTypes
    let sDefs = mapMaybe (\x -> case x of
                                     (GTopDefGTypeDef (GTDefStruct (UIdent n) m))
                                       -> Just (pack n, gStructToUDT m)
                                     _ -> Nothing
                         ) prog
    let eDefs = mapMaybe (\x -> case x of
                                     (GTopDefGTypeDef (GTDefEnum (UIdent n) m))
                                       -> Just (pack n, gEnumToUDT m)
                                     _ -> Nothing
                         ) prog
    let structEnumErrs = mapMaybe (liftA tshow . validateUDT . snd) (sDefs ++ eDefs)
    funs <- if null structEnumErrs
               then Right $ join (map seqEnvItems (sDefs ++ eDefs))
               else Left . intercalate "\n" $ structEnumErrs
    env <- either
             (\x -> Left . intercalate "\n" $ "Duplicates in env: " : x)
             Right
             (addToEnv defaultEnv funs)
    ex <- runParser pGExpr tx
    ty <- either (Left . tshow)
                 (Right . hmTyOf)
                 (typeInf env $ gToS1Ex ex)
    return $ assertBool ("Expected : " ++ show expected ++
                         "\nbut got  :" ++ show ty)
                        (expected `tyEquiv` ty)

envListEq :: [(Text, HMPolyTy)] -> [(Text, HMPolyTy)] -> Assertion
envListEq expected actual =
  let mExpected = HMTyEnv $ M.fromList expected
      mActual = HMTyEnv $ M.fromList actual
   in mExpected `envEq` mActual

envEq :: HMTyEnv -> HMTyEnv -> Assertion
envEq (HMTyEnv expected) (HMTyEnv actual) =
   let inter = M.intersectionWith polyEquiv expected actual
   in assertBool ("Expected :" ++ show expected ++
                  "\nbut got : " ++ show actual
                 )
                 (M.size expected == M.size inter && and inter)

-- Two HMPolyTy are equivalent when their structure is the same and there exists a
-- one-to-one mapping of the type variables of both types to each other.
-- a -> b -> b is equivalent to b -> c -> c because the substituion a->b; b->c
-- makes the first into the second and b->a; c->b makes the second.
tyEquiv :: HMTy -> HMTy -> Bool
tyEquiv t u =
    case mkSub M.empty t u of
         Nothing -> False
         Just s  -> apply s t == u
    where mkSub :: HMTySub -> HMTy -> HMTy -> Maybe HMTySub
          mkSub sub (HMTyVar a) b@(HMTyVar bname) =
            case M.lookup a sub of
                 Just (HMTyVar b'name) -> if bname == b'name
                                          then Just sub
                                          else Nothing
                 _ -> Just (M.insert a b sub)
          mkSub sub (HMTyNamed n as) (HMTyNamed m bs) =
            if n == m && length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (HMTyTup as) (HMTyTup bs) =
            if length as == length bs
               then foldM (\acc (a, b) -> mkSub acc a b) sub (zip as bs)
               else Nothing
          mkSub sub (HMTyFun a a') (HMTyFun b b') =
            case mkSub sub a b of
                 Just sub' -> mkSub sub' a' b'
                 Nothing -> Nothing
          mkSub sub a b = if a == b
                             then Just sub
                             else Nothing

polyEquiv :: HMPolyTy -> HMPolyTy -> Bool
polyEquiv (HMPolyTy _ t) (HMPolyTy _ u) = t `tyEquiv` u

