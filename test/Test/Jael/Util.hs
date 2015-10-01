{-# Language NoImplicitPrelude #-}

module Test.Jael.Util where

import ClassyPrelude hiding (Enum)
import qualified Data.Map as M
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Enum
import Jael.Seq.Env
import Jael.Seq.Expr
import Jael.Seq.Struct
import Jael.Seq.TI
import Jael.Seq.Types
import Jael.UserDefTy
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.HUnit

-- https://hackage.haskell.org/package/raw-strings-qq
-- Extracted from dead-simple-json.
raw :: QuasiQuoter
raw = QuasiQuoter
  { quoteExp  = return . LitE . StringL
  , quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)"
  , quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)"
  , quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
  }

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
         Right _ -> Left $ "Expected definition error"

checkParsedTypes :: Show b
                 => ParseFun a
                 -> (a -> Either b [(Text, PolyTy)])
                 -> (Text, [(Text, PolyTy)])
                 -> Assertion
checkParsedTypes p validator (def, expected) =
  either (assertFailure . unpack) id $ do
    gDef <- runParser p def
    tys <- either (Left . tshow) Right $ validator gDef
    Right $ expected `envListEq` tys

checkInference :: Text -> (Text, Ty) -> Assertion
checkInference testTypes (tx, expected) =
  either (assertFailure . unpack) id $ do
    (GProg prog) <- runParser pGProg testTypes
    let sDefs = mapMaybe (\x -> case x of
                                     (GTopDefGTypeDef (GTDefStruct (UIdent n) m))
                                       -> Just (pack n, gToUserDefTy m)
                                     _ -> Nothing
                         ) prog :: [(Text, Struct)]
    let eDefs = mapMaybe (\x -> case x of
                                     (GTopDefGTypeDef (GTDefEnum (UIdent n) m))
                                       -> Just (pack n, gToUserDefTy m)
                                     _ -> Nothing
                         ) prog :: [(Text, Enum)]
    let structEnumErrs = mapMaybe (liftA tshow . validate . snd) sDefs ++
                         mapMaybe (liftA tshow . validate . snd) eDefs
    funs <- if length structEnumErrs == 0
               then Right $ join (map envItems sDefs) ++
                            join (map envItems eDefs)
               else Left . intercalate "\n" $ structEnumErrs
    env <- either
             (\x -> Left . intercalate "\n" $ "Duplicates in env: " : x)
             (Right)
             (addToEnv defaultEnv funs)
    ex <- runParser pGExpr tx
    ty <- either (\x -> Left . intercalate "\n" $ x)
                 Right
                 (seqInfer env $ gToEx ex)
    return $ assertBool ("Expected:\n" ++
                        show expected ++
                        "\n    and:\n" ++
                        show ty ++
                        "\nto be equivalent."
                        ) (expected `tyEquiv` ty)

envListEq :: [(Text, PolyTy)] -> [(Text, PolyTy)] -> Assertion
envListEq expected actual =
  let mExpected = TyEnv $ M.fromList expected
      mActual = TyEnv $ M.fromList actual
   in mExpected `envEq` mActual

envEq :: TyEnv -> TyEnv -> Assertion
envEq (TyEnv expected) (TyEnv actual) =
   let inter = M.intersectionWith polyEquiv expected actual
   in assertBool ("Expected:\n" ++ show expected ++
                  "\n     and:\n" ++ show actual ++
                  "\nto be equivalent."
                 )
                 (M.size expected == M.size inter && and inter)

