{-# Language NoImplicitPrelude #-}

module Test.Jael.Util where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Env
import Jael.Seq.Expr
import Jael.Seq.TI
import Jael.Seq.Types
import Jael.Seq.UserDefTy
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

checkTDefErr :: ParseFun a
             -> (a -> Either TDefError b)
             -> (Text, TDefError)
             -> Assertion
checkTDefErr p validator (def, TDefError { dupTv = ets
                                         , dupField = efs
                                         , freeTv = eftvs
                                         , unusedTv = eutvs
                                         }) =
  case runParser p def of
       Left err -> assertFailure (show err)
       Right gDef ->
         case validator gDef of
              Left (TDefError { dupTv = ts
                              , dupField = fs
                              , freeTv = ftvs
                              , unusedTv = utvs})-> do
                   assertEqual "Duplicate type variables wrong" ets ts
                   assertEqual "Duplicate fields wrong" efs fs
                   assertEqual "Free type variables wrong" eftvs ftvs
                   assertEqual "Unused type variables wrong" eutvs utvs
              Right _ -> assertFailure "Expected struct error"

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
    tDefs <- mapM (\xs -> case xs of
                               GTopDefGTypeDef t -> Right t
                               y -> Left ("Parsed non-type: " ++ tshow y)
                  )
                  prog
    funs <- either (Left . tshow)
                   (Right . concat)
                   (mapM (validateType . gToUserDefTy) $ tDefs)
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

