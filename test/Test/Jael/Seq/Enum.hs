module Test.Jael.Seq.Enum
( enumTests
) where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Enum
import           Jael.Seq.HM_Types
import           Jael.UserDefTy
import qualified Test.Framework as T
import           Test.Jael.Util

validator :: GTypeDef -> Either EnumerDefError [(Text, PolyTy)]
validator (GTDefEnum (UIdent i) e) = validate' (pack i, gToUserDefTy e :: Enumer)
validator _ = error "Parsed non-enum typedef"

checkEnum :: (Text, [(Text, PolyTy)]) -> Assertion
checkEnum =
  checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, EnumerDefError) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

enumTests :: [T.Test]
enumTests =
  [ testCase "simple enum with tags with types" $ checkEnum enumTypedTags
  , testCase "simple enum with tags without types" $ checkEnum enumUntypedTags
  , testCase "simple enum with mixed tags" $ checkEnum enumMixedTags
  , testCase "Combination of all possible errors" $ checkErr enumAllErrs
  ]

enumTypedTags :: (Text, [(Text, PolyTy)])
enumTypedTags = (pack [raw|
  enum X{f0,f1}
|], [ ("x::f0", PolyTy [] $ TNamed "X" [])
    , ("x::f1", PolyTy [] $ TNamed "X" [])
    ]
  )

enumUntypedTags :: (Text, [(Text, PolyTy)])
enumUntypedTags = (pack [raw|
  enum X { f0 Int, f1 Bool }
|], [ ("x::f0", PolyTy [] $ TFun TInt  (TNamed "X" []))
    , ("x::f1", PolyTy [] $ TFun TBool (TNamed "X" []))
    ]
  )

enumMixedTags :: (Text, [(Text, PolyTy)])
enumMixedTags = (pack [raw|
  enum X a { f0, f1 a, f2 Bool }
|], [ ("x::f0", PolyTy ["a"] $ TNamed "X" [TyVar "a"])
    , ("x::f1", PolyTy ["a"] $ TFun (TyVar "a") (TNamed "X" [TyVar "a"]))
    , ("x::f2", PolyTy ["a"] $ TFun TBool (TNamed "X" [TyVar "a"]))
    ]
  )

enumAllErrs :: (Text, EnumerDefError)
enumAllErrs = (pack [raw|
  enum X a a b b { f0 a, f1 a, f1 c, f2 c }
|], EnumerDefError
      { eErrDupTv = S.fromList ["a", "b"]
      , eErrDupField = S.fromList ["f1"]
      , eErrFreeTv = S.fromList ["c"]
      , eErrUnusedTv = S.fromList ["b"]
      }
  )

