module Test.Jael.Seq.Struct
( structTests
) where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Struct
import           Jael.Seq.Types
import           Jael.UserDefTy
import qualified Test.Framework as T
import           Test.Jael.Util

validator :: GTypeDef -> Either StructDefError [(Text, PolyTy)]
validator (GTDefStruct (UIdent i) s) =
  validate' (pack i, gToUserDefTy s :: Struct)
validator _ = error "Parsed non-struct typedef"

checkStruct :: (Text, [(Text, PolyTy)]) -> Assertion
checkStruct =
  checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, StructDefError) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

structTests :: [T.Test]
structTests =
  [ testCase "Valid struct, not polymorphic" $ checkStruct structValidSimple
  , testCase "Valid struct, polymorphic" $ checkStruct structValidPoly
  , testCase "Duplicate type variables" $ checkErr structDupTyVars
  , testCase "Duplicate fields" $ checkErr structDupFields
  , testCase "Free type variables in struct types" $ checkErr structFreeTvs
  , testCase "Unused type variable in struct" $ checkErr structUnusedTv
  , testCase "Combination of all possible errors" $ checkErr structAllErrs
  ]

structValidSimple :: (Text, [(Text, PolyTy)])
structValidSimple = (pack [raw|
  struct X { f0 :: Int ,f1::Bool}
|], [ ("x",     PolyTy [] $ TFun TInt (TFun TBool (TNamed "X" [])))
    , ("X::f0", PolyTy [] $ TFun (TNamed "X" []) TInt)
    , ("X::f1", PolyTy [] $ TFun (TNamed "X" []) TBool)
    ]
  )

structValidPoly :: (Text, [(Text, PolyTy)])
structValidPoly = (pack [raw|
  struct X a b{f0::a, f1 :: b }
|], [ ("x",     PolyTy ["a", "b"] $ TFun (TyVar "a") (TFun (TyVar "b")
                                      (TNamed "X" [TyVar "a", TyVar "b"])
                                    ))
    , ("X::f0", PolyTy ["a", "b"] $ TFun (TNamed "X" [TyVar "a", TyVar "b"])
                                         (TyVar "a")
      )
    , ("X::f1", PolyTy ["a", "b"] $ TFun (TNamed "X" [TyVar "a", TyVar "b"])
                                         (TyVar "b")
      )
    ]
  )

structDupTyVars :: (Text, StructDefError)
structDupTyVars = (pack [raw|
  struct X a a { f1:: a , f2 :: a }
|], StructDefError
      { sErrDupTv = S.fromList ["a"]
      , sErrDupField = S.empty
      , sErrFreeTv = S.empty
      , sErrUnusedTv = S.empty
      }
  )

structDupFields :: (Text, StructDefError)
structDupFields = (pack [raw|
  struct X { same :: Int , same :: Bool }
|], StructDefError
      { sErrDupTv = S.empty
      , sErrDupField = S.fromList ["same"]
      , sErrFreeTv = S.empty
      , sErrUnusedTv = S.empty
      }
  )

structFreeTvs :: (Text, StructDefError)
structFreeTvs = (pack [raw|
  struct X a { f1 :: a , f2 :: b }
|], StructDefError
      { sErrDupTv = S.empty
      , sErrDupField = S.empty
      , sErrFreeTv = S.fromList ["b"]
      , sErrUnusedTv = S.empty
      }
  )

structUnusedTv :: (Text, StructDefError)
structUnusedTv = (pack [raw|
  struct X a { f1 :: Int , f2 :: Bool }
|], StructDefError
      { sErrDupTv = S.empty
      , sErrDupField = S.empty
      , sErrFreeTv = S.empty
      , sErrUnusedTv = S.fromList ["a"]
      }
  )

structAllErrs :: (Text, StructDefError)
structAllErrs = (pack [raw|
  struct X a a c c { f1 :: a , f1 :: a , f2 :: b , f2 :: b }
|], StructDefError
      { sErrDupTv = S.fromList ["a", "c"]
      , sErrDupField = S.fromList ["f1", "f2"]
      , sErrFreeTv = S.fromList ["b"]
      , sErrUnusedTv = S.fromList ["c"]
      }
  )

