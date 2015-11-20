module Test.Jael.Seq.Struct
( structTests
) where

import           Jael.Grammar
import           Jael.Seq.HM_Types
import           Jael.Seq.Struct
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
  , testCase "Duplicate fields" $ checkErr structDupFields
  ]

structValidSimple :: (Text, [(Text, PolyTy)])
structValidSimple = (pack [raw|
  struct X { f0 : Int ,f1:Bool}
|], [ ("x",     PolyTy [] $ TFun TInt (TFun TBool (TNamed "X" [])))
    , ("x::f0", PolyTy [] $ TFun (TNamed "X" []) TInt)
    , ("x::f1", PolyTy [] $ TFun (TNamed "X" []) TBool)
    ]
  )

structDupFields :: (Text, StructDefError)
structDupFields = (pack [raw|
  struct X { same : Int , same : Bool }
|], SDEDupFields ["same"]
  )

