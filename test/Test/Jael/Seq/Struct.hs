module Test.Jael.Seq.Struct
( structTests
) where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Types
import           Jael.Seq.UserDefinedType
import qualified Test.Framework as T
import           Test.Jael.Util

validator :: GTypeDef -> Either UserDefinedTypeErr [(Text, HMPolyTy)]
validator (GTDefStruct (UIdent i) s) =
  let structDef = gStructToUDT s
   in maybe (Right . seqEnvItems $ (pack i, structDef)) Left (validateUDT structDef)
validator _ = error "Parsed non-struct typedef"

checkStruct :: (Text, [(Text, HMPolyTy)]) -> Assertion
checkStruct =
  checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, UserDefinedTypeErr) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

structTests :: [T.Test]
structTests =
  [ testCase "Valid struct, not polymorphic" $ checkStruct structValidSimple
  , testCase "Duplicate fields" $ checkErr structDupFields
  ]

structValidSimple :: (Text, [(Text, HMPolyTy)])
structValidSimple = (pack [raw|
  struct X { f0 : Int ,f1:Bool}
|], [ ("x",     HMPolyTy [] $ HMTyFun HMTyInt (HMTyFun HMTyBool (HMTyNamed "X" [])))
    , ("x::f0", HMPolyTy [] $ HMTyFun (HMTyNamed "X" []) HMTyInt)
    , ("x::f1", HMPolyTy [] $ HMTyFun (HMTyNamed "X" []) HMTyBool)
    ]
  )

structDupFields :: (Text, UserDefinedTypeErr)
structDupFields = (pack [raw|
  struct X { same : Int , same : Bool }
|], UDTDuplicateFieldsErr $ S.fromList ["same"]
  )

