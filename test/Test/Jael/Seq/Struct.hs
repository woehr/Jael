module Test.Jael.Seq.Struct
( structTests
) where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.HM_Types
import           Jael.Seq.UserDefinedType
import qualified Test.Framework as T
import           Test.Jael.Util

validator :: GTypeDef -> Either UserDefinedTypeErr [(Text, PolyTy)]
validator (GTDefStruct (UIdent i) s) =
  let structDef = gStructToUDT s
   in maybe (Right . seqEnvItems $ (pack i, structDef)) Left (validateUDT structDef)
validator _ = error "Parsed non-struct typedef"

checkStruct :: (Text, [(Text, PolyTy)]) -> Assertion
checkStruct =
  checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, UserDefinedTypeErr) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

structTests :: [T.Test]
structTests =
  [ testCase "Valid struct, not polymorphic" $ checkStruct structValidSimple
  , testCase "Duplicate fields" $ checkErr structDupFields
  ]

structValidSimple :: (Text, [(Text, PolyTy)])
structValidSimple = (pack [raw|
  struct X { f0 : Int ,f1:Bool}
|], [ ("x",     PolyTy [] $ TyFun (TySimple TyInt) (TyFun (TySimple TyBool) (TyNamed "X" [])))
    , ("x::f0", PolyTy [] $ TyFun (TyNamed "X" []) (TySimple TyInt))
    , ("x::f1", PolyTy [] $ TyFun (TyNamed "X" []) (TySimple TyBool))
    ]
  )

structDupFields :: (Text, UserDefinedTypeErr)
structDupFields = (pack [raw|
  struct X { same : Int , same : Bool }
|], UDTDuplicateFieldsErr $ S.fromList ["same"]
  )

