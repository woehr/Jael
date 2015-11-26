module Test.Jael.Seq.Enum
( enumTests
) where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.HM_Types
import           Jael.Seq.UserDefinedType
import qualified Test.Framework as T
import           Test.Jael.Util

validator :: GTypeDef -> Either UserDefinedTypeErr [(Text, PolyTy)]
validator (GTDefEnum (UIdent i) e) =
  let enumDef = gEnumToUDT e
   in maybe (Right . seqEnvItems $ (pack i, enumDef)) Left (validateUDT enumDef)
validator _ = error "Parsed non-enum typedef"

checkEnum :: (Text, [(Text, PolyTy)]) -> Assertion
checkEnum =
  checkParsedTypes pGTypeDef ((either (Left . tshow) Right) . validator)

checkErr :: (Text, UserDefinedTypeErr) -> Assertion
checkErr = checkTDefErr pGTypeDef validator

enumTests :: [T.Test]
enumTests =
  [ testCase "simple enum with tags with types" $ checkEnum enumTypedTags
  , testCase "Combination of all possible errors" $ checkErr enumAllErrs
  ]

enumTypedTags :: (Text, [(Text, PolyTy)])
enumTypedTags = (pack [raw|
  enum X { f0 Int, f1 Bool }
|], [ ("x::f0", PolyTy [] $ TyFun (TySimple TyInt)  (TyNamed "X" []))
    , ("x::f1", PolyTy [] $ TyFun (TySimple TyBool) (TyNamed "X" []))
    ]
  )

enumAllErrs :: (Text, UserDefinedTypeErr)
enumAllErrs = (pack [raw|
  enum X { f0 Int, f1 Int, f1 Int, f2 Int }
|], UDTDuplicateFieldsErr $ S.fromList ["f1"]
  )

