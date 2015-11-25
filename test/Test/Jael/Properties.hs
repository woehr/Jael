module Test.Jael.Properties
( propTests
) where

import           Jael.Seq.HM_Types
import qualified Test.Framework as T

propTests :: [T.Test]
propTests =
  [ testProperty "funToTypes and typesToFun are inverse" prop_funToTypes_inverse
  ]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

arbitraryLeaf :: Gen Ty
arbitraryLeaf = oneof
  [ TyVar <$> arbitrary
  , pure TUnit
  , pure TInt
  , pure TBool
  , pure TBit
  ]

arbNotFun :: Gen Ty
arbNotFun = oneof $ arbitraryLeaf:
              [ TTup <$> listOf1 arbitraryLeaf
              , TNamed <$> arbitrary <*> listOf arbitraryLeaf
              ]

-- Not truly arbitrary but close enough because we don't expect tups and named
-- types to have TFun elements.
arbFun :: Gen Ty
arbFun = TFun <$> arbNotFun <*> oneof (arbFun:[arbNotFun])

instance Arbitrary Ty where
  arbitrary = oneof (arbFun:[arbNotFun])

prop_funToTypes_inverse :: Ty -> Property
prop_funToTypes_inverse x = typesToFun (funToTypes x) === x

