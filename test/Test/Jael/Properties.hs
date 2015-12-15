module Test.Jael.Properties
( propTests
) where

import           Jael.Seq.Types
import qualified Test.Framework as T

propTests :: [T.Test]
propTests =
  [ testProperty "funToTypes and typesToFun are inverse" prop_funToTypes_inverse
  ]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

arbitraryLeaf :: Gen HMTy
arbitraryLeaf = oneof
  [ HMTyVar <$> arbitrary
  , pure HMTyUnit
  , pure HMTyBool
  , pure HMTyBit
  , pure HMTyBuffer
  , pure HMTyInt
  ]

arbNotFun :: Gen HMTy
arbNotFun = oneof $ arbitraryLeaf:
              [ HMTyTup <$> listOf1 arbitraryLeaf
              , HMTyNamed <$> arbitrary <*> listOf arbitraryLeaf
              ]

-- Not truly arbitrary but close enough because we don't expect tups and named
-- types to have TFun elements.
arbFun :: Gen HMTy
arbFun = HMTyFun <$> arbNotFun <*> oneof (arbFun:[arbNotFun])

instance Arbitrary HMTy where
  arbitrary = oneof (arbFun:[arbNotFun])

prop_funToTypes_inverse :: HMTy -> Property
prop_funToTypes_inverse x = typesToFun (funToTypes x) === x

