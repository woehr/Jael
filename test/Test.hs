import Data.Monoid (mempty)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts
        [ testCase "asdf" asdf
        ] mempty

asdf :: Assertion
asdf = True @?= not False

