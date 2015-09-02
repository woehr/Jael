{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Closure
( closureTests
) where

import ClassyPrelude
import qualified Data.Map as M
import Jael.Grammar
import Jael.Parser
import Jael.Seq.Closure
import Jael.Seq.Env
import Jael.Seq.Expr
import Jael.Seq.TI
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

closureTests :: [T.Test]
closureTests =
  [ testCase "escaping closure" $ checkExClosureConv closureReturned
  , testCase "non-escaping closure" $ checkExClosureConv nonEscaping
  , testCase "nested lets" $ checkExClosureConv nestedLetsWithDepends
  , testCase "nested lets without dependencies" $ checkExClosureConv nestedLetsNoDepends
  , testCase "returned closures have differing environments" $ checkExClosureConv differentEnvs
  ]

checkExClosureConv :: (Text, ExCC, [(Text, CCFun)]) -> Assertion
checkExClosureConv (tx, expectedEx, expectedLiftedLams) = either
  (assertFailure . unpack)
  (\typedEx -> let (ccEx, ccLiftedLams) = closureConversion typedEx
                in do
                     assertEqual "" expectedEx ccEx
                     assertEqual "" (M.fromList expectedLiftedLams) (M.fromList ccLiftedLams)
  )
  $ do
    g <- runParser pGExpr tx
    typedEx <- either (Left . intercalate "\n") Right (seqInferTypedEx defaultEnv $ gToEx g)
    return typedEx

-- A closure is returned
-- A function lam_0 is defined at the top level as follows
-- lam_0 = \env b -> { env::a + b + 1 }
-- The lambda expression is replaced by a closure with lam_0 as the code and an
-- environment containing `a` as the data.
-- The environment is an automatically generated structure whose name is unique
-- from any user generated types.
closureReturned :: (Text, ExCC, [(Text, CCFun)])
closureReturned = (pack [raw|
  \a b -> {
    a + b + 1
  }(1)
|], ECClos "lam_0" (ECApp "env'lam_0" [ECInt 1])
  , [ ( "lam_0"
      , CCFun ["'env", "b"] (ECApp "+" [ECApp "+" [ECApp "env'lam_0::a" [ECVar "'env"], ECVar "b"], ECInt 1])
      )
    ]
  )

-- The inner lambda uses the free variable a but neither a nor b
-- escape either lambda. Ideally this should not require closures and simplify
-- lam_0 = \a b -> { a + b + 1 }
-- lam_0(1, 2)
nonEscaping :: (Text, ExCC, [(Text, CCFun)])
nonEscaping = (pack [raw|
  \a -> {
    \b -> { a + b + 1 }(2)
  }(1)
|], ECApp "lam_0" [ECInt 1, ECInt 2]
  , [ ( "lam_0"
      , CCFun ["a", "b"] (ECApp "+" [ECApp "+" [ECVar "a", ECVar "b"], ECInt 1])
      )
    ]
  )

-- let statements are converted to lambdas and closure converted
nestedLetsWithDepends :: (Text, ExCC, [(Text, CCFun)])
nestedLetsWithDepends = (pack [raw|
  \a -> {
    x = 1;
    y = x + 1;
    z = y + 1;
    a + z
  }
|], ECUnit
  , []
  )

-- Without dependencies between the let statements the lambdas should be
-- "smashed" together
-- lam_0_0 = \env x y z -> { env::a + x + y + z }
-- lam_0 = \a -> { lam_0_0(lam_0_0_env(a), 2*a, 3*a, 4*a) }
nestedLetsNoDepends :: (Text, ExCC, [(Text, CCFun)])
nestedLetsNoDepends = (pack [raw|
  \a -> {
    x = 2 * a;
    y = 3 * a;
    z = 4 * a;
    a + x + y + z
  }
|], ECUnit
  , []
  )

-- The environments in the two branches of the if are different but the types
-- of the branches are the same. The environments after closure conversion will
-- be different, but this should still be valid. See "Typed Closure Conversion"
-- by Minamide, Morrisett, and Harper
differentEnvs :: (Text, ExCC, [(Text, CCFun)])
differentEnvs = (pack [raw|
  \a b -> {
    if b {
      \x -> { a + x }
    } else {
      \x -> { x }
    }
  }
|], ECUnit
  , []
  )

