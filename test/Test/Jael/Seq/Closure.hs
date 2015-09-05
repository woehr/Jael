{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Closure
( closureTests
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST
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
  [ testCase "lets to lambdas" $ checkLetConv letConv
  , testCase "no free vars" $ checkFreeVars noFv
  , testCase "one free var" $ checkFreeVars oneFv
  , testCase "nested free vars" $ checkFreeVars nestedFv
  , testCase "closure is returned" $ checkExClosureConv closureReturned
  , testCase "non-escaping closure" $ checkExClosureConv nonEscaping
  , testCase "nested lets with dependencies" $ checkExClosureConv nestedLetsWithDepends
  , testCase "nested lets without dependencies" $ checkExClosureConv nestedLetsNoDepends
  , testCase "returned closures have differing environments" $ checkExClosureConv differentEnvs
  ]

instance Eq TypedEx where
  (TEVar t x)        == (TEVar t' x')           = t `tyEquiv` t' && x == x'
  (TEUnit t)         == (TEUnit t')             = t `tyEquiv` t'
  (TEInt t i)        == (TEInt t' i')           = t `tyEquiv` t' && i == i'
  (TEBool t b)       == (TEBool t' b')          = t `tyEquiv` t' && b == b'
  (TEApp t e1 e2)    == (TEApp t' e1' e2')      = t `tyEquiv` t' && e1 == e1' && e2 == e2'
  (TEAbs t x e)      == (TEAbs t' x' e')        = t `tyEquiv` t' && x == x'   && e == e'
  (TELet t x e1 e2)  == (TELet t' x' e1' e2')   = t `tyEquiv` t' && x == x'   && e1 == e1' && e2 == e2'
  _                  == _                       = False

checkLetConv :: (Text, TypedEx) -> Assertion
checkLetConv (tx, te) = either
  (assertFailure . unpack)
  (assertEqual "" te)
  $ do
    g <- runParser pGExpr tx
    either (Left . intercalate "\n")
           Right
           (liftA letConversion . seqInferTypedEx defaultEnv $ gToEx g)

checkFreeVars :: (TypedEx, S.Set Text) -> Assertion
checkFreeVars (te, s) = assertEqual "" (freeVars te) s

checkExClosureConv :: (Text, ExCC, [(Text, CCFun)]) -> Assertion
checkExClosureConv (tx, expectedEx, expectedLiftedLams) = either
  (assertFailure . unpack)
  (\typedEx -> let (ccEx, ccLiftedLams, _) = --newStructs) =
                     closureConversion S.empty "Env'" "lam'" typedEx
                in do
                     assertEqual "" expectedEx ccEx
                     assertEqual "" (M.fromList expectedLiftedLams) (M.fromList ccLiftedLams)
  )
  $ do
    g <- runParser pGExpr tx
    either (Left . intercalate "\n") Right (seqInferTypedEx defaultEnv $ gToEx g)

letConv :: (Text, TypedEx)
letConv = (pack [raw|
  \a -> {
    x = a + 1;
    y = x + 1;
    z = y + 1;
    a + z
  }
|], TEAbs (TFun TInt TInt)
          "a"
          (TEApp TInt
                 (TEAbs (TFun TInt TInt)
                        "x"
                        (TEApp TInt
                               (TEAbs (TFun TInt TInt)
                                      "y"
                                      (TEApp TInt
                                             (TEAbs (TFun TInt TInt)
                                                    "z"
                                                    (TEApp TInt
                                                           (TEApp (TFun TInt TInt)
                                                                  (TEVar (TFun TInt (TFun TInt TInt)) "+")
                                                                  (TEVar TInt "a")
                                                           )
                                                           (TEVar TInt "z")
                                                    )
                                             )
                                             (TEApp TInt
                                                    (TEApp (TFun TInt TInt)
                                                           (TEVar (TFun TInt (TFun TInt TInt)) "+")
                                                           (TEVar TInt "y")
                                                    )
                                                    (TEInt TInt 1)
                                             )
                                      )
                               )
                               (TEApp TInt
                                      (TEApp (TFun TInt TInt)
                                             (TEVar (TFun TInt (TFun TInt TInt)) "+")
                                             (TEVar TInt "x")
                                      )
                                      (TEInt TInt 1)
                               )
                        )
                 )
                 (TEApp TInt
                        (TEApp (TFun TInt TInt)
                               (TEVar (TFun TInt (TFun TInt TInt)) "+")
                               (TEVar TInt "a")
                        )
                        (TEInt TInt 1)
                 )
          )
  )

noFv :: (TypedEx, S.Set Text)
noFv =
  ( TEAbs TUnit "a" (TEApp TUnit (TEVar TUnit "a") (TEInt TUnit 1))
  , S.empty
  )

oneFv :: (TypedEx, S.Set Text)
oneFv =
  ( TEApp TUnit (TEApp TUnit (TEVar TUnit "+") (TEInt TUnit 1)) (TEVar TUnit "a")
  , S.fromList ["a"]
  )

nestedFv :: (TypedEx, S.Set Text)
nestedFv =
  ( TEAbs TUnit
          "a"
          (TEApp TUnit
                 (TEAbs TUnit
                        "b"
                        (TEApp TUnit (TEVar TUnit "a") (TEVar TUnit "c"))
                 )
                 (TEApp TUnit (TEVar TUnit "a") (TEVar TUnit "d"))
          )
  , S.fromList ["c", "d"]
  )

-- A closure is returned
-- A function lam'0 is defined at the top level as follows
-- lam'0 = \a b -> { a + b + 1 } since is doesn't capture any variables.
-- The partial application is replaced by a closure with lam_0 as the code,
-- with no environment (since nothing was captured), and with the first applied
-- argument in the argument list.
closureReturned :: (Text, ExCC, [(Text, CCFun)])
closureReturned = (pack [raw|
  \a b -> {
    a + b + 1
  }(1)
|], ECApp (ECFun "lam'0") [ECInt 1]
  , [ ( "lam'0"
      , CCFun ["a", "b"] (ECApp (ECFun "+") [ECApp (ECFun "+") [ECVar "a", ECVar "b"], ECInt 1])
      )
    ]
  )

{-
Similar to the nested lets without dependencies the following could be optimized
to be

\a b -> {
  a + b + 1
}(1, 2)

instead of what it is.
-}
nonEscaping :: (Text, ExCC, [(Text, CCFun)])
nonEscaping = (pack [raw|
  \a -> {
    \b -> { a + b + 1 }(2)
  }(1)
|], ECApp (ECFun "lam'0") [ECInt 1]
  , [ ( "lam'0"
      , CCFun ["a"] $ ECApp (ECClos "lam'1"
                                    (ECApp (ECFun "env'lam'1") [ECVar "a"])
                            )
                            [ECInt 2]
      )
    , ( "lam'1"
      , CCFun ["'env", "b"] $ ECApp (ECFun "+")
                                    [ ECApp (ECFun "+")
                                            [ ECApp (ECFun "env'lam'1::a") [ECVar "'env"]
                                            , ECVar "b"
                                            ]
                                    , ECInt 1
                                    ]
      )
    ]
  )

{-
let statements are converted to lambdas and closure converted
\a -> {
  \x -> {
    \y -> {
      \z -> {
        a + z
      }(y+1)
    }(x+1)
  }(1)
}

lam'0 = \a -> { lam'1('env(a), 1) }
lam'1 = \'env x -> { lam'2('env, x+1) }
lam'2 = \'env y -> { lam'3('env, y+1) }
lam'3 = \'env z -> { 'env::a + z }

-}
nestedLetsWithDepends :: (Text, ExCC, [(Text, CCFun)])
nestedLetsWithDepends = (pack [raw|
  \a -> {
    x = 1;
    y = x + 1;
    z = y + 1;
    a + z
  }
|], ECFun "lam'0"
  , [ ( "lam'0"
      , CCFun ["a"] $ ECApp (ECClos "lam'1"
                                    (ECApp (ECFun "env'lam'1")
                                           [ECVar "a"]
                                    )
                            )
                            [ECInt 1]
      )
    , ( "lam'1"
      , CCFun ["'env", "x"] $ ECApp (ECClos "lam'2"
                                            (ECApp (ECFun "env'lam'2")
                                                   [ ECApp (ECFun "env'lam'1::a")
                                                           [ECVar "'env"]
                                                   ]
                                            )
                                    )
                                    [ ECApp ( ECFun "+"
                                            )
                                            [ ECVar "x"
                                            , ECInt 1
                                            ]
                                    ]
      )
    , ( "lam'2"
      , CCFun ["'env", "y"] $ ECApp (ECClos "lam'3"
                                            (ECApp (ECFun "env'lam'3")
                                                   [ ECApp (ECFun "env'lam'2::a")
                                                           [ECVar "'env"]
                                                   ]
                                            )
                                    )
                                    [ ECApp ( ECFun "+"
                                            )
                                            [ ECVar "y"
                                            , ECInt 1
                                            ]
                                    ]
      )
    , ( "lam'3"
      , CCFun ["'env", "z"] $ ECApp (ECFun "+")
                                    [ ECApp (ECFun "env'lam'3::a")
                                            [ECVar "'env"]
                                    , ECVar "z"
                                    ]
      )
    ]
  )

{-
Without dependencies between the let statements the lambdas could be
combined together, but the closure conversion doesn't do that at the moment

so instead of
\a -> {
  \x y z -> {
    a + x + y + z
  }(2*a, 3*a, 4*a)
}

we have

\a -> {
  \x -> {
    \y -> {
      \z -> {
        a + x + y + z
      }(4*a)
    }(3*a)
  }(2*a)
}

lam'0 = \a -> { lam'1(env'lam'1(a), 2*a) }
lam'1 = \env x -> { lam'2(env'lam'2(env::a, x), 3*env::a) }
lam'2 = \env y -> { lam'3(env'lam'3(env::a, env::x, y), 4*env::a) }
lam'3 = \env z -> { env::a + env::x + env::y + z }
-}
nestedLetsNoDepends :: (Text, ExCC, [(Text, CCFun)])
nestedLetsNoDepends = (pack [raw|
  \a -> {
    x = 2 * a;
    y = 3 * a;
    z = 4 * a;
    a + x + y + z
  }
|], ECFun "lam'0"
  , [ ( "lam'0"
      , CCFun ["a"] $ ECApp (ECClos "lam'1" $ (ECApp (ECFun "env'lam'1")
                                                     [ECVar "a"]
                                              )
                            )
                            [ ECApp (ECFun "*") [ECInt 2, ECVar "a"]
                            ]
      )
    , ( "lam'1"
      , CCFun ["'env", "x"] $ ECApp (ECClos "lam'2" $ (ECApp (ECFun "env'lam'2")
                                                             [ ECApp (ECFun "env'lam'1::a") [ECVar "'env"]
                                                             , ECVar "x"
                                                             ]
                                                      )
                                    )
                                    [ ECApp (ECFun "*")
                                            [ ECInt 3
                                            , ECApp (ECFun "env'lam'1::a") [ECVar "'env"]
                                            ]
                                    ]
      )
    , ( "lam'2"
      , CCFun ["'env", "y"] $ ECApp (ECClos "lam'3" $ (ECApp (ECFun "env'lam'3")
                                                             [ ECApp (ECFun "env'lam'2::a") [ECVar "'env"]
                                                             , ECApp (ECFun "env'lam'2::x") [ECVar "'env"]
                                                             , ECVar "y"
                                                             ]
                                                      )
                                    )
                                    [ ECApp (ECFun "*")
                                            [ ECInt 4
                                            , ECApp (ECFun "env'lam'2::a") [ECVar "'env"]
                                            ]
                                    ]
      )
    , ( "lam'3"
      , CCFun ["'env", "z"] $ ECApp (ECFun "+")
                                    [ ECApp (ECFun "+")
                                            [ ECApp (ECFun "+")
                                                    [ ECApp (ECFun "env'lam'3::a") [ECVar "'env"]
                                                    , ECApp (ECFun "env'lam'3::x") [ECVar "'env"]
                                                    ]
                                            , ECApp (ECFun "env'lam'3::y") [ECVar "'env"]
                                            ]
                                    , ECVar "z"
                                    ]
      )
    ]
  )

{-
The environments in the two branches of the if are different but the types
of the branches are the same. The environments after closure conversion will
be different, but this should still be valid. See "Typed Closure Conversion"
by Minamide, Morrisett, and Harper
-}
differentEnvs :: (Text, ExCC, [(Text, CCFun)])
differentEnvs = (pack [raw|
  \a b -> {
    if b {
      \x -> { a + x }
    } else {
      \x -> { x }
    }
  }
|], ECFun "lam'0"
  , [ ( "lam'0"
      , CCFun ["a", "b"] $
              ECApp (ECFun "if")
                    [ ECVar "b"
                    , ECClos "lam'1" $ ECApp (ECFun "env'lam'1") [ECVar "a"]
                    , ECFun "lam'2"
                    ]
      )
    , ( "lam'1"
      , CCFun ["'env", "x"] $
              ECApp (ECFun "+")
                    [ ECApp (ECFun "env'lam'1::a") [ECVar "'env"]
                    , ECVar "x"
                    ]
      )
    , ( "lam'2"
      , CCFun ["x"] $ ECVar "x"
      )
    ]
  )

