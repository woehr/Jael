module Test.Jael.Seq.Closure
( closureTests
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Parser
import           Jael.Seq.AST
import           Jael.Seq.Closure
import           Jael.Seq.Env
import           Jael.Seq.Expr
import           Jael.Seq.TI
import           Jael.Seq.Types
import qualified Test.Framework as T

closureTests :: [T.Test]
closureTests =
  [ testCase "lets to lambdas" $ checkLetConv letConv
  , testCase "closure is returned" $ checkExClosureConv closureReturned
  , testCase "non-escaping closure" $ checkExClosureConv nonEscaping
  , testCase "nested lets with dependencies" $ checkExClosureConv nestedLetsWithDepends
  , testCase "nested lets without dependencies" $ checkExClosureConv nestedLetsNoDepends
  , testCase "returned closures have differing environments" $ checkExClosureConv differentEnvs
  ]

instance Eq TypedEx where
  (TypedEx x) == (TypedEx y) = ann x `tyEquiv` ann y && unAnn x == unAnn y

instance Eq TypedCC where
  (TypedCC x) == (TypedCC y) = ann x `tyEquiv` ann y && unAnn x == unAnn y

instance Eq CCFun where
  (CCFun{funArg=a, funEnv=e, funExp=x}) == (CCFun{funArg=a', funEnv=e', funExp=x'}) =
    a == a' && M.keys e == M.keys e' && (and . M.elems) (M.intersectionWith tyEquiv e e') && x == x'

checkLetConv :: (Text, TypedEx) -> Assertion
checkLetConv (tx, te) = either
  (assertFailure . unpack)
  (assertEqual "" te)
  $ do
    g <- runParser pGExpr tx
    either (Left . intercalate "\n")
           Right
           (liftA letConversion . seqInferTypedEx defaultEnv $ gToEx g)

--checkFreeVars :: (TypedEx, S.Set Text) -> Assertion
--checkFreeVars (te, s) = assertEqual "" (freeVars te) s

checkExClosureConv :: (Text, TypedCC, [(Text, CCFun)]) -> Assertion
checkExClosureConv (tx, expectedEx, expectedLiftedLams) = either
  (assertFailure . unpack)
  (\typedEx -> let (ccEx, ccLiftedLams) =
                     closureConversion S.empty "lam'" typedEx
                in do
                     assertEqual "" expectedEx ccEx
                     assertEqual "" (M.fromList expectedLiftedLams) ccLiftedLams
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
|], mkTyped (TFun TInt TInt) $ EAbsF
          "a"
          (mkTyped TInt $ EAppF
                 (mkTyped (TFun TInt TInt) $ EAbsF
                        "x"
                        (mkTyped TInt $ EAppF
                               (mkTyped (TFun TInt TInt) $ EAbsF
                                      "y"
                                      (mkTyped TInt $ EAppF
                                             (mkTyped (TFun TInt TInt) $ EAbsF
                                                    "z"
                                                    (mkTyped TInt $ EAppF
                                                           (mkTyped (TFun TInt TInt) $ EAppF
                                                                  (mkTyped (TFun TInt (TFun TInt TInt)) $ EPrmF PAdd)
                                                                  (mkTyped TInt $ EVarF "a")
                                                           )
                                                           (mkTyped TInt $ EVarF "z")
                                                    )
                                             )
                                             (mkTyped TInt $ EAppF
                                                    (mkTyped (TFun TInt TInt) $ EAppF
                                                           (mkTyped (TFun TInt (TFun TInt TInt)) $ EPrmF PAdd)
                                                           (mkTyped TInt $ EVarF "y")
                                                    )
                                                    (mkTyped TInt $ ELitF $ LInt 1)
                                             )
                                      )
                               )
                               (mkTyped TInt $ EAppF
                                      (mkTyped (TFun TInt TInt) $ EAppF
                                             (mkTyped (TFun TInt (TFun TInt TInt)) $ EPrmF PAdd)
                                             (mkTyped TInt $ EVarF "x")
                                      )
                                      (mkTyped TInt $ ELitF $ LInt 1)
                               )
                        )
                 )
                 (mkTyped TInt $ EAppF
                        (mkTyped (TFun TInt TInt) $ EAppF
                               (mkTyped (TFun TInt (TFun TInt TInt)) $ EPrmF PAdd)
                               (mkTyped TInt $ EVarF "a")
                        )
                        (mkTyped TInt $ ELitF $ LInt 1)
                 )
          )
  )

-- A closure is returned
closureReturned :: (Text, TypedCC, [(Text, CCFun)])
closureReturned = (pack [raw|
  \a b -> {
    a + b + 1
  }(1)
|], mkTypedCC (TFun TInt TInt) $ CAppF
      (mkTypedCC (TFun TInt (TFun TInt TInt)) $ CClosF
        "lam'1"
      )
      (mkTypedCC TInt $ CLitF
        (LInt 1)
      )
  , [ ( "lam'0"
      , CCFun
          { funArg = "b"
          , funEnv = M.fromList [("a", TInt)]
          , funExp = mkTypedCC TInt $ CAppF
              (mkTypedCC (TFun TInt TInt) $ CAppF
                (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                    (mkTypedCC TInt $ CVarF "a")
                  )
                  (mkTypedCC TInt $ CVarF "b")
                )
              )
              (mkTypedCC TInt $ CLitF $ LInt 1)
          }
      )
    , ( "lam'1"
      , CCFun
          { funArg = "a"
          , funEnv = M.empty
          , funExp = mkTypedCC (TFun TInt TInt) $ CClosF "lam'0"
          }
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
nonEscaping :: (Text, TypedCC, [(Text, CCFun)])
nonEscaping = (pack [raw|
  \a -> {
    \b -> { a + b + 1 }(2)
  }(1)
|], (mkTypedCC TInt $ CAppF
      (mkTypedCC (TFun TInt TInt) $ CClosF "lam'1")
      (mkTypedCC TInt $ CLitF $ LInt 1)
    )
  , [ ( "lam'0"
      , CCFun
          { funArg = "b"
          , funEnv = M.fromList [("a", TInt)]
          , funExp = mkTypedCC TInt $ CAppF
              (mkTypedCC (TFun TInt TInt) $ CAppF
                (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                    (mkTypedCC TInt $ CVarF "a")
                  )
                  (mkTypedCC TInt $ CVarF "b")
                )
              )
              (mkTypedCC TInt $ CLitF $ LInt 1)
          }
      )
    , ( "lam'1"
      , CCFun
          { funArg = "a"
          , funEnv = M.empty
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'0")
                (mkTypedCC TInt $ CLitF $ LInt 2)
              )
          }
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
nestedLetsWithDepends :: (Text, TypedCC, [(Text, CCFun)])
nestedLetsWithDepends = (pack [raw|
  \a -> {
    x = 1;
    y = x + 1;
    z = y + 1;
    a + z
  }
|], mkTypedCC (TFun TInt TInt) $ CClosF "lam'3"
  , [ ( "lam'0"
      , CCFun
          { funArg = "z"
          , funEnv = M.fromList [("a", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CAppF
                  (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                  (mkTypedCC TInt $ CVarF "a")
                )
                (mkTypedCC TInt $ CVarF "z")
              )
          }
      )
    , ( "lam'1"
      , CCFun
          { funArg = "y"
          , funEnv = M.fromList [("a", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'0")
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                    (mkTypedCC TInt $ CVarF "y")
                  )
                  (mkTypedCC TInt $ CLitF $ LInt 1)
                )
              )
          }
      )
    , ( "lam'2"
      , CCFun
          { funArg = "x"
          , funEnv = M.fromList [("a", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'1")
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                    (mkTypedCC TInt $ CVarF "x")
                  )
                  (mkTypedCC TInt $ CLitF $ LInt 1)
                )
              )
          }
      )
    , ( "lam'3"
      , CCFun
          { funArg = "a"
          , funEnv = M.empty
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'2")
                (mkTypedCC TInt $ CLitF $ LInt 1)
              )
          }
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
  }(a*2, a*3, a*4)
}

we have

\a -> {
  \x -> {
    \y -> {
      \z -> {
        a + x + y + z
      }(a*4)
    }(a*3)
  }(a*2)
}

lam'0 = \a -> { lam'1(env'lam'1(a), a*2) }
lam'1 = \env x -> { lam'2(env'lam'2(env::a, x), env::a*3) }
lam'2 = \env y -> { lam'3(env'lam'3(env::a, env::x, y), env::a*4) }
lam'3 = \env z -> { env::a + env::x + env::y + z }
-}
nestedLetsNoDepends :: (Text, TypedCC, [(Text, CCFun)])
nestedLetsNoDepends = (pack [raw|
  \a -> {
    x = a * 2;
    y = a * 3;
    z = a * 4;
    a + x + y + z
  }
|], mkTypedCC (TFun TInt TInt) $ CClosF "lam'3"
  , [ ( "lam'0"
      , CCFun
          { funArg = "z"
          , funEnv = M.fromList [("a", TInt), ("x", TInt), ("y", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF -- ((a+x)+y) + z
                (mkTypedCC (TFun TInt TInt) $ CAppF
                  (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                  (mkTypedCC TInt $ CAppF -- (a+x) + y
                    (mkTypedCC (TFun TInt TInt) $ CAppF
                      (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                      (mkTypedCC TInt $ CAppF -- a + x
                        (mkTypedCC (TFun TInt TInt) $ CAppF
                          (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PAdd))
                          (mkTypedCC TInt $ CVarF "a")
                        )
                        (mkTypedCC TInt $ CVarF "x")
                      )
                    )
                    (mkTypedCC TInt $ CVarF "y")
                  )
                )
                (mkTypedCC TInt $ CVarF "z")
              )
          }
      )
    , ( "lam'1"
      , CCFun
          { funArg = "y"
          , funEnv = M.fromList [("a", TInt), ("x", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'0")
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PTimes))
                    (mkTypedCC TInt $ CVarF "a")
                  )
                  (mkTypedCC TInt $ CLitF $ LInt 4)
                )
              )
          }
      )
    , ( "lam'2"
      , CCFun
          { funArg = "x"
          , funEnv = M.fromList [("a", TInt)]
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'1")
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PTimes))
                    (mkTypedCC TInt $ CVarF "a")
                  )
                  (mkTypedCC TInt $ CLitF $ LInt 3)
                )
              )
          }
      )
    , ( "lam'3"
      , CCFun
          { funArg = "a"
          , funEnv = M.empty
          , funExp =
              (mkTypedCC TInt $ CAppF
                (mkTypedCC (TFun TInt TInt) $ CClosF "lam'2")
                (mkTypedCC TInt $ CAppF
                  (mkTypedCC (TFun TInt TInt) $ CAppF
                    (mkTypedCC (TFun TInt (TFun TInt TInt)) (CPrmF PTimes))
                    (mkTypedCC TInt $ CVarF "a")
                  )
                  (mkTypedCC TInt $ CLitF $ LInt 2)
                )
              )
          }
      )
    ]
  )

{-
The environments in the two branches of the if are different but the types
of the branches are the same. The environments after closure conversion will
be different, but this should still be valid. See "Typed Closure Conversion"
by Minamide, Morrisett, and Harper
-}
differentEnvs :: (Text, TypedCC, [(Text, CCFun)])
differentEnvs = (pack [raw|
  \a b -> {
    if b {
      \x -> { a + x }
    } else {
      \x -> { x }
    }
  }
|], mkTypedCC (TFun (TyVar "a") (TFun TBool (TFun (TyVar "a") (TyVar "a")))) $ CClosF "lam'3"
  , [ ( "lam'0"
      , CCFun
          { funArg = "x"
          , funEnv = M.fromList [("a", TyVar "a")]
          , funExp =
              (mkTypedCC (TyVar "a") $ CAppF
                (mkTypedCC (TFun (TyVar "a") (TyVar "a")) $ CAppF
                  (mkTypedCC (TFun (TyVar "a") (TFun (TyVar "a") (TyVar "a"))) $ CPrmF PAdd)
                  (mkTypedCC (TyVar "a") $ CVarF "a")
                )
                (mkTypedCC (TyVar "a") $ CVarF "x")
              )
          }
      )
    , ( "lam'1"
      , CCFun
          { funArg = "x"
          , funEnv = M.empty
          , funExp =
              (mkTypedCC (TyVar "a") $ CVarF "x")
          }
      )
    , ( "lam'2"
      , CCFun
          { funArg = "b"
          , funEnv = M.fromList [("a", TyVar "a")]
          , funExp =
              (mkTypedCC (TFun (TyVar "a") (TyVar "a")) $ CAppF
                (mkTypedCC (TFun (TFun (TyVar "a") (TyVar "a"))
                               (TFun (TyVar "a") (TyVar "a"))
                         ) $ CAppF
                  (mkTypedCC (TFun (TFun (TyVar "a") (TyVar "a"))
                                   (TFun (TFun (TyVar "a") (TyVar "a"))
                                         (TFun (TyVar "a") (TyVar "a"))
                                   )
                             ) $ CAppF
                    (mkTypedCC (TFun (TBool)
                                     (TFun (TFun (TyVar "a") (TyVar "a"))
                                           (TFun (TFun (TyVar "a") (TyVar "a"))
                                                 (TFun (TyVar "a") (TyVar "a"))
                                           )
                                     )
                               ) $ CPrmF PIf)
                    (mkTypedCC TBool $ CVarF "b")
                  )
                  (mkTypedCC (TFun (TyVar "a") (TyVar "a")) $ CClosF "lam'0")
                )
                (mkTypedCC (TFun (TyVar "a") (TyVar "a")) $ CClosF "lam'1")
              )
          }
      )
    , ( "lam'3"
      , CCFun
          { funArg = "a"
          , funEnv = M.empty
          , funExp =
              (mkTypedCC (TFun TBool (TFun (TyVar "a") (TyVar "a"))) (CClosF "lam'2"))
          }
      )
    ]
  )

