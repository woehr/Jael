{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Conc.TyCk
( concTyCkTests
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Conc.Proc
import Jael.Conc.Session
import Jael.Conc.TyCk
import Jael.Seq.Env
import Jael.Seq.Types
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

concTyCkTests :: [T.Test]
concTyCkTests =
  [ testCase "empty proc" $ shouldTyCk emptyProc
  , testCase "unused linear argument" $ checkTyCkErr unusedLinearArg
  , testCase "session alias used" $ shouldTyCk sessionAlias
  , testCase "valid channel put" $ shouldTyCk channelPut
  , testCase "unused seq from arg and chan" $ checkTyCkErr unusedSeqVars
  , testCase "sequential vars used in expr" $ shouldTyCk seqUsedInExpr
  , testCase "unused linear from chan" $ checkTyCkErr channelGetUnusedLin
  , testCase "rec def unfolding" $ checkTyCkErr recDefUnfold
  , testCase "alias has rec var with same name" $ checkTyCkErr reusedRecVarInAlias
  , testCase "bad label" $ checkTyCkErr badLabel
  , testCase "channel not used in parallel (get)" $ checkTyCkErr nonParGet
  , testCase "channel not used in parallel (put)" $ checkTyCkErr nonParPut
  , testCase "channel not used in parallel (sel)" $ checkTyCkErr nonParSel
  , testCase "channel not used in parallel (cho)" $ checkTyCkErr nonParCho
  , testCase "non-fresh channel put on channel" $ checkTyCkErr nonFreshChanPut
  , testCase "sent channel consumed" $ checkTyCkErr sentChannelConsumed
  , testCase "received channel non parallel usage" $ shouldTyCk nonParallelChanRx
  , testCase "named proc passed duals" $ checkTyCkErr namedProcWithDualArgs
  , testCase "named proc insufficient args" $ checkTyCkErr namedProcInsufficientArgs
  , testCase "named proc channel type err" $ checkTyCkErr namedProcWrongChanArg
  , testCase "named proc expr type err" $ checkTyCkErr namedProcWrongExprArg
  , testCase "named proc consumes resources" $ shouldTyCk namedProcConsume
  , testCase "parallel composition splits env correctly" $ checkTyCkErr parEnvSplit
  , testCase "case does not implement correct labels" $ checkTyCkErr caseMismatchedLabels
  , testCase "check channel used properly after case" $ shouldTyCk checkCaseChannelSession
  , testCase "duals used in same parallel proc" $ checkTyCkErr dualsUsedInSameParProc
  , testCase "put channel interference" $ checkTyCkErr putChannelInterference
  , testCase "put channel continuation used in parallel" $ checkTyCkErr putChannelParContinuation
  , testCase "make sure rx'd channels are not considered fresh" $ checkTyCkErr nonFreshChanRx
  , testCase "check type errors in cases" $ checkTyCkErr caseTypeErrors
  , testCase "check all cases use resources the same" $ checkTyCkErr casesWithDifferingUsages
  -- The remainder of these tests are specific examples from papers. See the
  -- inline comments for more details
  , testCase "example 1" $ checkTyCkErr ex1
  , testCase "example 2" $ checkTyCkErr ex2
  , testCase "example 3" $ shouldTyCk ex3
  ]

-- A map of aliases that will be passed to the type checking function
testAliases :: M.Map Text Session
testAliases =
  M.map (
    \t -> case runParser pGSession t of
               Left err -> error (unpack err)
               Right g  -> gToSession g
  ) $ M.fromList
        [ ("GetInt", "?[Int]")
        , ("PutInt", "![Int]")
        , ("AltTxRxInt", "rec X. ![Int] ?[Int] <X>")
        ]

-- A map of function names to their arguments that will be passed to the type
-- checking function
testProcs :: M.Map Text [(Text, TyOrSess)]
testProcs =
  M.map (map $
    \t -> case runParser pGProcArg t of
               Left err -> error (unpack err)
               Right g -> gToProcArg g
  ) $ M.fromList
        [ ("DualArgProc", ["^arg1: ![Int]", "^arg2: ?[Int]"])
        , ("ProcArgTest", ["a1: Int", "a2: Bool", "^a3: ![Int]", "^a4: ![Bool]"])
        ]

doTyCk :: Text -> Maybe SessTyErr
doTyCk = tyCheckTopProc defaultEnv testAliases testProcs
         . parseTopProc
         . pGTopDef
         . myLexer
         . unpack

shouldTyCk :: Text -> Assertion
shouldTyCk t =
  case doTyCk t of
       Just err -> assertFailure $ show err
       Nothing  -> return ()

checkTyCkErr :: (Text, SessTyErr) -> Assertion
checkTyCkErr (t, expected) =
  case doTyCk t of
       Just err -> assertEqual "" expected err
       Nothing -> assertFailure "Expected type check error"

parseTopProc :: Err GTopDef -> TopProc
parseTopProc (Ok (GTopDefGProcDef (GProcDef _ as p))) = gToTopProc (as, p)
parseTopProc x = error $ "Not a TopProc definition or invalid syntax:\n" ++ show x

emptyProc :: Text
emptyProc = pack [raw|
  proc X(){ }
|]

unusedLinearArg :: (Text, SessTyErr)
unusedLinearArg = (pack [raw|
  proc X(^x: ?[Int]){ }
|], UnusedResources{ unusedLin=M.fromList [("x", SGetTy TInt SEnd)]
                   , unusedSeq=M.empty
                   }
  )

sessionAlias :: Text
sessionAlias = pack [raw|
  proc X(^x: ![Int] <GetInt>, ^y: ![Int]) {
    // Put an int on x and then get an int on x.
    ^x <- 5;
    ^x -> z;
    ^y <- z; // Use z so we don't get an unused sequential variable error
  }
|]

channelPut :: Text
channelPut = pack [raw|
  proc X(^x: ![Int]) {
    ^x <- 42;
  }
|]

unusedSeqVars :: (Text, SessTyErr)
unusedSeqVars = (pack [raw|
  proc X(^x: ?[Int], y: Bool) {
    ^x -> z;
  }
|], UnusedResources{ unusedSeq=M.fromList [("y", TBool), ("z", TInt)]
                   , unusedLin=M.empty
                   }
  )

seqUsedInExpr :: Text
seqUsedInExpr = pack [raw|
  proc X(x: Bool, ^y: ?[Int], ^z: ![Int]) {
    ^y -> a;
    ^z <- if x { a } else { 42 };
  }
|]

-- Test that y being unused is an error. <GetInt> is an alias that is resolved
-- so y is reported not as the alias but it's actual session type
channelGetUnusedLin :: (Text, SessTyErr)
channelGetUnusedLin = (pack [raw|
  proc X(^x: ?[<GetInt>]) {
    ^x -> ^y;
  }
|], UnusedResources{ unusedLin=M.fromList [("y", SGetTy TInt SEnd)]
                   , unusedSeq=M.empty
                   }
  )

-- Test that a recursive session definition is unfolded when defined and used
-- This tests two cases, the first when the recursive session is not used at
-- all and the second when the recursive definition needs its variable unfolded
recDefUnfold :: (Text, SessTyErr)
recDefUnfold = (pack [raw|
  proc P(^x: <AltTxRxInt>, ^y: <AltTxRxInt>) {
    ^x <- 42;
    ^x -> z;
  }
|], UnusedResources{ unusedLin=M.fromList
                      [ ("x", SPutTy TInt $ SGetTy TInt $ SVar "X")
                      , ("y", SPutTy TInt $ SGetTy TInt $ SVar "X")
                      ]
                   , unusedSeq=M.fromList [("z", TInt)]
                   }
  )

-- The session AltTxRxInt is defined with a recursion variable X. By defining
-- a session that uses that alias and defines its own recursion variable with
-- the same name, and substitution of AltTxRxInt would create an ambiguity.
-- This test checks that the X in AltTxRxInt is renamed such that there is no
-- ambiguity.
reusedRecVarInAlias :: (Text, SessTyErr)
reusedRecVarInAlias = (pack [raw|
  proc P( ^x: rec X. +[a => <X>, b => <AltTxRxInt>]
        , ^y: rec X. +[a => <X>, b => <AltTxRxInt>]
        )
  {
    ^x select b;
    ^x <- 42;
    ^x -> z;
    ^y select a;
  }
|], UnusedResources{ unusedLin=M.fromList
                      [ ("x", SPutTy TInt $ SGetTy TInt $ SVar "X")
                      , ("y", SSelect [("a", SVar "X"), ("b", SVar "AltTxRxInt")])
                      ]
                   , unusedSeq=M.fromList [("z", TInt)]
                   }
  )

badLabel :: (Text, SessTyErr)
badLabel = (pack [raw|
  proc P( ^x: +[a => , b => ![Int]])
  {
    ^x select c;
  }
|], UnknownLabel "c"
  )

-- The following four tests check that a fresh channel is not used outside of
-- a parallel composition. This is necessary for the Tcut rule to be
-- satisfied. Intuitively, it ensures that there is always a process
-- communicating on each end of a channel.
nonParGet :: (Text, SessTyErr)
nonParGet = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int];
    // Expecting an error here before an unused linear resources error.
    ^y -> z;
  }
|], NonParallelUsage "y"
  )

nonParPut :: (Text, SessTyErr)
nonParPut = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int];
    ^x <- 42;
  }
|], NonParallelUsage "x"
  )

nonParSel :: (Text, SessTyErr)
nonParSel = (pack [raw|
  proc P() {
    new (^x, ^y) : +[a => ];
    ^x select a;
  }
|], NonParallelUsage "x"
  )

nonParCho :: (Text, SessTyErr)
nonParCho = (pack [raw|
  proc P() {
    new (^x, ^y) : +[a => ];
    ^y case {
      a =>
    }
  }
|], NonParallelUsage "y"
  )

nonFreshChanPut :: (Text, SessTyErr)
nonFreshChanPut = (pack [raw|
  proc P(^c: ![ ?[Int] ]) {
    new (^x, ^y) : ?[Int]?[Int];
    ( ^x -> i;  ^c <- ^x;
    | ^y <- 42; ^y <- 42;
    )
  }
|], NonFreshChan "x"
  )

sentChannelConsumed :: (Text, SessTyErr)
sentChannelConsumed = (pack [raw|
  proc P(^c: ![ ?[Int] ]) {
    new (^x, ^y) : ?[Int];
    ( ^c <- ^x;
    // anInt is unused but ^x being used should error first.
    | ^x -> anInt;
    | ^y <- 42;
    )
  }
|], UndefinedChan "x"
  )

-- This test ensures that interferring channels can not be passed as arguments
-- to the same named process. When typing named processes, it's assumed that
-- none of it's arguments interfere. This implicitly enforces dual channels
-- to be composed in parallel.
namedProcWithDualArgs :: (Text, SessTyErr)
namedProcWithDualArgs = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int];
    DualArgProc(^x, ^y)
  }
|], InterferingProcArgs "DualArgProc" $ M.fromList
      [ ("x", S.fromList ["y"])
      , ("y", S.fromList ["x"])
      ]
  )

namedProcInsufficientArgs :: (Text, SessTyErr)
namedProcInsufficientArgs = (pack [raw|
  proc P(a: Int) {
    ProcArgTest(a)
  }
|], InsufficientProcArgs "ProcArgTest"
  )

namedProcWrongChanArg :: (Text, SessTyErr)
namedProcWrongChanArg = (pack [raw|
  proc P(^a: ![Int], ^b: ![Bool]) {
    y = true;
    // Should type check as ProcArgTest(42, y, ^a, ^b)
    ProcArgTest(42, y, ^b, ^a)
  }
|], ProcArgTypeMismatch $ S.fromList ["a3", "a4"]
  )

namedProcWrongExprArg :: (Text, SessTyErr)
namedProcWrongExprArg = (pack [raw|
  proc P(^a: ![Int], ^b: ![Bool]) {
    y = true;
    // Should type check as ProcArgTest(42, y, ^a, ^b)
    ProcArgTest(y, 42, ^a, ^b)
  }
|], ProcArgTypeMismatch $ S.fromList ["a1", "a2"]
  )

namedProcConsume :: Text
namedProcConsume = pack [raw|
  proc P(a: Int, b: Bool, ^x: ![Int], ^y: ![Bool]) {
    ProcArgTest(a, b, ^x, ^y)
  }
|]

parEnvSplit :: (Text, SessTyErr)
parEnvSplit = (pack [raw|
  proc P(^dummy: ![Int]) {
    answer = 42;
    new (^w, ^x) : ![Int];
    new (^y, ^z) : ![Int];
    ( DualArgProc(^w, ^z)
    // w was used by the first process in the composition, DualArgProc, and
    // can no longer be used in the remainder of the composition
    | ^w <- 42;
    // There are other errors but the above is expected to be hit first
    )
  }
|], UndefinedChan "w"
  )

caseMismatchedLabels :: (Text, SessTyErr)
caseMismatchedLabels = (pack [raw|
  proc P(^x: &[a=>, b=>]) {
    ^x case
      { b =>
      , c =>
      }
  }
|], CaseLabelMismatch $ S.fromList ["a", "c"]
  )

-- Check that cases use a channel according to the session choices
checkCaseChannelSession :: Text
checkCaseChannelSession = pack [raw|
  proc P(^x: &[doNothing=>, sendInt=>![Int]]) {
    ^x case
      { doNothing =>
      , sendInt   => ^x <- 42;
      }
  }
|]

dualsUsedInSameParProc :: (Text, SessTyErr)
dualsUsedInSameParProc = (pack [raw|
  proc P(^z: ![Int]) {
    new (^x, ^y) : ![Int];
    ( ^x <- 42; ^y -> a; ^z <- a;
    |
    )
  }
|], ChannelInterference "x" $ S.fromList ["y"]
  )

putChannelInterference :: (Text, SessTyErr)
putChannelInterference = (pack [raw|
  proc P( ^x: ![![Int]]![Int]
        , ^dummy: ![Int]
        )
  {
    new (^yp, ^yn) : ![Int];
    ^x <- ^yp;
    // x and yn need to be used in parallel now according rule T(circle cross)
    ( ^x <- 42;
      ^yn -> z;
      ^dummy <- z;
    |
    )
  }
|], ChannelInterference "x" $ S.fromList ["yn"]
  )

-- Test as above except check that the continuation of the session on which the
-- channel is put is used in a parallel context
putChannelParContinuation :: (Text, SessTyErr)
putChannelParContinuation = (pack [raw|
  proc P( ^x: ![![Int]]![Int]
        , ^dummy: ![Int]
        )
  {
    new (^yp, ^yn) : ![Int];
    ^x <- ^yp;
    // x and yn need to be used in parallel now according rule T(circle cross)
    ^x <- 42;
    ( ^yn -> z;
      ^dummy <- z;
    |
    )
  }
|], NonParallelUsage "x"
  )

nonFreshChanRx :: (Text, SessTyErr)
nonFreshChanRx = (pack [raw|
  proc P(^dummy: ![Int]) {
    new (^xp, ^xn) : ![ ![Int] ];
    new (^yp, ^yn) : ![Int] ;
    new (^zp, ^zn) : ![ ![Int] ];
    ( ^xp <- ^yp;

    // ^xn receives ^yp which is bound to a
    // we can't use yn in this process because it would interfere with xn
    // since xp and yp were used together.
    // A session coming from a channel can not be considered fresh since we
    // don't know how it was used previously.
    // Using a forwarding process here is what is needed for this to type check
    // and would also prevent a deadlock.
    | ^xn -> ^a;
      ^zp <- ^a;

    // ^zn receives ^yp which is bound to b
    // Now we can use yp and yn in sequence causes a deadlock without any of the
    // x, y, or z channel duals being used together directly.
    | ^zn -> b; ^b <- 42; ^yn -> c; ^dummy <- c;
    )
  }
|], NonFreshChan "a"
  )

-- Test that after receiving a channel it doesn't have to be used in parallel
-- See the pi-DILL implication rule or the pi-CLL upside-down ampersand rule.
nonParallelChanRx :: Text
nonParallelChanRx = pack [raw|
  proc P(^a: ?[ ![Int] ] ![Bool]) {
    ^a -> ^rxdChan;
    ^a <- true;
    ^rxdChan <- 42;
  }
|]

-- Check errors in cases. In this test, a has a type error isolated within the
-- case, b has a type error using external resources, c doesn't use a linear
-- resource within the case, d doesn't use an external linear resource, and
-- e has no errors.
-- Cases are type checked before being checked that they use the environment in
-- the same way so the test doesn't need to worry about that.
caseTypeErrors :: (Text, SessTyErr)
caseTypeErrors = (pack [raw|
  proc P( ^x: &[ a =>
              , b =>
              , c =>
              , d =>
              , e =>
              ]
        , ^y: ?[Bool]
        , ^z: ![Int]
        )
  {
    new (^a, ^b) : ![Int]![Int];
    ( ^x case
        { a => new (^c, ^d) : ![Int];
               // Note that the channel z and the base variable z don't conflict
               // because we know that the value from y isn't a channel
               // It might be a good idea to change this since it's confusing.
               ( ^y -> z; ^d <- z; // ^d is used incorrectly
               | ^b -> w; ^b -> ww; ^c <- w; ^z <- ww;
               )
        , b => ^z <- true;
        , c => new (^c, ^d) : ![Int];
               ( ^c <- 42;
               | ^z <- 42;
               )
        , d => ^b -> w; ^z <- w; // b is only partly used
        , e =>
        }
    | ^a <- 42; ^a <- 42;
    )
  }
|], CaseProcErrs $ M.fromList [ ("a", ProtocolMismatch "d" $ SGetTy TInt SEnd)
                              , ("b", TypeMismatch "z" TBool)
                              , ("c", UnusedResources
                                        { unusedLin=M.fromList [("d", SGetTy TInt SEnd)]
                                        , unusedSeq=M.empty
                                        }
                                )
                              , ("d", UnusedResources
                                        { unusedLin=M.fromList [("b", SGetTy TInt SEnd)]
                                        , unusedSeq=M.empty
                                        }
                                )
                              ]
  )

casesWithDifferingUsages :: (Text, SessTyErr)
casesWithDifferingUsages = (pack [raw|
  proc P( ^c: &[ a =>
              , b =>
              ]
        , ^dummy: ![Int]
        )
  {
    new (^x, ^y) : ![Int]?[Int];
    ( ^c case
        { a => ^x <- 42; ^x -> z; ^dummy <- z;
        , b =>
        }
    | SomeProc(^y)
    | // should we be able to use x here or not? case a consumed x but
           // case b didn't
    )
  }
|], CasesUseDifferentLinearResources
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, the following is typable in Gay and Hole's system but not
-- pi-DILL. Note no deadlock.
ex1 :: (Text, SessTyErr)
ex1 = (pack [raw|
  proc P(^dummy: ![Int]) {
    new (^xp, ^xn) : ![Int];
    new (^yp, ^yn) : ![Int];
    ( ^xp <- 42; ^yp <- 84;
    | ^xn -> a;  ^yn -> b; ^dummy <- a+b;
    )
  }
|], ChannelInterference "xn" $ S.fromList ["yn"]
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, the following is typable in Gay and Hole's system but not
-- pi-DILL. Note that this time there is a deadlock because the act of sending
-- a value on x blocks until the value can be received. Also note that a send
-- only block in a synchronous model.
ex2 :: (Text, SessTyErr)
ex2 = (pack [raw|
  proc P(^dummy: ![Int]) {
    new (^xp, ^xn) : ![Int];
    new (^yp, ^yn) : ![Int];
    ( ^xp <- 42; ^yp <- 84;
    | ^yn -> b;  ^xn -> a; ^dummy <- a+b;
    )
  }
|], ChannelInterference "xn" $ S.fromList ["yn"]
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, this is essentially ex1 in a "globally coordinated" form that
-- is typable in their system.
ex3 :: Text
ex3 = pack [raw|
  proc P(^dummy: ![Int]) {
    new (^nxy_p, ^nxy_n) : ![Int]![Int];
    ( ^nxy_p <- 41; ^nxy_p <- 42;
    | ^nxy_n -> x;  ^nxy_n -> y;  ^dummy <- x+y;
    )
  }
|]

