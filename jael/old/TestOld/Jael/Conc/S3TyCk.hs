module Test.Jael.Conc.S3TyCk
( procS3TyCkTests
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Conc.TyCk.S2 (s1ProcToS2Proc)
import           Jael.Conc.TyCk.S3
import           Jael.Grammar
import           Jael.Parser
import           Jael.Seq.AST
import           Jael.Seq.Env
import           Jael.Seq.Types
import           Jael.Seq.TI.S2 (S2TypeErr)
import qualified Test.Framework as T

procS3TyCkTests :: [T.Test]
procS3TyCkTests =
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
  , testCase "forward chan to avoid non-fresh" $ shouldTyCk chanFwdToMakeFresh
  , testCase "forward recursive channel" $ shouldTyCk coRecFwd
  , testCase "sent channel consumed" $ checkTyCkErr sentChannelConsumed
  , testCase "received channel non parallel usage" $ shouldTyCk nonParallelChanRx
  , testCase "trying to ignore rxd session should fail" $ checkTyCkErr ignoreSession
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
  , testCase "interfering corecursive arguments" $ checkTyCkErr interferingCoRecArgs
  , testCase "undefined co-rec channel arg" $ checkTyCkErr undefinedCoRecArgChan
  , testCase "undefined co-rec expr arg" $ checkTyCkErr undefinedCoRecArgExpr
  -- same as above three tests but for the corecursive variable instead of definition
  , testCase "interfering corecursive variable arguments" $ checkTyCkErr interferingCoRecVarArgs
  , testCase "rec-var undefined channel arg" $ checkTyCkErr coRecVariableArgUndefChan
  , testCase "rec-var undefined expr arg" $ checkTyCkErr coRecVariableArgUndefExpr
  , testCase "rec-var argument type mismatch" $ checkTyCkErr coRecVariableArgTypeMismatch
  , testCase "test residual environment with corecursive definition" $ checkTyCkErr coRecResidualEnv
  , testCase "test inductive sessions equal under renaming" $ shouldTyCk equalityOfCoinductiveSessions
  , testCase "test inductive sessions equal under renaming (2)" $ shouldTyCk equalityOfCoinductiveSessions2
  , testCase "unfolded session as argument" $ checkTyCkErr unfoldedInductiveArgumentToRecProc
  , testCase "inductive session requires 'use' behaviour (1)" $ checkTyCkErr indSessUseReqd1
  , testCase "inductive session requires 'use' behaviour (2)" $ checkTyCkErr indSessUseReqd2
  , testCase "inductive session requires 'use' behaviour (3)" $ checkTyCkErr indSessUseReqd3
  , testCase "inductive session requires 'impl' behaviour (1)" $ checkTyCkErr indSessImplReqd1
  , testCase "inductive session requires 'impl' behaviour (2)" $ checkTyCkErr indSessImplReqd2
  -- The remainder of these tests are specific examples from papers. See the
  -- inline comments for more details
  , testCase "example 1" $ checkTyCkErr ex1
  , testCase "example 2" $ checkTyCkErr ex2
  , testCase "example 3" $ shouldTyCk ex3
  , testCase "example 4" $ checkTyCkErr ex4
  , testCase "example 5" $ shouldTyCk ex5
  ]

-- A map of aliases that will be passed to the type checking function
testAliases :: M.Map Text Session
testAliases =
  M.map (
    \t -> case runParser pGSession t of
               Left err -> error (unpack err)
               Right g -> case gToSession g of
                               Left err -> error (show err)
                               Right s -> s
  ) $ M.fromList
        [ ("GetBool", "?[Bool]")
        , ("PutBool", "![Bool]")
        , ("AltTxRxBool", "rec X. ![Bool] ?[Bool] <X>")
        ]

-- A map of process names to their arguments that will be passed to the type
-- checking function
testProcs :: M.Map Text [(Text, S2TyOrSess)]
testProcs =
  M.map (map $
    \t -> case runParser pGProcArg t of
               Left err -> error (unpack err)
               Right g -> case gToProcArg g of
                               Left err -> error (show err)
                               Right (n, TorSSess s) -> (n, TorSSess s)
                               Right (n, TorSTy s1t) -> case s1TypeToS2Type s1t of
                                                             Just s2t -> (n, TorSTy s2t)
                                                             Nothing -> error "Incomplete type"
    ) $ M.fromList
        [ ("DualArgProc", ["^arg1: ![Bool]", "^arg2: ?[Bool]"])
        , ("ProcArgTest", ["a1: Int<0,42>", "a2:Bool", "^a3: ![Int<0,42>]", "^a4: ![Bool]"])
        , ("ProcsUseRecSessPos", ["^a: rec X. ![Bool] <X>"])
        , ("ProcsUseRecSessNeg", ["^a: rec X. ?[Bool] <X>"])
        , ("CoIndEqY", ["^a: rec Y. ![Bool] <Y>"])
        ]

doTyCk :: Text -> Maybe SessTyErr
doTyCk t = tyCheckTopProc defaultEnv testAliases testProcs
             (case s1ProcToS2Proc defaultEnv
                  . parseTopProc
                  . pGTopDef
                  . myLexer
                  . unpack $ t of
                  Left err -> error (show err)
                  Right (tp, _) -> tp
             )

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

parseTopProc :: Err GTopDef -> S1TopProc
parseTopProc (Ok (GTopDefGProcDef (GProcDef _ as p))) =
  case gToTopProc (as, p) of
       Left err -> error (show err)
       Right x -> x
parseTopProc x = error $ "Not a TopProc definition or invalid syntax:\n" ++ show x

emptyProc :: Text
emptyProc = pack [raw|
  proc X(){ }
|]

unusedLinearArg :: (Text, SessTyErr)
unusedLinearArg = (pack [raw|
  proc X(^x: ?[Bool]){ }
|], UnusedResources{ unusedLin=M.fromList [("x", SGetTy (S2TySimple undefined) SEnd)]
                   , unusedSeq=M.empty
                   }
  )

sessionAlias :: Text
sessionAlias = pack [raw|
  proc X(^x: ![Bool] <GetBool>, ^y: ![Bool]) {
    // Put an int on x and then get an int on x.
    ^x <- true;
    ^x -> z;
    ^y <- z; // Use z so we don't get an unused sequential variable error
  }
|]

channelPut :: Text
channelPut = pack [raw|
  proc X(^x: ![Bool]) {
    ^x <- true;
  }
|]

unusedSeqVars :: (Text, SessTyErr)
unusedSeqVars = (pack [raw|
  proc X(^x: ?[Bool], y: Bool) {
    ^x -> z;
  }
|], UnusedResources{ unusedSeq=M.fromList [("y", (S2TySimple BTBool)), ("z", (S2TySimple undefined))]
                   , unusedLin=M.empty
                   }
  )

seqUsedInExpr :: Text
seqUsedInExpr = pack [raw|
  proc X(x: Bool, ^y: ?[Bool], ^z: ![Bool]) {
    ^y -> a;
    ^z <- if x { a } else { true };
  }
|]

-- Test that y being unused is an error. <GetBool> is an alias that is resolved
-- so y is reported not as the alias but it's actual session type
channelGetUnusedLin :: (Text, SessTyErr)
channelGetUnusedLin = (pack [raw|
  proc X(^x: ?[<GetBool>]) {
    ^x -> ^y;
  }
|], UnusedResources{ unusedLin=M.fromList [("y", SGetTy (S2TySimple undefined) SEnd)]
                   , unusedSeq=M.empty
                   }
  )

-- Test that a recursive session definition is unfolded when defined and used
-- This tests two cases, the first when the recursive session is not used at
-- all and the second when the recursive definition needs its variable unfolded
recDefUnfold :: (Text, SessTyErr)
recDefUnfold = (pack [raw|
  proc P(^x: <AltTxRxBool>, ^y: <AltTxRxBool>) {
    ^x <- true;
    ^x -> z;
  }
|], UnusedResources{ unusedLin=M.fromList
                      [ ("x", SCoInd "X" $ SPutTy (S2TySimple undefined) $ SGetTy (S2TySimple undefined) $ SVar "X")
                      , ("y", SCoInd "X" $ SPutTy (S2TySimple undefined) $ SGetTy (S2TySimple undefined) $ SVar "X")
                      ]
                   , unusedSeq=M.fromList [("z", (S2TySimple undefined))]
                   }
  )

-- The session AltTxRxBool is defined with a recursion variable X. By defining
-- a session that uses that alias and defines its own recursion variable with
-- the same name, and substitution of AltTxRxBool would create an ambiguity.
-- This test checks that the X in AltTxRxBool is renamed such that there is no
-- ambiguity.
reusedRecVarInAlias :: (Text, SessTyErr)
reusedRecVarInAlias = (pack [raw|
  proc P( ^x: rec X. +[a => <X>, b => <AltTxRxBool>]
        , ^y: rec X. +[a => <X>, b => <AltTxRxBool>]
        )
  {
    ^x select b;
    ^x <- true;
    ^x -> z;
    ^y select a;
  }
|], UnusedResources{ unusedLin=M.fromList
                      [ ("x", SCoInd "X" $ SPutTy (S2TySimple undefined) $ SGetTy (S2TySimple undefined) $ SVar "X")
                      , ("y", SCoInd "X" $ SSelect [ ("a", SVar "X")
                                                   , ("b", SVar "AltTxRxBool")
                                                   ]
                        )
                      ]
                   , unusedSeq=M.fromList [("z", (S2TySimple undefined))]
                   }
  )

badLabel :: (Text, SessTyErr)
badLabel = (pack [raw|
  proc P( ^x: +[a => , b => ![Bool]])
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
    new (^x, ^y) : ![Bool];
    // Expecting an error here before an unused linear resources error.
    ^y -> z;
  }
|], NonParallelUsage "y"
  )

nonParPut :: (Text, SessTyErr)
nonParPut = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Bool];
    ^x <- true;
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
  proc P(^c: ![ ?[Bool] ]) {
    new (^x, ^y) : ?[Bool]?[Bool];
    ( ^x -> i;  ^c <- ^x;
    | ^y <- true; ^y <- true;
    )
  }
|], NonFreshChan "x"
  )

sentChannelConsumed :: (Text, SessTyErr)
sentChannelConsumed = (pack [raw|
  proc P(^c: ![ ?[Bool] ]) {
    new (^x, ^y) : ?[Bool];
    ( ^c <- ^x;
    // aBool is unused but ^x being used should error first.
    | ^x -> aBool;
    | ^y <- true;
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
    new (^x, ^y) : ![Bool];
    DualArgProc(^x, ^y)
  }
|], InterferingProcArgs "DualArgProc" $ M.fromList
      [ ("x", S.fromList ["y"])
      , ("y", S.fromList ["x"])
      ]
  )

namedProcInsufficientArgs :: (Text, SessTyErr)
namedProcInsufficientArgs = (pack [raw|
  proc P(a: Bool) {
    ProcArgTest(a)
  }
|], InsufficientProcArgs "ProcArgTest"
  )

namedProcWrongChanArg :: (Text, SessTyErr)
namedProcWrongChanArg = (pack [raw|
  proc P(^a: ![Int<0,42>], ^b: ![Bool]) {
    y = true;
    // Should type check as ProcArgTest(42, y, ^a, ^b)
    ProcArgTest(42, y, ^b, ^a)
  }
|], ProcArgTypeMismatch $ S.fromList ["a3", "a4"]
  )

namedProcWrongExprArg :: (Text, SessTyErr)
namedProcWrongExprArg = (pack [raw|
  proc P(^a: ![Int<0,42>], ^b: ![Bool]) {
    y = true;
    // Should type check as ProcArgTest(42, y, ^a, ^b)
    ProcArgTest(y, 42, ^a, ^b)
  }
|], ProcArgTypeMismatch $ S.fromList ["a1", "a2"]
  )

namedProcConsume :: Text
namedProcConsume = pack [raw|
  proc P(a: Int<0,42>, b: Bool, ^x: ![Int<0,42>], ^y: ![Bool]) {
    ProcArgTest(a, b, ^x, ^y)
  }
|]

parEnvSplit :: (Text, SessTyErr)
parEnvSplit = (pack [raw|
  proc P() {
    answer = 42;
    new (^w, ^x) : ![Bool];
    new (^y, ^z) : ![Bool];
    ( DualArgProc(^w, ^z)
    // w was used by the first process in the composition, DualArgProc, and
    // can no longer be used in the remainder of the composition
    | ^w <- true;
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
  proc P(^x: &[doNothing=>, sendBool => ![Bool]]) {
    ^x case
      { doNothing =>
      , sendBool  => ^x <- true;
      }
  }
|]

dualsUsedInSameParProc :: (Text, SessTyErr)
dualsUsedInSameParProc = (pack [raw|
  proc P(^z: ![Bool]) {
    new (^x, ^y) : ![Bool];
    ( ^x <- true; ^y -> a; ^z <- a;
    |
    )
  }
|], ChannelInterference "x" $ S.fromList ["y"]
  )

putChannelInterference :: (Text, SessTyErr)
putChannelInterference = (pack [raw|
  proc P( ^x: ![![Bool]]![Bool]
        )
  {
    new (^yp, ^yn) : ![Bool];
    ^x <- ^yp;
    // x and yn need to be used in parallel now according rule T(circle cross)
    ( ^x <- true;
      ^yn -> _;
    |
    )
  }
|], ChannelInterference "x" $ S.fromList ["yn"]
  )

-- Test as above except check that the continuation of the session on which the
-- channel is put is used in a parallel context
putChannelParContinuation :: (Text, SessTyErr)
putChannelParContinuation = (pack [raw|
  proc P( ^x: ![![Bool]]![Bool]
        )
  {
    new (^yp, ^yn) : ![Bool];
    ^x <- ^yp;
    // x and yn need to be used in parallel now according rule T(circle cross)
    ^x <- true;
    ( ^yn -> _;
    |
    )
  }
|], NonParallelUsage "x"
  )

nonFreshChanRx :: (Text, SessTyErr)
nonFreshChanRx = (pack [raw|
  proc P() {
    new (^xp, ^xn) : ![ ![Bool] ];
    new (^yp, ^yn) : ![Bool] ;
    new (^zp, ^zn) : ![ ![Bool] ];
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
    | ^zn -> ^b; ^b <- true; ^yn -> _;
    )
  }
|], NonFreshChan "a"
  )

-- Similar to nonFreshChanRx, except this process will type check because it
-- forwards a to a fresh channel that can be sent over zp.
chanFwdToMakeFresh :: Text
chanFwdToMakeFresh = pack [raw|
  proc P() {
    new (^xp, ^xn) : ![ ![Bool] ];
    new (^yp, ^yn) : ![Bool];
    new (^zp, ^zn) : ![ ![Bool] ];
    ( ^xp <- ^yp;

    // ^xn receives ^yp which is bound to a
    // we can't use yn in this process because it would interfere with xn
    // since xp and yp were used together.
    // Using a forwarding process here allows us to send a fresh channel with
    // the same behaviour as a on zp.
    | ^xn -> ^a;
      new (^fwdAp, ^fwdAn) : ![Bool];
      ^zp <- ^fwdAp;
      ( ^a <-> ^fwdAn
      |
      )

    // ^zn receives ^yp which is bound to b
    // Now we can use yp and yn in sequence.
    // Note however that there is no deadlock because yp and yn aren't being
    // used directly but through the forwarding process which will first receive
    // "true" and immidiately send it on a (which is yp).
    | ^zn -> ^b; ^b <- true; ^yn -> _;
    )
  }
|]

-- Try to forward co-recusive sessions
-- Added this test because I suspect that the way I unfolded session aliases
-- would cause a problem. Basically, the fact that an alias refers to a
-- corecurisve session is important and can't be lost during unfolding.
coRecFwd :: Text
coRecFwd = pack [raw|
  proc P( ^x: rec X. ![Bool] <X>
        , ^y: rec Y. ?[Bool] <Y>
        )
  {
    ^x <-> ^y
  }
|]

-- Test that after receiving a channel it doesn't have to be used in parallel
-- See the pi-DILL implication rule or the pi-CLL upside-down ampersand rule.
nonParallelChanRx :: Text
nonParallelChanRx = pack [raw|
  proc P(^a: ?[ ![Bool] ] ![Bool]) {
    ^a -> ^rxdChan;
    ^a <- true;
    ^rxdChan <- true;
  }
|]

ignoreSession :: (Text, SessTyErr)
ignoreSession = (pack [raw|
  proc P(^c: ?[]) {
    ^c -> _;
  }
|], AttemptedChannelIgnore "c" SEnd
  )

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
        , ^z: ![Bool]
        )
  {
    new (^a, ^b) : ![Bool]![Bool];
    ( ^x case
        { a => new (^c, ^d) : ![Bool];
               ( ^y -> w; ^d <- w; // ^d is used incorrectly
               | ^b -> w; ^b -> ww; ^c <- w; ^z <- ww;
               )
        , b => ^z <- true;
        , c => new (^c, ^d) : ![Bool];
               ( ^c <- true;
               | ^z <- true;
               )
        , d => ^b -> w; ^z <- w; // b is only partly used
        , e =>
        }
    | ^a <- true; ^a <- true;
    )
  }
|], CaseProcErrs $ M.fromList [ ("a", ProtocolMismatch "d" $ SGetTy (S2TySimple undefined) SEnd)
                              , ("b", TypeMismatch "z" (S2TySimple undefined) (S2TySimple BTBool))
                              , ("c", UnusedResources
                                        { unusedLin=M.fromList [("d", SGetTy (S2TySimple undefined) SEnd)]
                                        , unusedSeq=M.empty
                                        }
                                )
                              , ("d", UnusedResources
                                        { unusedLin=M.fromList [("b", SGetTy (S2TySimple undefined) SEnd)]
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
        )
  {
    new (^x, ^y) : ![Bool]?[Bool];
    ( ^c case
        { a => ^x <- true; ^x -> _;
        , b =>
        }
    | SomeProc(^y)
    | // should we be able to use x here or not? case a consumed x but
           // case b didn't
    )
  }
|], CasesUseDifferentLinearResources
  )

undefinedCoRecArgChan :: (Text, SessTyErr)
undefinedCoRecArgChan = (pack [raw|
  proc P(^a : ![Bool]) {
    rec X(^a=^a, ^b=^b) {
      ^a <- true;
      ^b <- false;
    }
  }
|], UndefinedChan "b"
  )

undefinedCoRecArgExpr :: (Text, SessTyErr)
undefinedCoRecArgExpr = (pack [raw|
  proc P(^a : ![Bool]) {
    rec X(^a=^a, b=y) {
      ^a <- b;
    }
  }
|], undefined
  )

interferingCoRecArgs :: (Text, SessTyErr)
interferingCoRecArgs = (pack [raw|
  proc P() {
    new (^a, ^b) : ![Bool];
    rec X(^c=^a, ^d=^b) {
      ^c <- true;
      ^d <- false;
    }
  }
|], InterferingProcArgs "X" $ M.fromList
      [ ("a", S.fromList ["b"])
      , ("b", S.fromList ["a"])
      ]
  )

coRecVariableArgUndefChan :: (Text, SessTyErr)
coRecVariableArgUndefChan = (pack [raw|
  proc P(^a : ![Bool]) {
    rec X(^a=^a) {
      X(^b)
    }
  }
|], UndefinedChan "b"
  )

coRecVariableArgUndefExpr :: (Text, SessTyErr)
coRecVariableArgUndefExpr = (pack [raw|
  proc P(a : Bool) {
    rec X(a=a) {
      X(b)
    }
  }
|], undefined
  )

coRecVariableArgTypeMismatch :: (Text, SessTyErr)
coRecVariableArgTypeMismatch = (pack [raw|
  proc P(a : Bool) {
    rec X(a=a) {
      X(true)
    }
  }
|], ProcArgTypeMismatch (S.fromList ["a"])
  )

interferingCoRecVarArgs :: (Text, SessTyErr)
interferingCoRecVarArgs = (pack [raw|
  proc P(^a: ![Bool], ^b: ?[Bool]) {
    rec X(^a=^a, ^b=^b) {
      new (^c, ^d) : ![Bool];
      X(^c, ^d)
    }
  }
|], InterferingProcArgs "X" $ M.fromList
      [ ("c", S.fromList ["d"])
      , ("d", S.fromList ["c"])
      ]
  )

coRecResidualEnv :: (Text, SessTyErr)
coRecResidualEnv = (pack [raw|
  proc P() {
    new (^a, ^b) : rec X. ![Bool] <X>;
    ( rec X(^b=^b) {
        ^b -> _;
        X(^b)
      }
    |
    )
  }
|], UnusedResources
      { unusedLin=M.fromList [("a", SCoInd "X" $ SPutTy (S2TySimple undefined) $ SVar "X")]
      , unusedSeq=M.empty
      }
  )

-- should be equal under renaming of the induction variable
equalityOfCoinductiveSessions :: Text
equalityOfCoinductiveSessions = pack [raw|
  proc P( ^x: ![ rec X. ?[Bool] <X> ]
        )
  {
    new (^a, ^b) : rec Y. ![Bool] <Y>;
    ^x <- ^b;
    ( rec X(^a=^a) {
        ^a <- true;
        X(^a)
      }
    |
    )
  }
|]

equalityOfCoinductiveSessions2 :: Text
equalityOfCoinductiveSessions2 = pack [raw|
  proc P(^a: rec X. ![Bool] <X>)
  {
    // This process is defined to take a recursive session matching that of
    // ^a but differing in the variable name.
    CoIndEqY(^a)
  }
|]

unfoldedInductiveArgumentToRecProc :: (Text, SessTyErr)
unfoldedInductiveArgumentToRecProc = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Bool] rec X. ![Bool]?[Bool] <X>;
    ( rec X(^x=^x) {
      }
    |
    )
  }
|], NonPrimaryIndSessArg "x"
  )

-- If a recursive session is sent over a channel or passed to a process, it must
-- be "used" (left rule), and can't implement recursive behaviour in a recursive
-- process (right rule).
indSessUseReqd1 :: (Text, SessTyErr)
indSessUseReqd1 = (pack [raw|
  proc P(^x: rec X. ![Bool] <X>) {
    // x can't be used as an argument because we have no idea how its dual is
    // being used
    rec X(^x=^x) {
    }
  }
|], IndSessUseReqd "x"
  )

-- As above but for received sessions
indSessUseReqd2 :: (Text, SessTyErr)
indSessUseReqd2 = (pack [raw|
  proc P(^x: ?[rec X. ![Bool] <X>]) {
    ^x -> ^y;
    // tries to implement behaviour on y instead of using it
    rec X(^y=^y) {
    }
  }
|], IndSessUseReqd "y"
  )

-- As above but for sessions where the other end already implements behaviour
indSessUseReqd3 :: (Text, SessTyErr)
indSessUseReqd3 = (pack [raw|
  proc P() {
    new (^x, ^y): rec X. ![Bool] <X>;
    ( rec X(^x=^x) {
        ^x <- true;
        X(^x)
      }
    | rec X(^y=^y) {
      }
    )
  }
|], IndSessUseReqd "y"
  )

-- The next three tests test are as the previous but test the errors when the
-- implementation behaviour is required.
indSessImplReqd1 :: (Text, SessTyErr)
indSessImplReqd1 = (pack [raw|
  proc P( ^sendRecPos: ![rec X. ![Bool] <X>]
        , ^sendRecNeg: ![rec X. ?[Bool] <X>]
        )
  {
    new (^x, ^y): rec X. ![Bool] <X>;
    ^sendRecPos <- ^x;
    ( ProcsUseRecSessNeg(^y)
    |
    )
  }
|], IndSessImplReqd "y"
  )

indSessImplReqd2 :: (Text, SessTyErr)
indSessImplReqd2 = (pack [raw|
  proc P()
  {
    new (^x, ^y): rec X. ![Bool] <X>;
    ( ProcsUseRecSessPos(^x)
    | ProcsUseRecSessNeg(^y)
    )
  }
|], IndSessImplReqd "y"
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, the following is typable in Gay and Hole's system but not
-- pi-DILL. Note no deadlock.
ex1 :: (Text, SessTyErr)
ex1 = (pack [raw|
  proc P() {
    new (^xp, ^xn) : ![Bool];
    new (^yp, ^yn) : ![Bool];
    ( ^xp <- true; ^yp <- false;
    | ^xn -> _;  ^yn -> _;
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
  proc P() {
    new (^xp, ^xn) : ![Bool];
    new (^yp, ^yn) : ![Bool];
    ( ^xp <- true; ^yp <- false;
    | ^yn -> _;  ^xn -> _;
    )
  }
|], ChannelInterference "xn" $ S.fromList ["yn"]
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, this is essentially ex1 in a "globally coordinated" form that
-- is typable in their system.
ex3 :: Text
ex3 = pack [raw|
  proc P() {
    new (^nxy_p, ^nxy_n) : ![Bool]![Bool];
    ( ^nxy_p <- true; ^nxy_p <- false;
    | ^nxy_n -> _;  ^nxy_n -> _;
    )
  }
|]

-- Produces an infinite <internal> reduction sequence, i.e.: not useful
-- From Corecursion and non-divergence in session-typed processes, page 161
ex4 :: (Text, SessTyErr)
ex4 = (pack [raw|
  proc Loop(^useDual: ![rec X. ![Bool] <X>]) {
    new (^c, ^cDual): rec X. ?[Bool] <X>;
    ^useDual <- ^cDual;
    ( rec L(^c=^c) {
        ^c -> _;
        new (^dp, ^dn): rec X. ?[Bool] <X>;
        ( L(^dp)
        | ^dn <- true;
          ^dn <-> ^c
        )
      }
    |
    )
  }
|], RecVarUnfoldInRecProc "dn"
  )

-- Infinitely produces the sequence true, false, true, false, ... on the channel c
-- From Corecursion and non-divergence in session-typed processes, page 161
-- (with modification)
-- The paper does not explicitly talk about typing this process, but based on
-- its definition, ^c has to have a different type than in example 4 for it to
-- type since ^c can only be used up to it's recursion variable.
ex5 :: Text
ex5 = pack [raw|
  proc Good(^useDual: ![rec X. ?[Bool]?[Bool] <X>]) {
      new (^c, ^cDual) : rec X. ![Bool]![Bool] <X>;
      ^useDual <- ^cDual;
      ( rec G(^c=^c) {
          ^c <- true;
          new (^dp, ^dn) : rec X. ![Bool]![Bool] <X>;
          ( G(^dp)
          | ^c <- false;
            ^c <-> ^dn
          )
        }
      |
      )
  }
|]

