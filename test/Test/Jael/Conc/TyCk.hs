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
  , testCase "named proc passed duals" $ checkTyCkErr namedProcWithDualArgs
  , testCase "case does not implement correct labels" $ checkTyCkErr caseMismatchedLabels
  , testCase "check channel used properly after case" $ shouldTyCk checkCaseChannelSession
  , testCase "duals used in same parallel proc" $ checkTyCkErr dualsUsedInSameParProc
  , testCase "put channel interference" $ checkTyCkErr putChannelInterference
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
        [ ("GetInt", "?[Int];")
        , ("PutInt", "![Int];")
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
        [ ("DualArgProc", ["arg1: ![Int];", "arg2: ?[Int];"])
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
emptyProc = (pack [raw|
  proc X(){ done }
|])

unusedLinearArg :: (Text, SessTyErr)
unusedLinearArg = (pack [raw|
  proc X(x: ?[Int];){ done }
|], UnusedResources{ unusedLin=M.fromList [("x", SGetTy TInt SEnd)]
                   , unusedSeq=M.empty
                   }
  )

sessionAlias :: Text
sessionAlias = (pack [raw|
  proc X(x: ![Int] <GetInt>, y: ![Int];) {
    // Put an int on x and then get an int on x.
    ^x <- 5;
    ^x -> z;
    ^y <- z; // Use z so we don't get an unused sequential variable error
    done
  }
|])

channelPut :: Text
channelPut = (pack [raw|
  proc X(x: ![Int];) {
    ^x <- 42;
    done
  }
|])

unusedSeqVars :: (Text, SessTyErr)
unusedSeqVars = (pack [raw|
  proc X(x: ?[Int];, y: Bool) {
    ^x -> z;
    done
  }
|], UnusedResources{ unusedSeq=M.fromList [("y", TBool), ("z", TInt)]
                   , unusedLin=M.empty
                   }
  )

seqUsedInExpr :: Text
seqUsedInExpr = (pack [raw|
  proc X(x: Bool, y: ?[Int];, z: ![Int];) {
    ^y -> a;
    ^z <- if x { a } else { 42 };
    done
  }
|])

-- Test that y being unused is an error. <GetInt> is an alias that is resolved
-- so y is reported not as the alias but it's actual session type
channelGetUnusedLin :: (Text, SessTyErr)
channelGetUnusedLin = (pack [raw|
  proc X(x: ?[<GetInt>];) {
    ^x -> y;
    done
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
  proc P(x: <AltTxRxInt>, y: <AltTxRxInt>) {
    ^x <- 42;
    ^x -> z;
    done
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
  proc P( x: rec X. +[a => <X>, b => <AltTxRxInt>]
        , y: rec X. +[a => <X>, b => <AltTxRxInt>]
        )
  {
    ^x select b;
    ^x <- 42;
    ^x -> z;
    ^y select a;
    done
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
  proc P( x: +[a => ;, b => ![Int];])
  {
    ^x select c;
    done
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
    new (^x, ^y) : ![Int]; ;
    // Expecting an error here before an unused linear resources error.
    ^y -> z;
    done
  }
|], NonParallelUsage "y"
  )

nonParPut :: (Text, SessTyErr)
nonParPut = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int]; ;
    ^x <- 42;
    done
  }
|], NonParallelUsage "x"
  )

nonParSel :: (Text, SessTyErr)
nonParSel = (pack [raw|
  proc P() {
    new (^x, ^y) : +[a => ;];
    ^x select a;
    done
  }
|], NonParallelUsage "x"
  )

nonParCho :: (Text, SessTyErr)
nonParCho = (pack [raw|
  proc P() {
    new (^x, ^y) : +[a => ;];
    ^y case {
      a => done
    }
  }
|], NonParallelUsage "y"
  )

nonFreshChanPut :: (Text, SessTyErr)
nonFreshChanPut = (pack [raw|
  proc P(c: ![ ?[Int]; ];) {
    new (^x, ^y) : ?[Int]?[Int];;
    ( ^x -> i;  ^c <- ^x; done
    | ^y <- 42; ^y <- 42; done
    )
  }
|], NonFreshChan "x"
  )

-- This test ensures that interferring channels can not be passed as arguments
-- to the same named process. When typing named processes, it's assumed that
-- none of it's arguments interfere. This implicitly enforces dual channels
-- to be composed in parallel.
namedProcWithDualArgs :: (Text, SessTyErr)
namedProcWithDualArgs = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int]; ;
    DualArgProc(^x, ^y)
  }
|], InterferringProcArgs "DualArgProc" $ S.fromList ["x", "y"]
  )

caseMismatchedLabels :: (Text, SessTyErr)
caseMismatchedLabels = (pack [raw|
  proc P(x: &[a=>;, b=>;]) {
    ^x case
      { b => done
      , c => done
      }
  }
|], CaseLabelMismatch $ S.fromList ["a", "c"]
  )

-- Check that cases use a channel according to the session choices
checkCaseChannelSession :: Text
checkCaseChannelSession = (pack [raw|
  proc P(x: &[doNothing=>;, sendInt=>![Int];]) {
    ^x case
      { doNothing => done
      , sendInt   => ^x <- 42; done
      }
  }
|])

dualsUsedInSameParProc :: (Text, SessTyErr)
dualsUsedInSameParProc = (pack [raw|
  proc P(z: ![Int];) {
    new (^x, ^y) : ![Int]; ;
    ( ^x <- 42; ^y -> a; ^z <- a; done
    | done
    )
  }
|], ChannelInterference "x" $ S.fromList ["y"]
  )

putChannelInterference :: (Text, SessTyErr)
putChannelInterference = (pack [raw|
  proc P(dummy: ![Int];) {
    new (^xp, ^xn) : ![ ![Int]; ]; ;
    new (^yp, ^yn) : ![Int] ; ;
    new (^zp, ^zn) : ![ ![Int]; ]; ;
    ( ^xp <- ^yp; done

    // ^xn receives ^yp which is bound to a
    // we can't use yn in this process because it would interfere with xn
    // since xp and yp were used together.
    | ^xn -> a; ^zp <- a; done

    // ^zn receives ^yp which is bound to b
    // Now we can use yp and yn in sequence causes a deadlock without any of the
    // x, y, or z channel duals being used together directly.
    | ^zn -> b; ^b <- 42; ^yn -> c; ^dummy <- c; done
    )
  }
|], ChannelInterference "xp" $ S.fromList ["yp"]
  )

-- Check errors in cases. In this test, a has a type error isolated within the
-- case, b has a type error using external resources, c doesn't use a linear
-- resource within the case, d doesn't use an external linear resource, and
-- e has no errors.
-- Cases are type checked before being checked that they use the environment in
-- the same way so the test doesn't need to worry about that.
caseTypeErrors :: (Text, SessTyErr)
caseTypeErrors = (pack [raw|
  proc P( x: &[ a => ;
              , b => ;
              , c => ;
              , d => ;
              , e => ;
              ]
        , y: ?[Bool];
        , z: ![Int];
        )
  {
    new (^a, ^b) : ![Int]![Int];;
    ( ^x case
        { a => new (^c, ^d) : ![Int];;
               // Note that the channel z and the base variable z don't conflict
               // because we know that the value from y isn't a channel
               // It might be a good idea to change this since it's confusing.
               ( ^y -> z; ^d <- z; done // ^d is used incorrectly
               | ^b -> w; ^b -> ww; ^c <- w; ^z <- ww; done
               )
        , b => ^z <- true; done
        , c => new (^c, ^d) : ![Int];;
               ( ^c <- 42; done
               | ^z <- 42; done
               )
        , d => ^b -> w; ^z <- w; done // b is only partly used
        , e => done
        }
    | ^a <- 42; ^a <- 42; done
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
  proc P( c: &[ a => ;
              , b => ;
              ]
        , dummy: ![Int];
        )
  {
    new (^x, ^y) : ![Int]?[Int];;
    ( ^c case
        { a => ^x <- 42; ^x -> z; ^dummy <- z; done
        , b => done
        }
    | SomeProc(^y)
    | done // should we be able to use x here or not? case a consumed x but
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
  proc P() {
    new (^xp, ^xn) : ![Int];;
    new (^yp, ^yn) : ![Int];;
    ( ^xp <- 42; ^yp <- 84; done
    | ^xn -> a;  ^yn -> b;  done
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
    new (^xp, ^xn) : ![Int];;
    new (^yp, ^yn) : ![Int];;
    ( ^xp <- 42; ^yp <- 84; done
    | ^yn -> b;  ^xn -> a;  done
    )
  }
|], ChannelInterference "xn" $ S.fromList ["yn"]
  )

-- L. Caires et al. Linear logic propositions as session types, 2014
-- From page 23, this is essentially ex1 in a "globally coordinated" form that
-- is typable in their system.
ex3 :: Text
ex3 = (pack [raw|
  proc P(dummy: ![Int];) {
    new (^nxy_p, ^nxy_n) : ![Int]![Int];;
    ( ^nxy_p <- 41; ^nxy_p <- 42; done
    | ^nxy_n -> x;  ^nxy_n -> y;  ^dummy <- x+y; done
    )
  }
|])

