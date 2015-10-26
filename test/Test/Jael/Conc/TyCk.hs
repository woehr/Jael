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
  , testCase "unused seq from arg and chan" $ checkTyCkErr unusedSeq
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
  -- The remainder of these tests are specific examples from papers. See the
  -- inline comments for more details
  , testCase "example 1" $ checkTyCkErr ex1
  , testCase "example 2" $ checkTyCkErr ex2
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
|], UnusedLin $ M.fromList [("x", SGetTy TInt SEnd)]
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

unusedSeq :: (Text, SessTyErr)
unusedSeq = (pack [raw|
  proc X(x: ?[Int];, y: Bool) {
    ^x -> z;
    done
  }
|], UnusedSeq $ M.fromList [("y", TBool), ("z", TInt)]
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
|], UnusedLin $ M.fromList [("y", SGetTy TInt SEnd)]
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
|], UnusedLin $ M.fromList
      [ ("x", SPutTy TInt $ SGetTy TInt $ SVar "X")
      , ("y", SPutTy TInt $ SGetTy TInt $ SVar "X")
      ]
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
|], UnusedLin $ M.fromList
      [ ("x", SPutTy TInt $ SGetTy TInt $ SVar "X")
      , ("y", SSelect [("a", SVar "X"), ("b", SVar "AltTxRxInt")])
      ]
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
    ( ^x -> i;  ^c <- x; done
    | ^y <- 42; ^y <- 42; done
    )
  }
|], NonFreshChan "x"
  )

-- This test ensures that two ends of a channel can not be passed as arguments
-- to the same named process. When typing named processes, it's assumed that
-- none of it's arguments are duals. This implicitly enforces dual channels
-- to be composed in parallel.
namedProcWithDualArgs :: (Text, SessTyErr)
namedProcWithDualArgs = (pack [raw|
  proc P() {
    new (^x, ^y) : ![Int]; ;
    DualArgProc(^x, ^y)
  }
|], DualChanArgs ("x", "y")
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

-- Check that two cases that use a channel different after the case type check.
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
|], ChannelInterference ("x", "y")
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
|], undefined
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
|], undefined
  )

