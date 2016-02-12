module Test.Jael.Conc.Proc
( procTests
) where

import           Data.Either (isRight)
import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Parser
import           Jael.Conc.Proc
import           Jael.Conc.Session
import           Jael.Seq.AST
import           Jael.Seq.Literal
import           Jael.Seq.Prm
import qualified Test.Framework as T

procTests :: [T.Test]
procTests =
  [ testCase "proc valid" $ checkProc valid
  , testCase "dup args in rec proc" $ checkTopProcErr dupArgs
  , testCase "dup args in top proc" $ checkTopProcErr dupArgs2
  , testCase "ambiguious co-rec name" $ checkTopProcErr ambiguousRecName
  , testCase "non-ambiguous rec name in par/choice" $ checkNoErr ambiguousRecName2
  ]

checkProc :: (Text, S1Proc) -> Assertion
checkProc (t, expected) = case parseProc t of
                               Left err -> error (show err)
                               Right p -> assertEqual "" expected p

checkNoErr :: Text -> Assertion
checkNoErr t = assertBool "" (isRight $ parseTopProc t)

checkTopProcErr :: (Text, ProcDefErr) -> Assertion
checkTopProcErr (t, expected) = case parseTopProc t of
                                     Left err -> assertEqual "" expected err
                                     Right _ -> error "expected error"

parseProc :: Text -> Either ProcDefErr S1Proc
parseProc t = case runParser pGProc t of
                   Left err -> error (show err)
                   Right g -> gToProc g

parseTopProc :: Text -> Either ProcDefErr S1TopProc
parseTopProc t = case runParser pGTopDef t of
                      Right (GTopDefGProcDef (GProcDef _ as p)) -> gToTopProc (as, p)
                      Left err -> error (show err)
                      _ -> error "Expected only process definitions."

valid :: (Text, S1Proc)
valid = (pack [raw|
  new (^xp, ^xn) : <SomeProto>;
  y = 5;
  ^xp -> z;
  ^xn <- y;
  ^xp <- true;
  ^xn select label;
  ^xp case
    { p1 =>
    , p2 => (
            | SomeProc(^xp)
            | SomeProc()
            | new (^a, ^b) : <dual Proto2>;
              ( ^z <- ^a;
              | ^z -> b;
              )
            )
    , p3 => rec X(j=x, k=1)
              { ^j <- k;
                ( X(^j, k+1)
                |
                )
              }
    }
|], S1PNewChan "xp" "xn" (SVar "SomeProto")
  $ S1PNewVal "y" (S1Lit $ LInt 5)
  $ S1PGetVal "xp" "z"
  $ S1PPutVal "xn" (S1Var "y")
  $ S1PPutVal "xp" (S1Lit $ LBool True)
  $ S1PSel "xn" "label"
  $ S1PCase "xp"
      [ ("p1", S1PNil)
      , ("p2", S1PPar
                [ S1PNil
                , S1PNamed "SomeProc" [Left "xp"]
                , S1PNamed "SomeProc" []
                , S1PNewChan "a" "b" (SDualVar "Proto2")
                $ S1PPar
                    [ S1PPutChan "z" "a" S1PNil
                    , S1PGetVal  "z" "b" S1PNil
                    ]
                ]
        )
      , ("p3", S1PCoRec "X" [ ("j", Right $ S1Var "x")
                          , ("k", Right $ S1Lit (LInt 1))
                          ]
               ( S1PPutVal "j" (S1Var "k")
               $ S1PPar [ S1PNamed "X" [ Left "j"
                                       , Right $ S1CallPrm PAdd [S1Var "k", S1Lit (LInt 1)]
                                   ]
                      , S1PNil
                      ]
               )
        )
      ]
  )

dupArgs :: (Text, ProcDefErr)
dupArgs = (pack [raw|
  proc X(x:Int, x:Bool) {
    rec Y(y=void, y=void) {}
  }
|], PDEDupArgs $ S.fromList ["y"]
  )

dupArgs2 :: (Text, ProcDefErr)
dupArgs2 = (pack [raw|
  proc X(x:Int, x:Bool) {
    rec Y(y=void, z=void) {}
  }
|], PDEDupArgs $ S.fromList ["x"]
  )

-- Y is ambiguous, Z is not. X is as well, but we need to consider all top level
-- processes to determine if a recursive name is hiding another
ambiguousRecName :: (Text, ProcDefErr)
ambiguousRecName = (pack [raw|
  proc X(x:Int, y:Bool, z:Foo) {
    rec X(x=x, z=z) {
      rec Y(x=x) {
        rec Y(x=x) {
          ( rec Z(x=x) {}
          | rec Z(x=x) {}
          )
        }
      }
    }
  }
|], PDEDupRecVar "Y"
  )

ambiguousRecName2 :: Text
ambiguousRecName2 = pack [raw|
  proc X(x:Int, y:Bool, z:Foo) {
    rec X(x=x, z=z) {
      ( rec Y(x=x) {}
      | rec Y(x=x) {
          ^x case
          { c1 => rec Z(x=x){}
          , c2 => rec Z(x=x){}
          , c3 => rec Z(x=x){}
          }
        }
      )
    }
  }
|]

