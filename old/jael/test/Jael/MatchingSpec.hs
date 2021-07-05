{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Jael.MatchingSpec where

import           Debug.Trace
import           Data.Tree.Pretty

import qualified Test.Hspec
import           Test.Hspec                               ( Spec
                                                          , Expectation
                                                          , describe
                                                          , it
                                                          )

import qualified Data.Text                     as T
import qualified Data.Set                      as S

import           Jael.AST
import           Jael.Expr
import           Jael.DataDecl
import           Jael.Matching
import           Jael.Pattern
import           Jael.Prelude.Minimal

defs :: String
defs
  = "data Maybe(a) = just(a) | nothing;\
       \data Either(a, b) = left(a) | right(b);\
       \data List(a) = cons(a,a) | nil;"

cinfos :: [[ConsInfo ParseType]]
cinfos =
  fmap (\(_, dd) -> fmap snd (dataCons dd)) . _astDataDef . parseToAST $ defs

pInt :: Integer -> P
pInt = PLit . LInt . defaultInt

ex1 :: ClauseMatrix
ex1 =
  [ ([PCon (T.pack "nil") [], PVar (T.pack "a")], 1)
  , ([PVar (T.pack "b"), PCon (T.pack "nil") []], 2)
  , ( [ PCon (T.pack "cons") [PVar (T.pack "o00"), PVar (T.pack "o01")]
      , PCon (T.pack "cons") [PVar (T.pack "o10"), PVar (T.pack "o11")]
      ]
    , 3
    )
  ]

ats :: ClauseMatrix
ats =
  [ ([PAt (T.pack "v1") (PTup [pInt 1, PWild])], 1)
  , ([PAt (T.pack "v2") (PTup [pInt 2, PWild])], 2)
  , ( [ PAt (T.pack "v3")
            (PTup [PAt (T.pack "v3'") (pInt 3), PVar (T.pack "v3''")])
      ]
    , 3
    )
  , ( [ PTup
          [ PAt (T.pack "v4") (PAt (T.pack "v4'") (PAt (T.pack "v4''") PWild))
          , PWild
          ]
      ]
    , 4
    )
  ]

ints :: ClauseMatrix
ints = [([pInt 1], 1), ([pInt 2], 2), ([pInt 3], 3)]

intsDef :: ClauseMatrix
intsDef = [([pInt 1], 1), ([pInt 2], 2), ([pInt 3], 3), ([PWild], 4)]

sameCons :: ClauseMatrix
sameCons =
  [ ([PCon (T.pack "just") [PWild]], 1)
  , ([PCon (T.pack "just") [PWild]], 2)
  , ([PCon (T.pack "nothing") []]  , 3)
  ]

arr :: ClauseMatrix
arr =
  [ ([PArr [pInt 1, PWild]], 1)
  , ([PArr [PWild, pInt 2]], 2)
  , ([PArr [PWild, PWild]] , 3)
  ]

-- Same clause matrix as arr except using the tuple pattern
tup :: ClauseMatrix
tup =
  [ ([PTup [pInt 1, PWild]], 1)
  , ([PTup [PWild, pInt 2]], 2)
  , ([PTup [PWild, PWild]] , 3)
  ]

recs :: ClauseMatrix
recs =
  [ ([PRec [(T.pack "one", pInt 1)] (PWild :: RecTailPat T.Text)], 1)
  , ( [PRec [(T.pack "two", pInt 2)] (PVar (T.pack "x") :: RecTailPat T.Text)]
    , 2
    )
  , ( [ PRec
          [(T.pack "one", PVar (T.pack "y")), (T.pack "two", PVar (T.pack "z"))]
          (PRecEmpty :: RecTailPat T.Text)
      ]
    , 3
    )
  ]

testCC :: (PatternMatrix -> Integer)
       -> ClauseMatrix
       -> (DecisionTree, S.Set (Occurence T.Text))
testCC f m = case cc cinfos f m of
  Left  e -> error (show e)
  Right x -> x

ccLeft :: ClauseMatrix -> (DecisionTree, S.Set (Occurence T.Text))
ccLeft m = testCC selectLeftColumn m

ccRight :: ClauseMatrix -> (DecisionTree, S.Set (Occurence T.Text))
ccRight m = testCC selectRightColumn m

shouldBe :: (Show a, Eq a)
         => (DecisionTree, a)
         -> (DecisionTree, a)
         -> Expectation
shouldBe x@(l, _) y =
  let res = x `Test.Hspec.shouldBe` y
  in  if x == y
        then res
        else trace (drawVerticalTree . decisionTreeToTree $ l) res

-- [1] Compiler Pattern Matching to Good Decision Trees, Luc Maranget, ML'08
spec :: Spec
spec = describe "Match compiler" $ do
  it "Fails on empty matrix" $ ccLeft [] `shouldBe` (Fail, S.empty)
  it "matches example in [1] (examine left-most eligible column first)"
    $          ccLeft ex1
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ (CName "nil", Leaf 1)
                   , ( CName "cons"
                     , Switch
                       [1]
                       (CaseList
                         [(CName "nil", Leaf 2), (CName "cons", Leaf 3)]
                         Nothing
                       )
                     )
                   ]
                   Nothing
                 )
               , S.fromList
                 [ Occurence [0]    (1, "b")
                 , Occurence [1]    (0, "a")
                 , Occurence [0, 0] (2, "o00")
                 , Occurence [1, 0] (2, "o01")
                 , Occurence [0, 1] (2, "o10")
                 , Occurence [1, 1] (2, "o11")
                 ]
               )
  it "matches example in [1] (examine right-most eligible column first)"
    $          ccRight ex1
    `shouldBe` ( Switch
                 [1]
                 (CaseList
                   [ ( CName "nil"
                     , Switch
                       [0]
                       (CaseList [(CName "nil", Leaf 1)] (Just (Leaf 2)))
                     )
                   , ( CName "cons"
                     , Switch
                       [0]
                       (CaseList
                         [(CName "nil", Leaf 1), (CName "cons", Leaf 3)]
                         Nothing
                       )
                     )
                   ]
                   Nothing
                 )
               , S.fromList
                 [ Occurence [0]    (1, "b")
                 , Occurence [1]    (0, "a")
                 , Occurence [0, 0] (2, "o00")
                 , Occurence [1, 0] (2, "o01")
                 , Occurence [0, 1] (2, "o10")
                 , Occurence [1, 1] (2, "o11")
                 ]
               )

  it "Records occurences of variables in 'at' patterns"
    $          ccLeft ats
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ ( CTuple
                     , Switch
                       [0, 0]
                       (CaseList
                         [(CInt 1, Leaf 1), (CInt 2, Leaf 2), (CInt 3, Leaf 3)]
                         (Just (Leaf 4))
                       )
                     )
                   ]
                   Nothing
                 )
               , S.fromList
                 [ Occurence [0]    (0, "v1")
                 , Occurence [0]    (1, "v2")
                 , Occurence [0]    (2, "v3")
                 , Occurence [0, 0] (2, "v3'")
                 , Occurence [1, 0] (2, "v3''")
                 , Occurence [0, 0] (3, "v4")
                 , Occurence [0, 0] (3, "v4'")
                 , Occurence [0, 0] (3, "v4''")
                 ]
               )

  it "matches integers"
    $          ccLeft ints
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [(CInt 1, Leaf 1), (CInt 2, Leaf 2), (CInt 3, Leaf 3)]
                   (Just Fail)
                 )
               , S.empty
               )

  it "matches integers"
    $          ccLeft intsDef
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [(CInt 1, Leaf 1), (CInt 2, Leaf 2), (CInt 3, Leaf 3)]
                   (Just (Leaf 4))
                 )
               , S.empty
               )

  it "handles redundant constructors"
    $          ccLeft sameCons
    `shouldBe` ( Switch
                 [0]
                 (CaseList [(CName "just", Leaf 1), (CName "nothing", Leaf 3)]
                           Nothing
                 )
               , S.empty
               )

  it "arrays"
    $          ccLeft arr
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ ( CArray
                     , Switch
                       [0, 0]
                       (CaseList
                         [(CInt 1, Leaf 1)]
                         (Just
                           (Switch
                             [1, 0]
                             (CaseList [(CInt 2, Leaf 2)] (Just (Leaf 3)))
                           )
                         )
                       )
                     )
                   ]
                   Nothing
                 )
               , S.empty
               )

  it "arrays (right)"
    $          ccRight arr
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ ( CArray
                     , Switch
                       [1, 0]
                       (CaseList
                         [ ( CInt 2
                           , Switch
                             [0, 0]
                             (CaseList [(CInt 1, Leaf 1)] (Just (Leaf 2)))
                           )
                         ]
                         (Just
                           (Switch
                             [0, 0]
                             (CaseList [(CInt 1, Leaf 1)] (Just (Leaf 3)))
                           )
                         )
                       )
                     )
                   ]
                   Nothing
                 )
               , S.empty
               )

  it "tuples"
    $          ccLeft tup
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ ( CTuple
                     , Switch
                       [0, 0]
                       (CaseList
                         [(CInt 1, Leaf 1)]
                         (Just
                           (Switch
                             [1, 0]
                             (CaseList [(CInt 2, Leaf 2)] (Just (Leaf 3)))
                           )
                         )
                       )
                     )
                   ]
                   Nothing
                 )
               , S.empty
               )

  it "records"
    $          ccLeft recs
    `shouldBe` ( Switch
                 [0]
                 (CaseList
                   [ ( CRecord
                     , Switch
                       [0, 0]
                       (CaseList
                         [(CInt 1, Leaf 1)]
                         (Just
                           (Switch
                             [1, 0]
                             (CaseList [(CInt 2, Leaf 2)] (Just (Leaf 3)))
                           )
                         )
                       )
                     )
                   ]
                   Nothing
                 )
               , S.fromList
                 [ Occurence [2, 0] (1, "x")
                 , Occurence [0, 0] (2, "y")
                 , Occurence [1, 0] (2, "z")
                 ]
               )
