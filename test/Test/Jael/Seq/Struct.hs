{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Seq.Struct
( structTests
) where

import ClassyPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Parser
import Jael.Seq.AST
import Jael.Seq.Struct
import Test.Framework as T
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Jael.Util

-- For testing purposes two PolyTy are equal when their type variables and types
-- are equal. Consider, however, whether PolyTy ["a"] (TVar "a") should be equal
-- to PolyTy ["b"] (TVar "b")
instance Eq PolyTy where
  (PolyTy xs ts) == (PolyTy ys us) = xs == ys && ts == us

structTests :: [T.Test]
structTests =
  [ testCase "Valid struct, not polymorphic" $ checkParsedTypes structValidSimple
  , testCase "Valid struct, polymorphic" $ checkParsedTypes structValidPoly
  , testCase "Duplicate type variables" $ checkStructErr structDupTyVars
  , testCase "Duplicate fields" $ checkStructErr structDupFields
  , testCase "Free type variables in struct types" $ checkStructErr structFreeTvs
  , testCase "Unused type variable in struct" $ checkStructErr structUnusedTv
  , testCase "Combination of all possible errors" $ checkStructErr structAllErrs
  ]

checkParsedTypes :: (Text, [(Text, PolyTy)]) -> Assertion
checkParsedTypes (def, expected) =
  case runParser pGTStructDef def of
       Left err -> assertFailure (show err)
       Right gStructDef ->
         case validateStruct (gToStruct gStructDef) of
              Left sErr -> assertFailure (show sErr)
              Right tys -> assertEqual "" (M.fromList expected) (M.fromList tys)

checkStructErr :: (Text, StructError) -> Assertion
checkStructErr (def, StructError (DuplicateTyVars ets)
                                 (DuplicateFields efs)
                                 (FreeTyVars eftvs)
                                 (UnusedTyVars eutvs)) =
  case runParser pGTStructDef def of
       Left err -> assertFailure (show err)
       Right gStructDef ->
         case validateStruct (gToStruct gStructDef) of
              Left (StructError (DuplicateTyVars ts)
                                (DuplicateFields fs)
                                (FreeTyVars ftvs)
                                (UnusedTyVars utvs))-> do
                   assertEqual "Duplicate type variables wrong" (sort ets) (sort ts)
                   assertEqual "Duplicate fields wrong" (sort efs) (sort fs)
                   assertEqual "Free type variables wrong" eftvs ftvs
                   assertEqual "Unused type variables wrong" eutvs utvs
              Right _ -> assertFailure "Expected struct error"

structValidSimple :: (Text, [(Text, PolyTy)])
structValidSimple = (pack [raw|
  X { Int :: f0, Bool :: f1 }
|], [ ("x",     PolyTy [] $ TFun TInt (TFun TBool (TNamed "X" [])))
    , ("X::f0", PolyTy [] $ TFun (TNamed "X" []) TInt)
    , ("X::f1", PolyTy [] $ TFun (TNamed "X" []) TBool)
    ]
  )

structValidPoly :: (Text, [(Text, PolyTy)])
structValidPoly = (pack [raw|
  X a b { a :: f0, b :: f1 }
|], [ ("x",     PolyTy ["a", "b"] $ TFun (TVar "a") (TFun (TVar "b") (TNamed "X" [TVar "a", TVar "b"])))
    , ("X::f0", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "a"))
    , ("X::f1", PolyTy ["a", "b"] $ TFun (TNamed "X" [TVar "a", TVar "b"]) (TVar "b"))
    ]
  )

structDupTyVars :: (Text, StructError)
structDupTyVars = (pack [raw|
  X a a { a :: f1, a :: f2 }
|], StructError (DuplicateTyVars ["a"])
                (DuplicateFields [])
                (FreeTyVars $ S.fromList [])
                (UnusedTyVars $ S.fromList [])
  )

structDupFields :: (Text, StructError)
structDupFields = (pack [raw|
  X { Int :: same, Bool :: same }
|], StructError (DuplicateTyVars [])
                (DuplicateFields ["same"])
                (FreeTyVars $ S.fromList [])
                (UnusedTyVars $ S.fromList [])
  )

structFreeTvs :: (Text, StructError)
structFreeTvs = (pack [raw|
  X a { a :: f1, b :: f2 }
|], StructError (DuplicateTyVars [])
                (DuplicateFields [])
                (FreeTyVars $ S.fromList ["b"])
                (UnusedTyVars $ S.fromList [])
  )

structUnusedTv :: (Text, StructError)
structUnusedTv = (pack [raw|
  X a { TInt :: f1, TBool :: f2 }
|], StructError (DuplicateTyVars [])
                (DuplicateFields [])
                (FreeTyVars $ S.fromList [])
                (UnusedTyVars $ S.fromList ["a"])
  )

structAllErrs :: (Text, StructError)
structAllErrs = (pack [raw|
  X a a c c { a :: f1, a :: f1, b :: f2, b :: f2 }
|], StructError (DuplicateTyVars ["a", "c"])
                (DuplicateFields ["f1", "f2"])
                (FreeTyVars $ S.fromList ["b"])
                (UnusedTyVars $ S.fromList ["c"])
  )

