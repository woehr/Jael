{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}

module Jael.Test.PrettyProps where

import           Data.Char                                ( isLower )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc

import           Hedgehog
import qualified Hedgehog.Gen                  as G
import qualified Hedgehog.Range                as R

import           Jael.Types
import           Jael.Pretty

--reserved :: S.Set String
--reserved = btreeToSet L.resWords
-- where
--  btreeToSet L.N = S.empty
--  btreeToSet (L.B s _ l r) =
--    S.unions [S.singleton s, btreeToSet l, btreeToSet r]

--instance Arbitrary Literal where
--  arbitrary = oneof
--    [ LInt . defaultInt <$> arbitrary
--    , LInt . (\s -> JInt BinInt (number 2  (drop 2 s)) (length s))
--        <$> listOf1 (elements ( fmap intToDigit [0..1]))
--    , LInt . (\s -> JInt OctInt (number 8  (drop 2 s)) (length s))
--        <$> listOf1 (elements ( fmap intToDigit [0..7]))
--    , LInt . (\s -> JInt HexInt (number 16 (drop 2 s)) (length s))
--        <$> listOf1 (elements ( fmap intToDigit [0..15]))
--    ]

--listOf2 :: Gen a -> Gen [a]
--listOf2 g = (:) <$> g <*> listOf1 g

--recPats :: Gen [(T.Text, P)]
--recPats = do
--  ls <- listOf1 lident
--  zip ls <$> vectorOf (fromInteger $ length ls) arbitrary

lident :: Gen T.Text
lident = T.pack
  <$> G.filter f (G.string (R.linear 0 20) (G.alphaNum <> pure '_'))
 where
  f xs = let xs' = filter (/= '_') xs in not (null xs') && isLower (head xs')

genPattern :: Gen (Pattern T.Text Integer)
genPattern = G.recursive
  G.choice
  [ PVar <$> lident
  , PLit <$> G.integral (R.exponentialFrom 0 ((-2) ^ 32) (2 ^ 32))
  , pure PWild
  ]
  []

prop_true :: Property
prop_true = property $ do
  xs <- forAll genPattern
  (parseToPattern . show . pretty) xs === xs

tests :: Group
tests = $$discover
