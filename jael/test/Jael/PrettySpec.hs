{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jael.PrettySpec where

import           Data.Text.Prettyprint.Doc
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Text                     as T
import qualified Data.Set                      as S
import qualified Jael.Grammar                  as L

import           Jael.AST
import           Jael.Expr
import           Jael.Pattern
import           Jael.Prelude
import           Jael.Pretty                              ( )

type P = Pattern T.Text T.Text Literal

reserved :: S.Set String
reserved = btreeToSet L.resWords
 where
  btreeToSet L.N = S.empty
  btreeToSet (L.B s _ l r) =
    S.unions [S.singleton s, btreeToSet l, btreeToSet r]

lident :: Gen T.Text
lident = do
  n <- choose (1 :: Integer, 5)
  let l = elements ['a' .. 'z']
  nu <- choose (0 :: Integer, 2)
  let ls = take (fromIntegral n) <$> listOf1 l
  let c =
        elements $ '_' : ['a' .. 'z'] <> ['A' .. 'Z'] <> fmap toEnum [48 .. 57]
  let cs = take (fromIntegral n) <$> listOf1 c
  let x  = (replicate (fromIntegral nu) '_' <>) <$> ((<>) <$> ls <*> cs)
  T.pack <$> x `suchThat` (not . (`S.member` reserved))

uident :: Gen T.Text
uident = do
  u <- elements ['A' .. 'Z']
  (T.singleton u <>) <$> lident

instance Arbitrary Literal where
  arbitrary = oneof
    [ LInt . defaultInt <$> arbitrary
    , LInt . (\s -> JInt BinInt (number 2  (drop 2 s)) (length s))
        <$> listOf1 (elements ( fmap intToDigit [0..1]))
    , LInt . (\s -> JInt OctInt (number 8  (drop 2 s)) (length s))
        <$> listOf1 (elements ( fmap intToDigit [0..7]))
    , LInt . (\s -> JInt HexInt (number 16 (drop 2 s)) (length s))
        <$> listOf1 (elements ( fmap intToDigit [0..15]))
    ]

listOf2 :: Gen a -> Gen [a]
listOf2 g = (:) <$> g <*> listOf1 g

instance Arbitrary (RecTailPat T.Text) where
  arbitrary = oneof
    [ PVar <$> lident
    , return PWild
    , return PRecEmpty
    ]
  shrink (PVar (_::T.Text)) = []
  shrink PWild = []
  shrink PRecEmpty = []
  shrink _ = error "This to get rid of warning only"

recPats :: Gen [(T.Text, P)]
recPats = do
  ls <- listOf1 lident
  zip ls <$> vectorOf (fromInteger $ length ls) arbitrary

instance Arbitrary ParsePattern where
  arbitrary = sized $ \i -> resize (i `div` 2)
    $ oneof
    [ PVar <$> lident
    , PLit <$> (arbitrary :: Gen Literal)
    , return PWild
    , return PRecEmpty
    , PAt <$> lident <*> arbitrary
    , PArr <$> listOf1 arbitrary
    , PTup <$> listOf2 arbitrary
    , PCon <$> lident <*> listOf arbitrary
    , PRec <$> recPats <*> (arbitrary :: Gen (RecTailPat T.Text))
    , POr <$> listOf2 arbitrary
    ]
  shrink (PVar (_::T.Text)) = []
  shrink (PLit (_::Literal)) = []
  shrink PWild = []
  shrink PRecEmpty = []
  shrink (PAt (_::T.Text) p) = [p]
  shrink (PArr ps) = ps
  shrink (PTup ps) = ps
  shrink (PCon (_::T.Text) ps) = ps
  shrink (PRec (ls::[(T.Text,P)]) (_::RecTailPat T.Text)) = fmap snd ls
  shrink (POr ps) = ps
  shrink _ = error "This to get rid of warning only"

spec :: Spec
spec =
  describe "pretty should be inverse to parsing"
    $ it "for ParsePattern"
    $ property (\x -> (parseToPattern . show . pretty) x === x)
