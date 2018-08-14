--{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--exact-data-cons" @-}
--{-@ LIQUID "--counter-examples" @-}

module Overlaps
  ( overlaps
  , bitrepFoo
  , isBar
  , isBaz
  , sized
  )
where

import           Language.Haskell.Liquid.NewProofCombinators

-- User defined
data Foo = Bar Word Word | Baz

-- User provided
{-@ measure overlaps @-}
overlaps :: Foo -> Bool
overlaps (Bar x y) = x == 3 && y == 3
overlaps Baz       = False

-- Auto generated from bitrep declaration
{-@ measure bitrepFoo @-}
bitrepFoo :: Foo -> Word
bitrepFoo (Bar x y) = (x * 4) + y
bitrepFoo Baz       = 15

{-@ measure isBar @-}
isBar :: Foo -> Bool
isBar (Bar _ _) = True
isBar _         = False

{-@ measure isBaz @-}
isBaz :: Foo -> Bool
isBaz Baz = True
isBaz _   = False

{-@ measure sized @-}
sized :: Foo -> Bool
sized (Bar x y) = 0 <= x && x <= 3 && 0 <= y && y <= 3
sized Baz       = True

{-@ type SizedNonOverlappingFoo = { v:Foo | not (overlaps v) && sized v } @-}

-- Prove that Bar and Baz don't overlap. This should be machine generatable.
{-@ lemmaNonOverlappingBarBaz
    :: a:{ v:SizedNonOverlappingFoo | isBar v }
    -> b:{ v:SizedNonOverlappingFoo | isBaz v }
    ->   { bitrepFoo a /= bitrepFoo b }
@-}
lemmaNonOverlappingBarBaz :: Foo -> Foo -> Bool
lemmaNonOverlappingBarBaz a@(Bar x y) b@Baz =
  bitrepFoo a /= bitrepFoo b === x * y + y /= 15 === True
lemmaNonOverlappingBarBaz _ _ = impossible ()
