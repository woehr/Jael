{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--ple" @-}

module Mine3
  ( indices
  , cbIndices
  )
where

import           Prelude                           hiding ( mod )
import           Data.Set
import           Language.Haskell.Liquid.ProofCombinators

{-@ assume impossible :: { v:() | False } -> a @-}
impossible :: () -> a
impossible = undefined

{-@ assert :: { v:Bool | v } -> a -> a @-}
assert :: Bool -> a -> a
assert b x = if b then x else impossible ()

--foo = assert False ()
--foo = assert True ()

{-@ predicate Within N X = ( 0 <= X && X < N ) @-}

data Arr a = Arr (Set Int)

{-@ measure indices @-}
indices :: Arr a -> Set Int
indices (Arr s) = s

{-@ assume arrEmpty :: { v:Arr | Set_emp (indices v) } @-}
arrEmpty :: Arr a
arrEmpty = Arr empty

{-@ assume arrSet
    :: i:{ v:Int | Within 10 v }
    -> a
    -> inp:(Arr a)
    -> { v:(Arr a) | (indices v = Set_cup (indices inp) (Set_sng i)) }
@-}
arrSet :: Int -> a -> Arr a -> Arr a
arrSet i _ (Arr ixs) = Arr (insert i ixs)

{-@ assume arrIsSet
  :: i:{v:Int | Within 10 v }
  -> inp:(Arr a)
  -> {v:Bool | v <=> Set_mem i (indices inp) }
@-}
arrIsSet :: Int -> Arr a -> Bool
arrIsSet i (Arr ixs) = i `member` ixs

{-@ assume arrUnset
  :: i:{v:Int | Within 10 v}
  -> inp:(Arr a)
  -> { v:Arr a | indices v = Set_dif (indices inp) (Set_sng i) }
@-}
arrUnset :: Int -> Arr a -> Arr a
arrUnset i (Arr ixs) = Arr (delete i ixs)

{-@ arrAt
  :: i:Int
  -> inp:{v:(Arr a) | Set_mem i (indices v) }
  -> a
@-}
arrAt :: Int -> Arr a -> a
arrAt = undefined

{-@ assume mod
    :: x:Int
    -> y:{v:Int | v /= 0}
    ->   {v:Int | v = x mod y && ((0 <= x && 0 < y) => (0 <= v && v < y))}
@-}
mod :: Int -> Int -> Int
mod _ _ = undefined

{-@ reflect cbIndices @-}
{-@ cbIndices :: sz:{v:Int | v >= 0} -> {v:Int | v >= 0} -> Set Int / [sz] @-}
cbIndices :: Int -> Int -> Set Int
cbIndices sz fr = if sz > 0
  then union (singleton ((fr + sz - 1) `mod` 10)) (cbIndices (sz - 1) fr)
  else empty

{-@ data CircBuff a = CircBuff
      { count :: { v:Int   | 0 <= v && v <= 10 }
      , front :: { v:Int   | Within 10 v }
      , buff  :: { v:Arr a | (indices v) == (cbIndices count front) }
      }
@-}
data CircBuff a = CircBuff { count::Int, front::Int, buff::Arr a}

cbEmpty :: CircBuff a
cbEmpty = CircBuff 0 0 arrEmpty

cbAt :: Int -> CircBuff a -> a
cbAt i (CircBuff _ fr bu) = arrAt ((fr + i) `mod` 10) bu

{-@ cbPushBack
    :: a
    -> inp: { v:CircBuff a | count v < 10 }
    -> CircBuff a
@-}
cbPushBack :: a -> CircBuff a -> CircBuff a
cbPushBack x inp@(CircBuff sz fr bu) =
  let ix = (fr + sz) `mod` 10
  in  lemma sz fr bu `seq` CircBuff (sz + 1) fr (arrSet ix x bu)

{-@ lemma
    :: sz: { v:Int | v >= 0 }
    -> fr: { v:Int | v >= 0 }
    -> bu: Arr a
    -> { cbIndices (sz+1) fr = union (indices bu) (singleton ((fr+sz) mod 10)) } 
@-}
lemma :: Int -> Int -> Arr a -> ()
lemma sz fr bu =
  cbIndices (sz + 1) fr
  -- Substitute cbIndices definition
    ==. (if (sz + 1) > 0
          then union (singleton ((fr + (sz + 1) - 1) `mod` 10))
                     (cbIndices ((sz + 1) - 1) fr)
          else empty
        )
    -- True case of if statement
    ==. union (singleton ((fr + (sz + 1) - 1) `mod` 10))
              (cbIndices ((sz + 1) - 1) fr)
    -- Simplify (fr + sz + 1 - 1)
    ==. union (singleton ((fr + sz) `mod` 10)) (cbIndices sz fr)
    -- Sub (cbIndices sz fr) for (indices bu) by refinement of buff
    ==. union (singleton ((fr + sz) `mod` 10)) (indices bu)
    *** QED

test1 :: CircBuff a
test1 = cbEmpty

--test2 :: CircBuff Integer
--test2 = cbPushBack 0 test1

--test3 :: Integer
--test3 = cbAt 0 test2

-- {-@ dummy0 :: {v:CircBuff a | member 0 (indices (buff v)) } @-}
-- dummy0 :: CircBuff a
-- dummy0 = undefined
--
-- {-@ dummy1 :: {v:CircBuff a | member 1 (indices (buff v)) } @-}
-- dummy1 :: CircBuff a
-- dummy1 = undefined

-- {-@ cbPopBack
--     :: inp: { v:CircBuff a |
--                    count v > 0
--                 && member ((front v + count v - 1) mod 10) (indices (buff v))
--             }
--     ->      (a, {v:CircBuff a |
--                      (front v == front inp)
--                   && (count v == count inp - 1)
--             })
-- @-}
-- cbPopBack :: CircBuff a -> (a, CircBuff a)
-- cbPopBack (CircBuff sz fr bu) =
--   let ix = (fr + sz - 1) `mod` 10
--   in  (arrAt ix bu, CircBuff (sz - 1) fr (arrUnset ix bu))

-- cbPopBack' :: CircBuff a -> CircBuff a
-- cbPopBack' x = snd (cbPopBack x)
