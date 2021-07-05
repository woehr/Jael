module Mine2
  ()
where

import           Data.Set

{-@ assume impossible :: { v:() | False } -> a @-}
impossible :: () -> a
impossible = undefined

{-@ assert :: { v:Bool | v } -> a -> a @-}
assert :: Bool -> a -> a
assert b x = if b then x else impossible ()

--foo = assert False ()
foo = assert True ()

{-@ predicate Within N X = ( 0 <= X && X < N ) @-}

data Arr a = Arr

{-@ assume arrEmpty :: { v:Arr | Set_emp (indices v) } @-}
arrEmpty :: Arr a
arrEmpty = undefined

{-@ assume arrSet
    :: i:{ v:Int | Within 10 v }
    -> a
    -> inp:(Arr a)
    -> { v:(Arr a) | (indices v = Set_cup (indices inp) (Set_sng i)) }
@-}
arrSet :: Int -> a -> Arr a -> Arr a
arrSet = undefined

{-@ assume arrIsSet
  :: i:{v:Int | Within 10 v }
  -> inp:(Arr a)
  -> {v:Bool | v <=> Set_mem i (indices inp) }
@-}
arrIsSet :: Int -> Arr a -> Bool
arrIsSet = undefined

{-@ assume arrUnset
  :: i:{v:Int | Within 10 v}
  -> inp:(Arr a)
  -> { v:Arr a | indices v = Set_dif (indices inp) (Set_sng i) }
@-}
arrUnset :: Int -> Arr a -> Arr a
arrUnset = undefined

--unsetTest :: ()
--unsetTest = assert (not $ arrIsSet 1 (arrUnset 1 (arrSet 1 1 arrEmpty))) ()

{-@ arrAt
  :: i:Int
  -> inp:{v:(Arr a) | Set_mem i (indices v) }
  -> a
@-}
arrAt :: Int -> Arr a -> a
arrAt = undefined

{-@ measure indices :: Arr a -> Set Int @-}

{-@ dummy0 :: {v:CircBuff a | member 0 (indices (buff v)) } @-}
dummy0 :: CircBuff a
dummy0 = undefined

{-@ dummy1 :: {v:CircBuff a | member 1 (indices (buff v)) } @-}
dummy1 :: CircBuff a
dummy1 = undefined

{-@ predicate WithinCirc X FR SZ =
    if (FR+SZ <= 10) then ((FR <= X) && (X < FR+SZ)) else ((FR <= X && X < 10) || (0 <= X && X < (FR+SZ mod 10)))
@-}

{-@ data CircBuff a <dom :: Int -> Bool> = CircBuff
      { count :: { v:Int   | 0 <= v && v <= 10 }
      , front :: { v:Int   | Within 10 v }
      , buff  :: Arr a
      }
@-}
data CircBuff a = CircBuff { count::Int, front::Int, buff::Arr a}

{-@ cbEmpty
    :: {v:CircBuff <{\_ -> False}> a |
             front v = 0
          && count v = 0
          && Set_emp (indices (buff v))
       }
@-}
cbEmpty :: CircBuff a
cbEmpty = CircBuff 0 0 arrEmpty

{-@ cbAt
    :: forall <dom :: Int -> Bool>
    .  i:   { v:Int | Within 10 i }
    -> inp: { v:CircBuff <dom> a | member ((front v +i) mod 10) (indices (buff inp)) }
    -> a
@-}
cbAt :: Int -> CircBuff a -> a
cbAt i (CircBuff _ fr bu) = arrAt ((fr + i) `mod` 10) bu

-- <{\x -> (WithinCirc x (front inp) (count inp + 1)) => (member x (union (indices (buff inp)) (singleton ((front inp + count inp) mod 10)))) }> a
{-@ cbPushBack
    :: forall <dom :: Int -> Bool>
    .  a
    -> inp: { v:CircBuff <dom> a | count v < 10 }
    ->      { v:CircBuff <dom> a
               | (count v == count inp + 1)
              && (front v == front inp)
              && (indices (buff v) == union (indices (buff inp)) (singleton ((front inp + count inp) mod 10)))
    }
@-}
cbPushBack :: a -> CircBuff a -> CircBuff a
cbPushBack x (CircBuff sz fr bu) =
  let ix = (fr + sz) `mod` 10 in CircBuff (sz + 1) fr (arrSet ix x bu)

test1 :: CircBuff a
test1 = cbEmpty

test2 :: CircBuff Integer
test2 = cbPushBack 0 test1

test3 :: Integer
test3 = cbAt 0 test2

-- {-@ cbPopBack
--     :: inp: { v:CircBuff a |
--                  count v > 0
--               && member (front v + count v - 1 mod 10) (ixs v)
--             }
--     ->      (a, {v:CircBuff a |
--                  (front v == front inp)
--               && count v == count inp - 1
--             })
-- @-}
-- cbPopBack :: CircBuff a -> (a, CircBuff a)
-- cbPopBack (CircBuff sz fr bu ixs) =
--   let ix = (fr + sz - 1) `mod` 10
--   in  (arrAt ix bu, CircBuff (sz - 1) fr (arrUnset ix bu) (delete ix ixs))
--
-- cbPopBack' :: CircBuff a -> CircBuff a
-- cbPopBack' x = snd (cbPopBack x)
--
-- test6 = cbEmpty
-- test6' = cbPushBack 100 test6
-- test6'' = cbPushBack 1 test6'
-- test6''' = cbPushBack
--   9
--   (cbPushBack
--     8
--     (cbPushBack
--       7
--       (cbPushBack
--         6
--         (cbPushBack
--           5
--           (cbPushBack
--             4
--             (cbPushBack 3 (cbPushBack 2 (cbPushBack 1 (cbPushBack 0 cbEmpty))))
--           )
--         )
--       )
--     )
--   )
-- --test6'''' = cbPushBack 10 test7
--
-- --test7 = cbPopBack cbEmpty
-- test7' = cbPopBack test6'
-- test7'' = cbPushBack 0 (cbPopBack' (cbPushBack 0 (snd test7')))
-- test7''' = cbPopBack' test7''
-- -- test7'''' = cbPopBack' test7'''
--
-- --test8 = cbAt 0 cbEmpty
-- test8' = cbAt 1 test6''
