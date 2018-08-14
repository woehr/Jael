module Mine
  ()
where

import           Data.Set

{-@ predicate Within N X = ( 0 <= X && X < N ) @-}

--{-@ assume impossible :: {v:() | False} -> a @-}
--impossible () = undefined

--{-@ assert :: {v:Bool | v} -> a -> a @-}
--assert :: Bool -> a -> a
--assert b x = if b then x else impossible ()

--test :: Int
--test = assert True 4
--test = assert False 4

--test2 = impossible undefined
--test3 = assert True test2

data Arr a = Arr

{-@ measure indices :: Arr a -> Set Int @-}

{-@ assume arrEmpty :: { v:Arr | Set_emp (indices v) } @-}
arrEmpty :: Arr a
arrEmpty = undefined

{-@ assume arrSet :: i:{ v:Int | Within 10 v } -> a -> inp:(Arr a) -> { v:(Arr a) | (indices v = Set_cup (indices inp) (Set_sng i)) } @-}
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

set015 :: Arr Int
set015 = arrSet 0 0 (arrSet 1 1 (arrSet 5 42 arrEmpty))

{-@ test4 :: {v:Bool | v <=> True} @-}
test4 = arrIsSet 0 set015
--test4 = arrIsSet 2 set015

{-@ f :: {v:Arr Int | Set_mem 0 (indices v) && Set_mem 1 (indices v) } -> Int @-}
f :: Arr Int -> Int
f a = arrAt 0 a + arrAt 1 a

--test5 = f (arrUnset 0 set015)
test5 = f set015

data CircBuff a = CircBuff { count::Int, front::Int, buff::Arr a, ixs::Set Int}
{-@ data CircBuff a = CircBuff
      { count :: { v:Int   | 0 <= v && v <= 10 }
      , front :: { v:Int   | Within 10 v }
      , buff  :: Arr a
      , ixs   :: { v:Set Int |
                       (v == indices buff)
                 }
      }
@-}

{-@ cbEmpty
    :: {v:CircBuff a |
             front v = 0
          && count v = 0
          && Set_emp (indices (buff v))
       }
@-}
cbEmpty :: CircBuff a
cbEmpty = CircBuff 0 0 arrEmpty empty

{-@ cbAt
    :: i:{v:Int | Within 10 v }
    -> inp:
        { v:CircBuff a |
          member ((front v +i) mod 10) (ixs v)
        }
    -> a
@-}
cbAt :: Int -> CircBuff a -> a
cbAt i (CircBuff _ fr bu _) = arrAt ((fr + i) `mod` 10) bu

{-@ cbPushBack
    :: a
    -> inp: { v:CircBuff a | count v < 10
            }
    ->      { v:CircBuff a |
                   (count v == count inp + 1)
                && (front v == front inp)
                && (ixs v == union (ixs inp) (singleton ((front inp + count inp) mod 10)))
            }
@-}
cbPushBack :: a -> CircBuff a -> CircBuff a
cbPushBack x (CircBuff sz fr bu ixs) =
  let ix = (fr + sz) `mod` 10
  in  CircBuff (sz + 1) fr (arrSet ix x bu) (insert ix ixs)

{-@ cbPopBack
    :: inp: { v:CircBuff a |
                 count v > 0
              && member (front v + count v - 1 mod 10) (ixs v)
            }
    ->      (a, {v:CircBuff a |
                 (front v == front inp)
              && count v == count inp - 1
            })
@-}
cbPopBack :: CircBuff a -> (a, CircBuff a)
cbPopBack (CircBuff sz fr bu ixs) =
  let ix = (fr + sz - 1) `mod` 10
  in  (arrAt ix bu, CircBuff (sz - 1) fr (arrUnset ix bu) (delete ix ixs))

cbPopBack' :: CircBuff a -> CircBuff a
cbPopBack' x = snd (cbPopBack x)

test6 = cbEmpty
test6' = cbPushBack 100 test6
test6'' = cbPushBack 1 test6'
test6''' = cbPushBack
  9
  (cbPushBack
    8
    (cbPushBack
      7
      (cbPushBack
        6
        (cbPushBack
          5
          (cbPushBack
            4
            (cbPushBack 3 (cbPushBack 2 (cbPushBack 1 (cbPushBack 0 cbEmpty))))
          )
        )
      )
    )
  )
--test6'''' = cbPushBack 10 test7

--test7 = cbPopBack cbEmpty
test7' = cbPopBack test6'
test7'' = cbPushBack 0 (cbPopBack' (cbPushBack 0 (snd test7')))
test7''' = cbPopBack' test7''
-- test7'''' = cbPopBack' test7'''

--test8 = cbAt 0 cbEmpty
--test8' = cbAt 1 test6''

-- {-@ dummy1::
--     {v:CircBuff a| count v == 1
--     }
-- @-}
-- dummy1 :: CircBuff a
-- dummy1 = undefined
