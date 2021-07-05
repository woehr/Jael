{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--ple" @-}

module CircBuff
  ( indices
  , calcCBIndices
  , cbIndices
  , modl
  , modr
  )
where

import           Prelude                           hiding ( mod
                                                          , null
                                                          )
import           Data.Set                          hiding ( splitAt )
import           Language.Haskell.Liquid.Prelude
import           Language.Haskell.Liquid.NewProofCombinators

{-@ assert :: { v:Bool | v } -> a -> a @-}
assert :: Bool -> a -> a
assert b x = if b then x else impossible ()

{-@ predicate Within N X = ( 0 <= X && X < N ) @-}

{-@ Arr
    :: Set Int
    -> { v:[a] | len v > 1000 }
    -> Arr a
@-}
data Arr a = Arr (Set Int) [a]

instance (Show a) => Show (Arr a) where
  show (Arr ixs xs) = (case toAscList ixs of
    i:is -> "[ " ++ f i ++ Prelude.foldl (\str i' -> str ++ ", " ++ f i') "" is
    [] -> "[") ++ " ]"
   where
    {-@ assume i :: {v:Int | 0 <= v && v <= 1000} @-}
    f i = show i ++ " -> " ++ show (xs !! i)

{-@ measure indices @-}
indices :: Arr a -> Set Int
indices (Arr s _) = s

{-@ assume arrEmpty :: { v:Arr | Set_emp (indices v) } @-}
arrEmpty :: Arr a
arrEmpty = Arr empty (repeat undefined)

{-@ assume arrSet
    :: i:{ v:Int | Within 10 v }
    -> a
    -> inp:(Arr a)
    -> { v:(Arr a) | (indices v == Set_cup (indices inp) (Set_sng i)) }
@-}
arrSet :: Int -> a -> Arr a -> Arr a
arrSet i x (Arr ixs xs) = Arr
  (insert i ixs)
  (case splitAt i xs of
    (bef, _ : aft) -> bef ++ [x] ++ aft
    _              -> undefined
  )

{-@ assume arrIsSet
  :: i:{v:Int | Within 10 v }
  -> inp:(Arr a)
  -> {v:Bool | v <=> Set_mem i (indices inp) }
@-}
arrIsSet :: Int -> Arr a -> Bool
arrIsSet i (Arr ixs _) = i `member` ixs

{-@ assume arrUnset
  :: i:{v:Int | Within 10 v}
  -> inp:(Arr a)
  -> { v:Arr a | indices v == Set_dif (indices inp) (Set_sng i) }
@-}
arrUnset :: Int -> Arr a -> Arr a
arrUnset i (Arr ixs xs) = Arr (delete i ixs) xs

{-@ arrAt
  :: i:{v:Int | Within 10 v }
  -> inp:{v:(Arr a) | Set_mem i (indices v) }
  -> a
@-}
arrAt :: Int -> Arr a -> a
arrAt i (Arr _ xs) = xs !! i

{-@ reflect modl @-}
{-@ modl
    :: x:{ v:Int | 0 <= v }
    -> y:{ v:Int | 0 < v }
    -> { v:Int | 0 <= v && v < y }
@-}
modl :: Int -> Int -> Int
modl x y = if x >= y then modl (x - y) y else x

{-@ reflect modr @-}
{-@ modr
    :: y:{ v:Int | 0 < v }
    -> x:{ v:Int | v < y }
    -> { v:Int | 0 <= v && v < y }
    / [y - x]
@-}
modr :: Int -> Int -> Int
modr y x = if x < 0 then modr y (x + y) else x

{-@ reflect calcCBIndices @-}
{-@ calcCBIndices
    :: sz:{v:Int | v >= 0}
    -> {v:Int | v >= 0}
    -> Set Int
    / [sz]
@-}
calcCBIndices :: Int -> Int -> Set Int
calcCBIndices sz fr = if sz > 0
  then union (singleton ((fr + sz - 1) `modl` 10)) (calcCBIndices (sz - 1) fr)
  else empty

{-@ measure cbIndices @-}
cbIndices :: CircBuff a -> Set Int
cbIndices (CircBuff sz fr _) = calcCBIndices sz fr

{-@ data CircBuff a = CircBuff
    { count :: { v:Int   | 0 <= v && v <= 10 }
    , front :: { v:Int   | Within 10 v }
    , buff  :: { v:Arr a | indices v == calcCBIndices count front }
    }
@-}
data CircBuff a = CircBuff { count::Int, front::Int, buff::Arr a}
  deriving (Show)

{-@ cbEmpty
    :: { v: CircBuff a | (count v == 0)
                      && (front v == 0)
       }
@-}
cbEmpty :: CircBuff a
cbEmpty = CircBuff 0 0 arrEmpty

{-@ cbAt
    :: i:   { v:Int | Within 10 v }
    -> inp: { v:CircBuff a | i < count v}
    -> a
@-}
cbAt :: Int -> CircBuff a -> a
cbAt i (CircBuff sz fr bu) =
  lemmaValidIndex sz fr i `seq` arrAt ((fr + i) `modl` 10) bu

{-@ cbPushBack
    :: a
    -> inp: { v:CircBuff a | count v < 10 }
    -> { v:CircBuff a |
               (count v == count inp + 1)
            && (front v == front inp)
       }
@-}
cbPushBack :: a -> CircBuff a -> CircBuff a
cbPushBack x inp@(CircBuff sz fr bu) =
  let ix = (fr + sz) `modl` 10 in CircBuff (sz + 1) fr (arrSet ix x bu)

{-@ cbPopBack
    :: inp: { v:CircBuff a | count v > 0 }
    ->      (a, {v:CircBuff a | (front v == front inp) &&
                                (count v == count inp - 1)
            })
@-}
cbPopBack :: CircBuff a -> (a, CircBuff a)
cbPopBack (CircBuff sz fr bu) =
  let ix = (fr + sz - 1) `modl` 10
  in  lemmaPopBack sz fr
        `seq` (arrAt ix bu, CircBuff (sz - 1) fr (arrUnset ix bu))

{-@ cbPopBack'
    :: inp: { v:CircBuff a | count v > 0 }
    ->      CircBuff a
@-}
cbPopBack' :: CircBuff a -> CircBuff a
cbPopBack' x = snd (cbPopBack x)

{-@ cbPushFront
    :: a
    -> inp: { v:CircBuff a | count v < 10 }
    -> { v:CircBuff a | (count v == count inp + 1) &&
                        (front v == (modr 10 (front inp - 1)))
       }
@-}
cbPushFront :: a -> CircBuff a -> CircBuff a
cbPushFront x (CircBuff sz fr bu) =
  let ix = 10 `modr` (fr - 1)
  in  lemmaPushFront1 sz fr `seq` CircBuff (sz + 1) ix (arrSet ix x bu)

{-@ cbPopFront
    :: inp: { v:CircBuff a | count v > 0 }
    ->      (a, {v:CircBuff a | (front v == modl (front inp + 1) 10) &&
                                (count v == count inp - 1)
            })
@-}
cbPopFront :: CircBuff a -> (a, CircBuff a)
cbPopFront (CircBuff sz fr bu) =
  lemmaPopFront1 sz fr
    `seq` lemmaPopFront2 sz fr
    `seq` (arrAt fr bu, CircBuff (sz - 1) ((fr + 1) `modl` 10) (arrUnset fr bu))

{-@ cbPopFront'
    :: inp: { v:CircBuff a | count v > 0 }
    ->      CircBuff a
@-}
cbPopFront' :: CircBuff a -> CircBuff a
cbPopFront' x = snd (cbPopFront x)

{-@ lemmaDifMod
    :: x:Nat
    -> y:{ v:Int | x < v && v - x < 10 }
    -> { modl x 10 /= modl y 10 }
@-}
lemmaDifMod :: Int -> Int -> Bool
lemmaDifMod x y
  | x < 10 && y < 10
  = x /= y === True
  | x < 10 && y >= 10
  = x /= modl (y - 10) 10 === True
  | x >= 10 && y >= 10
  = modl (x - 10) 10 /= modl (y - 10) 10 ==? True ? lemmaDifMod (x - 10)
                                                                (y - 10)
  | otherwise
  = impossible ()

{-@ lemmaValidIndex
    :: sz:{ v:Int | 0 < v && v <= 10 }
    -> fr:{ v:Int | Within 10 v }
    -> i: { v:Int | 0 <= i && i < sz }
    -> { Set_mem (modl (fr+i) 10) (calcCBIndices sz fr) }
@-}
lemmaValidIndex :: Int -> Int -> Int -> Bool
lemmaValidIndex sz fr i =
  member ((fr + i) `modl` 10) (calcCBIndices sz fr)
    -- sz > 0 branch of calcCBIndices since sz > i && i >= 0
    === member
          ((fr + i) `modl` 10)
          (union (singleton ((fr + sz - 1) `modl` 10))
                 (calcCBIndices (sz - 1) fr)
          )
    === if i >= sz
          then impossible ()
          else if i == sz - 1
            then
              member
                  ((fr + i) `modl` 10)
                  (union (singleton ((fr + sz - 1) `modl` 10))
                         (calcCBIndices (sz - 1) fr)
                  )
                === True
            else if i < sz - 1 && 0 < sz - 1
              then
                member ((fr + i) `modl` 10) (calcCBIndices (sz - 1) fr)
                ==? True
                ?   lemmaValidIndex (sz - 1) fr i
              else impossible ()

{-@ lemmaNotMember
    :: sz: { v:Int | 0 <= v && v <= 10 }
    -> fr: { v:Int | Within 10 v }
    -> i:  { v:Int | sz <= i && i < 10 }
    -> { Set_emp (Set_cap (calcCBIndices sz fr) (singleton (modl (fr + i) 10))) }
@-}
lemmaNotMember :: Int -> Int -> Int -> Bool
lemmaNotMember sz fr i =
  null (intersection (calcCBIndices sz fr) (singleton ((fr + i) `modl` 10)))
    === if sz == 0
          then null (intersection empty (singleton (modl (fr + i) 10))) === True
          else if sz > 0
            then
              null
                (intersection
                  (union (singleton ((fr + sz - 1) `modl` 10))
                         (calcCBIndices (sz - 1) fr)
                  )
                  (singleton ((fr + i) `modl` 10))
                )
              ==? null
                    (intersection (calcCBIndices (sz - 1) fr)
                                  (singleton ((fr + i) `modl` 10))
                    )
-- The intersection (union x y) z can be simplified to
-- intersection y z if we know x and z are different
              ?   (lemmaDifMod (fr + sz - 1) (fr + i))
              ==? True
              ?   lemmaNotMember (sz - 1) fr i
            else impossible ()

{-@ lemmaPopBack
    :: sz:{ v:Int | 0 < v && v <= 10 }
    -> fr:{ v:Int | Within 10 v }
    -> { calcCBIndices (sz - 1) fr ==
          Set_dif (calcCBIndices sz fr) (Set_sng (modl (fr + sz - 1) 10))
       }
@-}
lemmaPopBack :: Int -> Int -> Bool
lemmaPopBack sz fr =
  calcCBIndices (sz - 1) fr
    ==  difference (calcCBIndices sz fr) (singleton (modl (fr + sz - 1) 10))
    === if sz == 1
          then
            calcCBIndices 0 fr
            ==  delete (fr `modl` 10)
                       (union (singleton (fr `modl` 10)) (calcCBIndices 0 fr))


            === empty
            ==  delete (fr `modl` 10) (union (singleton (fr `modl` 10)) empty)


            === True
          else if sz > 1
            then
              calcCBIndices (sz - 1) fr
              ==  difference
                    (union (singleton ((fr + sz - 1) `modl` 10))
                           (calcCBIndices (sz - 1) fr)
                    )
                    (singleton ((fr + sz - 1) `modl` 10))

              ==? calcCBIndices (sz - 1) fr
              ==  (calcCBIndices (sz - 1) fr)
              ?   lemmaNotMember (sz - 1) fr (sz - 1)

              === True
            else impossible ()

{-@ lemmaExtendLeft
    :: sz:{ v:Int | 0 < sz && sz < 10 }
    -> fr:{ v:Int | Within 10 v }
    -> { Set_cup (Set_sng (modr 10 (fr - 1))) (calcCBIndices (sz - 1) fr)
           ==  calcCBIndices sz (modr 10 (fr-1))
       }
@-}
lemmaExtendLeft :: Int -> Int -> Bool
lemmaExtendLeft sz fr =
  union (singleton (10 `modr` (fr - 1))) (calcCBIndices (sz - 1) fr)
    ==  calcCBIndices sz (10 `modr` (fr - 1))
    === if sz == 1
          then
            singleton (10 `modr` (fr - 1))
            ==  calcCBIndices 1 (10 `modr` (fr - 1))
            === True
          else if sz > 1
            then

              union (singleton (10 `modr` (fr - 1))) (calcCBIndices (sz - 1) fr)

              ==  calcCBIndices sz (10 `modr` (fr - 1))

              === union
                    (singleton (10 `modr` (fr - 1)))
                    (union (singleton ((fr + sz - 2) `modl` 10))
                           (calcCBIndices (sz - 2) fr)
                    )
              ==  union (singleton (((10 `modr` (fr - 1)) + sz - 1) `modl` 10))
                        (calcCBIndices (sz - 1) (10 `modr` (fr - 1)))

              ==? union (singleton (10 `modr` (fr - 1)))
                        (calcCBIndices (sz - 2) fr)
              ==  (calcCBIndices (sz - 1) (10 `modr` (fr - 1)))
              ?   (lemmaPushFront2 (sz - 1) fr && lemmaExtendLeft (sz - 1) fr)

              === True
            else impossible ()

{-@ lemmaPushFront2
    :: sz:{ v:Int | 0 < sz && sz <= 10 }
    -> fr:{ v:Int | Within 10 v }
    -> { modl (fr + sz - 1) 10 == modl ((modr 10 (fr - 1)) + sz) 10 }
@-}
lemmaPushFront2 :: Int -> Int -> Bool
lemmaPushFront2 sz fr =
  (fr + sz - 1)
    `modl` 10
    ==     ((10 `modr` (fr - 1)) + sz)
    `modl` 10
    ===    if fr == 0
             then
               (sz - 1)
               `modl` 10
               ==     ((10 `modr` (-1)) + sz)
               `modl` 10
               ===    (sz - 1)
               ==     (9 + sz)
               `modl` 10
               ===    True
             else if fr > 0 then True else impossible ()

{-@ lemmaPushFront1
    :: sz:{v:Int | 0 <= sz && sz < 10 }
    -> fr:{v:Int | Within 10 v }
    -> { Set_cup (calcCBIndices sz fr) (Set_sng (modr 10 (fr - 1)))
            == calcCBIndices (sz + 1) (modr 10 (fr - 1))
       }
@-}
lemmaPushFront1 :: Int -> Int -> Bool
lemmaPushFront1 sz fr =
  union (calcCBIndices sz fr) (singleton (10 `modr` (fr - 1)))
    ==  calcCBIndices (sz + 1) (10 `modr` (fr - 1))
    === if sz == 0
          then
            singleton (10 `modr` (fr - 1))
            ==  singleton ((10 `modr` (fr - 1)) `modl` 10)
            === True
          else if sz > 0
            then
              union
                (union (singleton (10 `modr` (fr - 1)))
                       (calcCBIndices (sz - 1) fr)
                )
                (singleton ((fr + sz - 1) `modl` 10))
              ==  calcCBIndices (sz + 1) (10 `modr` (fr - 1))

              ==? union (calcCBIndices sz (10 `modr` (fr - 1)))
                        (singleton ((fr + sz - 1) `modl` 10))
              ==  calcCBIndices (sz + 1) (10 `modr` (fr - 1))
              ?   lemmaExtendLeft sz fr

              ==? True
              ?   lemmaPushFront2 sz fr
            else impossible ()

{-@ lemmaPopFront3
    :: sz:{ v:Int | 0 < sz && sz <= 10 }
    -> fr:{ v:Int | Within 10 v }
    -> { modl (fr + sz) 10 == modl ((modl (fr + 1) 10) + sz - 1) 10 }
@-}
lemmaPopFront3 :: Int -> Int -> Bool
lemmaPopFront3 sz fr
  | sz == 1
  = (fr + 1) `modl` 10 == ((fr + 1) `modl` 10) `modl` 10 === True
  | sz > 0 && fr == 9
  = modl (sz - 1) 10 == modl ((modl 0 10) + sz - 1) 10 === True
  | sz > 0 && fr < 9
  = (if (fr + sz) >= 10 then modl (fr + sz - 10) 10 else (fr + sz))
    ==  modl (fr + sz) 10
    === True
  | otherwise
  = impossible ()

{-@ lemmaPopFront2
    :: sz:{v:Int | 0 < v && v <= 10 }
    -> fr:{v:Int | Within 10 v }
    -> { Set_dif (calcCBIndices sz fr) (Set_sng fr)
          == calcCBIndices (sz - 1) (modl (fr + 1) 10)
       }
@-}
lemmaPopFront2 :: Int -> Int -> Bool
lemmaPopFront2 sz fr =
  difference (calcCBIndices sz fr) (singleton fr)
    ==  calcCBIndices (sz - 1) ((fr + 1) `modl` 10)
    === if sz == 1
          then difference (singleton fr) (singleton fr) == empty === True
          else if sz > 1
            then
              difference
                (union (singleton ((fr + sz - 1) `modl` 10))
                       (calcCBIndices (sz - 1) fr)
                )
                (singleton fr)
              ==  union (singleton ((sz - 2 + ((fr + 1) `modl` 10)) `modl` 10))
                        (calcCBIndices (sz - 2) ((fr + 1) `modl` 10))

              === union
                    (difference (singleton ((fr + sz - 1) `modl` 10))
                                (singleton fr)
                    )
                    (difference (calcCBIndices (sz - 1) fr) (singleton fr))
              ==  union (singleton ((sz - 2 + ((fr + 1) `modl` 10)) `modl` 10))
                        (calcCBIndices (sz - 2) ((fr + 1) `modl` 10))

              ==? union
                    (singleton ((fr + sz - 1) `modl` 10))
                    (difference (calcCBIndices (sz - 1) fr) (singleton fr))
              ==  union (singleton ((sz - 2 + ((fr + 1) `modl` 10)) `modl` 10))
                        (calcCBIndices (sz - 2) ((fr + 1) `modl` 10))
              ?   lemmaDifMod fr (fr + sz - 1)

              ==? True
              ?   (lemmaPopFront2 (sz - 1) fr && lemmaPopFront3 (sz - 1) fr)
            else impossible ()

{-@ lemmaPopFront1
    :: sz:{v:Int | 0 < v && v <= 10 }
    -> fr:{v:Int | Within 10 v }
    -> { Set_mem fr (calcCBIndices sz fr) }
@-}
lemmaPopFront1 :: Int -> Int -> Bool
lemmaPopFront1 sz fr = if sz == 1
  then
    member fr (union (singleton (fr `modl` 10)) (calcCBIndices 0 fr)) === True
  else if sz > 1
    then
      member
        fr
        (union (singleton ((fr + sz - 1) `modl` 10)) (calcCBIndices (sz - 1) fr)
        )
      ==? True
      ?   lemmaPopFront1 (sz - 1) fr
    else impossible ()

test1 :: CircBuff Integer
test1 = cbPushBack 0 cbEmpty

{-@ test2
    :: { v:CircBuff Integer | count v == 1 }
@-}
test2 :: CircBuff Integer
test2 = cbPushBack 0 cbEmpty

test3 :: Integer
test3 = cbAt 0 test2

test4 :: CircBuff Integer
test4 = cbPopBack' test2

{-@ test5
    :: { v:CircBuff Integer | count v == 1 }
@-}
test5 :: CircBuff Integer
test5 = cbPushFront 0 cbEmpty

test6 :: CircBuff Integer
test6 = cbPopFront' test5
