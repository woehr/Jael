module Jael.Data.MultiMap
( module MM
, union
, unionWith
, unions
, unionsWith
)
where

import Prelude (Ord, ($), map)
import Data.MultiMap as MM
import qualified Data.Map as M

union :: Ord k => MultiMap k a -> MultiMap k a -> MultiMap k a
union m1 m2 = fromMap $ M.union (toMap m1) (toMap m2)

unionWith :: Ord k => ([a] -> [a] -> [a])
          -> MultiMap k a -> MultiMap k a -> MultiMap k a
unionWith f m1 m2 = fromMap $ M.unionWith f (toMap m1) (toMap m2)

unions :: Ord k => [MultiMap k a] -> MultiMap k a
unions ms = fromMap $ M.unions (Prelude.map toMap ms)

unionsWith :: Ord k => ([a] -> [a] -> [a])
           -> [MultiMap k a] -> MultiMap k a
unionsWith f ms = fromMap $ M.unionsWith f (Prelude.map toMap ms)
