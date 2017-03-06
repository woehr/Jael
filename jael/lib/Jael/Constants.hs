{-# Language FlexibleInstances #-}
{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}

module Jael.Constants
  ( intConst
  , add
  )
where

import           Jael.Prelude
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as F
import           Jael.Types

-- A class for things which can be made QTypes
class QTypable a where
  mkQType :: a -> QType

instance QTypable (TypeF QType) where
  mkQType t = F.trueReft :< t

instance QTypable (T.Text, TypeF QType) where
  mkQType (v, t) = F.reft (F.symbol v) F.PTrue :< t

instance QTypable QType where
  mkQType = id

binOp :: F.Expression a => F.Bop -> a -> a -> F.Expr
binOp o a b = F.EBin o (F.expr a) (F.expr b)

binRel :: F.Expression a => F.Brel -> a -> a -> F.Expr
binRel r a b = F.PAtom r (F.expr a) (F.expr b)

--infix 9 <=>
--(<=>) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a <=> b = F.PIff (F.expr a) (F.expr b)

--infix 8 .=>
--(.=>) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .=> b = F.PImp (F.expr a) (F.expr b)

--infix 7 .||
--(.||) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .|| b = F.expr a F.|.| F.expr b

--infix 6 .&&
--(.&&) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .&& b = F.expr a F.&.& F.expr b

infix 5 .=
(.=) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
a .= b = binRel F.Eq (F.expr a) (F.expr b)

--infix 5 ./=
--(./=) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a ./= b = binRel F.Ne (F.expr a) (F.expr b)

--infix 5 .>
--(.>) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .> b = binRel F.Gt (F.expr a) (F.expr b)

--infix 5 .<
--(.<) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .< b = binRel F.Lt (F.expr a) (F.expr b)

--infix 5 .>=
--(.>=) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .>= b = binRel F.Ge (F.expr a) (F.expr b)

--infix 5 .<=
--(.<=) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .<= b = binRel F.Le (F.expr a) (F.expr b)

--infixr 4 .*
--(.*) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .* b = binOp F.Times (F.expr a) (F.expr b)

--infixr 4 ./
--(./) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a ./ b = binOp F.Div (F.expr a) (F.expr b)

--infixr 4 .%
--(.%) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .% b = binOp F.Mod (F.expr a) (F.expr b)

infixr 4 .+
(.+) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
a .+ b = binOp F.Plus (F.expr a) (F.expr b)

--infixr 4 .-
--(.-) :: (F.Expression a, F.Expression b) => a -> b -> F.Expr
--a .- b = binOp F.Minus (F.expr a) (F.expr b)

infix 3 .:
(.:) :: T.Text -> TypeF QType -> (T.Text, TypeF QType)
v .: t = (v, t)

infix 2 .|
(.|) :: (T.Text, TypeF QType) -> F.Expr -> QType
(v, t) .| e = F.reft (F.symbol v) e :< t

infixl 1 .->
(.->) :: (QTypable a, QTypable b) => a -> b -> TypeF QType
t .-> u = TFunF (mkQType t) (mkQType u)

var :: T.Text -> F.Symbol
var = F.symbol

intConst :: Integer -> QScheme
intConst x = Scheme [] $ "v" .: TIntF .| var "v" .= x

add :: QScheme
add = Scheme [] $ mkQType $
      "x" .: TIntF
  .-> "y" .: TIntF
  .-> "z" .: TIntF .| var "z" .= var "x" .+ var "y"
