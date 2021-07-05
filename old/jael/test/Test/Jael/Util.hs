module Test.Jael.Util where

import qualified Data.Set  as S
import qualified Data.Text as T

import Jael.HMInfer
import Jael.Pattern
import Jael.Type

toSet :: [T.Text] -> S.Set T.Text
toSet = S.fromList

rEmpty :: Row'
rEmpty = TRowEmpty

rVar :: T.Text -> Row'
rVar = TRowVar

rExt :: (T.Text, Type') -> Row' -> Row'
rExt = TRowExt

tVar :: T.Text -> Type'
tVar = TVar

tAll :: T.Text -> Type' -> Type'
tAll = TAll

tRec :: Row' -> Type'
tRec = TRec

pVar :: (T.Text, Type') -> TypedPat
pVar = PVar
