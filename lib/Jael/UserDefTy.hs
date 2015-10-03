{-# Language NoImplicitPrelude #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Jael.UserDefTy where

import ClassyPrelude
import qualified Data.Set as S

-- User defined type, u, grammar input, g, the type of things added to an
-- environment, t, and the error, e
class (Show (TError u)) => UserDefTy u where
    type TGrammar u
    type TError u
    type TEnvItem u
    gToUserDefTy :: (TGrammar u) -> u
    validate     :: u -> Maybe (TError u)
    typeDeps     :: u -> S.Set Text
    envItems     :: (Text, u) -> [(Text, (TEnvItem u))]
    validate'    :: (Text, u) -> Either (TError u) [(Text, (TEnvItem u))]
    validate' x@(_, y) = maybe (Right $ envItems x) Left (validate y)

