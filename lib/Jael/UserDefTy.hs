{-# Language NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}

module Jael.UserDefTy where

import ClassyPrelude
import qualified Data.Set as S

-- User defined type, u, uniquely determines the grammar input, g, the type
-- of things added to an environment, t, and the error, e
class Show e => UserDefTy u g t e | u -> g t e
                                  , g -> u t e
                                  , e -> u g t
  where
    gToUserDefTy :: g -> u
    validate  :: u -> Maybe e
    typeDeps  :: u -> S.Set Text
    envItems  :: (Text, u) -> [(Text, t)]
    validate' :: (Text, u) -> Either e [(Text, t)]
    validate' x@(_, y) = maybe (Right $ envItems x) Left (validate y)

