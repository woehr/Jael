module Prelude
( module X
) where

import BasePrelude as X hiding (Chan, shift)
--import ClassyPrelude as X
import MTLPrelude as X hiding (shift)
-- Things needed by bnfc generated code
--import Data.List as X (tail)
--import Text.Show as X (ShowS, showChar, showString, shows)

