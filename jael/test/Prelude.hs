module Prelude
( module X
, raw
) where

import ClassyPrelude as X hiding (assert)
import Test.Framework as X hiding (Test)
import Test.Framework.Providers.HUnit as X
import Test.Framework.Providers.QuickCheck2 as X
import Test.HUnit as X hiding (Testable)
import Test.QuickCheck as X hiding (Testable)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- https://hackage.haskell.org/package/raw-strings-qq
-- Extracted from dead-simple-json.
raw :: QuasiQuoter
raw = QuasiQuoter
  { quoteExp  = return . LitE . StringL
  , quotePat  = \_ -> fail "illegal raw string QuasiQuote (allowed as\
                          \ expression only, used as a pattern)"
  , quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as\
                          \ expression only, used as a type)"
  , quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as\
                          \ expression only, used as a declaration)"
  }

