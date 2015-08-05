{-# Language NoImplicitPrelude #-}

module Test.Jael.Util
( raw
) where

import ClassyPrelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- https://hackage.haskell.org/package/raw-strings-qq
raw :: QuasiQuoter
raw = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL,

    quotePat  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a pattern)",
    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",
    quoteDec  = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a declaration)"
}

