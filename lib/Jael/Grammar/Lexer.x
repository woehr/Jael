{
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}

module Jael.Grammar.Lexer where

import Jael.Grammar.Input
import Jael.Grammar.Token
}

$digit    = [0-9]
$binDigit = [01]
$octDigit = [0-7]
$hexDigit = [0-9a-fA-F]

$upper = [A-Z]
$lower = [a-z]
$alpha = [a-zA-Z]

$alphanum = [$alpha $digit]
$ident = [$alphanum _]

@intBin = "0b" $binDigit+
@intOct = "0o" $octDigit+
@intHex = "0x" $hexDigit+
@intDec = $digit+

@syms =
  
  -- Enclosures
  \( | \) |
  \[ | \] |
  \< | \> |
  \{ | \} |
  \(\| | \|\) |

  -- Other
  \= | \; |

  -- Arithmetic (unary)
  \~ |

  -- Arithmetic (binary)
  \+ | \- | \* | \/

  -- Comparison
  \=\= | \!\= |
  \> | \>\= |
  \< | \<\= |

  -- Logic (unary)
  \! |

  -- Logic (binary)
  \&\& | \|\| |  \=\=\> | \<\=\>

:-
  $white+         ;
  "//" .*         ;

  @syms           { mkToken (TokenSymbol . toS) }

  @intBin         { mkToken parseInteger }
  @intOct         { mkToken parseInteger }
  @intHex         { mkToken parseInteger }
  @intDec         { mkToken parseInteger }

  $lower $ident*  { mkToken lowerIdent }
  $upper $ident*  { mkToken upperIdent }

{

}
