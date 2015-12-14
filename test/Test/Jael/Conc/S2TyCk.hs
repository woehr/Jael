module Test.Jael.Conc.S2TyCk
( procS2TyCkTests
) where

import qualified Data.Map as M
import           Jael.Conc.Proc
import           Jael.Conc.TyCk.S2
import qualified Test.Framework as T

procS2TyCkTests :: [T.Test]
procS2TyCkTests =
  [ testCase "seq let" $ checkS2Proc seqLet
  , testCase "put expr" $ checkS2Proc putExpr
  ]

checkS2Proc :: (Text, M.Map Text S2TopProc, M.Map Text S2PEx) -> Assertion
checkS2Proc = undefined

seqLet :: (Text, M.Map Text S2TopProc, M.Map Text S2PEx)
seqLet = ([raw|
|], M.fromList
    [ ("procName", undefined)
    ]
  , M.fromList
    [ ("procName$seq1", undefined)
    ]
  )

putExpr :: (Text, M.Map Text S2TopProc, M.Map Text S2PEx)
putExpr = ([raw|
|], M.fromList
    [ ("procName", undefined)
    ]
  , M.fromList
    [ ("procName$seq1", undefined)
    ]
  )

