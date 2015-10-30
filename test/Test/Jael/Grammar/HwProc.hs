{-# Language NoImplicitPrelude, QuasiQuotes #-}

module Test.Jael.Grammar.HwProc
( gHwProcTests
) where

import ClassyPrelude
import Jael.Grammar
import Test.Framework as T
import Test.Framework.Providers.HUnit
--import Test.HUnit
import Test.Jael.Util

gHwProcTests :: [T.Test]
gHwProcTests =
  [ testCase "Parse valid hwproc" $ checkParsedTree pGHwProc hwProcTop
  ]

hwProcTop :: (Text, GHwProc)
hwProcTop = (pack [raw|
  hwproc SomeDev {
    interrupt 0x00100000;
    init = {
      letExpr = 2+4;
      ^c::x::y::z <- y;
    }
  }
|], GHwProc (GProcName (UIdent "SomeDev"))
            (HexInt "0x00100000")
            ( GProcLet (LIdent "letExpr")
                         (GEPlus (GEInt (DecInt "2")) (GEInt (DecInt "4")))
            $ GProcPut (GChan (GScopedIdent $
                          map (GScopeElem . LIdent) ["c", "x", "y", "z"]
                       ))
                       (GChanOrExprE $ GEVar (LIdent "y"))
            $ GProcInact
            )
  )

