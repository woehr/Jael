-- https://github.com/NixOS/cabal2nix/blob/06dd5bcfd9c09583e6871fd74ddf77bddc7d36c6/Setup.hs
module Main ( main ) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Control.Exception
import System.Directory
import System.FilePath
import Distribution.Simple.Program
import Data.List

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       { hookedPrograms = [ bnfcProgram ]
       , hookedPreProcessors = [ ("cf", bnfc) ]
       }

bnfcProgram :: Program
bnfcProgram = (simpleProgram "bnfc")
  { programFindVersion = findProgramVersion "--numeric-version" id
  }

bnfc :: BuildInfo -> LocalBuildInfo -> PreProcessor
bnfc _ lbi = PreProcessor
  { platformIndependent = True
  , runPreProcessor = \(inBaseDir, inRelativeFile) (outBaseDir, outRelativeFile) verbosity -> do
      (bnfcProg,_,_) <- requireProgramVersion verbosity bnfcProgram (orLaterVersion (Version [2,8] [])) (withPrograms lbi)
      (happyProg,_,_) <- requireProgramVersion verbosity happyProgram (withinVersion (Version [1] [])) (withPrograms lbi)
      (alexProg,_,_) <- requireProgramVersion verbosity alexProgram (withinVersion (Version [3] [])) (withPrograms lbi)
      let back   = joinPath (replicate (length (splitDirectories outBaseDir)) "..")
          scope' = intercalate "." (splitDirectories (takeDirectory outRelativeFile))
          scope  = scope' ++ "." ++ takeFileName (dropExtension outRelativeFile)
      bracket (setCurrentDirectory outBaseDir) (\_ -> setCurrentDirectory back) $ \_ -> do
        rawSystemProgram verbosity bnfcProg
          [ "--haskell"
          , "--alex3"
          , "--ghc"
          , "-d"
          , "-p", scope'
          , back </> inBaseDir </> inRelativeFile
          ]
        rawSystemProgram verbosity happyProg
         [ "-gcai"
         ,  dropExtension outRelativeFile </> "Par.y"
         ]
        rawSystemProgram verbosity alexProg
         [ "-g"
         ,  dropExtension outRelativeFile </> "Lex.x"
         ]
  }