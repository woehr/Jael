{-
Slightly modified version of
https://github.com/NixOS/cabal2nix/tree/ebnf-parser-for-nix
-}
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
      (bnfcProg,_,_) <- requireProgramVersion verbosity bnfcProgram (orLaterVersion (Version [2,7] [])) (withPrograms lbi)
      (happyProg,_,_) <- requireProgramVersion verbosity happyProgram (withinVersion (Version [1] [])) (withPrograms lbi)
      (alexProg,_,_) <- requireProgramVersion verbosity alexProgram (withinVersion (Version [3] [])) (withPrograms lbi)
      let back   = joinPath (replicate (length (splitDirectories outBaseDir)) "..")
          scope' = intercalate "." (splitDirectories (takeDirectory outRelativeFile))
          scope  = scope' ++ "." ++ takeFileName (dropExtension outRelativeFile)
      bracket (setCurrentDirectory outBaseDir) (\_ -> setCurrentDirectory back) $ \_ -> do
        rawSystemProgram verbosity bnfcProg
          [ "-haskell"
          , "-alex3"
          , "-d"
          , "-p", scope'
          , back </> inBaseDir </> inRelativeFile
          ]
        writeFile outRelativeFile $
          "module " ++ scope ++ "\n" ++
          "  (  module " ++ scope ++ ".Abs\n" ++
          "  ,  module " ++ scope ++ ".Lex\n" ++
          "  ,  module " ++ scope ++ ".Par\n" ++
          "  ,  module " ++ scope ++ ".Print\n" ++
          "  ,  module " ++ scope ++ ".ErrM\n" ++
          "  )\n" ++
          "  where\n" ++
          "import " ++ scope ++ ".Abs\n" ++
          "import " ++ scope ++ ".Lex\n" ++
          "import " ++ scope ++ ".Par\n" ++
          "import " ++ scope ++ ".Print\n" ++
          "import " ++ scope ++ ".ErrM\n" ++
          "\n"
        rawSystemProgram verbosity happyProg
         [ "-gcai"
         ,  dropExtension outRelativeFile </> "Par.y"
         ]
        rawSystemProgram verbosity alexProg
         [ "-g"
         ,  dropExtension outRelativeFile </> "Lex.x"
         ]
  }

