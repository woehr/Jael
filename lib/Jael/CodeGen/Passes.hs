module Jael.CodeGen.Passes where

import LLVM.General
import LLVM.General.Context
import LLVM.General.Transforms
import LLVM.General.PassManager
import qualified LLVM.General.AST as P -- for pure

passes :: [Pass]
passes = []

passSet :: PassSetSpec
passSet = PassSetSpec { transforms = passes
                      , dataLayout = Nothing
                      , targetLibraryInfo = Nothing
                      , targetMachine = Nothing
                      }

runPasses :: P.Module -> IO (Either String P.Module)
runPasses highMod =
  -- The operations to run in llvm, LLVM.General.Module -> IO a
  let loweredAction lowMod = do
        _ <- withPassManager passSet (`runPassManager` lowMod)
        moduleAST lowMod
      ctxAction ctx = runExceptT $ withModuleFromAST ctx highMod loweredAction
   in withContext ctxAction

