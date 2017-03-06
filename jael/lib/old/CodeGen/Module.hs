module Jael.CodeGen.Module where

import LLVM.General.AST

-- TODO: Accept as a parameter the platform
mkModule :: Text -> Module
mkModule progName = Module
  { moduleName = unpack progName
  , moduleDataLayout = Nothing
  , moduleTargetTriple = Nothing
  , moduleDefinitions = []
  }

