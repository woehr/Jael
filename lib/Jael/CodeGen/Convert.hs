module Jael.CodeGen.Convert where

import Jael.Seq.CG_AST
import Jael.Seq.CG_Types
import LLVM.General.AST

-- Functions for transforming Jael data structures into LLVM
-- Functions in this module are named gen<Jael data type> and generate an
-- appropriate LLVM data type

genCGTy :: CGTy -> Type
genCGTy = undefined

genCGEx :: CGEx -> [BasicBlock]
genCGEx = undefined

