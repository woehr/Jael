module Jael.Compile where

import Jael.Compile.Common
import Jael.Compile.Stage1
import Jael.Compile.Stage2

-- Split the compilation process into somewhat arbitrary stages to facilitate
-- easier testing.
compile :: Text -> CompileErrM Text
compile =
  -- Stage 1 parses the program and does basic sanity checks on various things
  -- later stages assume.
  -- Stage 1 returns the orders in which to process things and maps of names to
  -- structures for the various program constructs.
  stage1 >=>

  -- Stage 2 processes sequential constructs. It does type inference and
  -- constraint checking.
  -- Stage 2 returns:
  --   1) the resulting sequential type environment which sequential fragments
  --      in processes will need.
  --   2) an map of typed expressions, possibly still with type variables (at
  --      this point all constraints on types ints, bits, buffers should be
  --      known)
  stage2 >=>

  -- Stage 3 type checks all the processes in the program. This includes type
  -- checking/inference on sequential fragments within processes, which are
  -- given names and returned as functions. This could be handled differently
  -- in the future, perhaps for code space or performance, but for now this
  -- should suffice. I expect that the LLVM optimizer will take care of most
  -- redundancy passing arguments around.
  (\_ -> return "Unimplemented")

  -- Stage 4 monomorphizes all sequential code and converts it to the AST that
  -- is transformed to LLVM IR. The LLVM IR is optimized and the worst case
  -- execution time and space usage is calculated.

  -- Stage 5 simulates processes, converting them to straightline code in a
  -- representation that will be transformed to LLVM IR.
  -- As part of this simulation, the worst case execution time and space usage
  -- is determined between subsequent interrupts.

  -- Stage 6 analyzes the worst case time/space characteristics of the program
  -- ensuring that response times and memory capacity are not exceeded.

  -- Stage 7 does assembling and linking

