module Test.Jael.CodeGen.Types
( codeGenTypeTests
) where

import           Jael.CodeGen.Convert
import           Jael.Seq.CG_Types
import           LLVM.General.AST
import qualified Test.Framework as T

codeGenTypeTests :: [T.Test]
codeGenTypeTests =
  [ testCase "Integer ranges represented with proper num bits" $ testGeneratedType intRangeTest
  ]

-- Tests that the first element of the input tuple converts to the second.
testGeneratedType :: [(CGTy, Type)] -> Assertion
testGeneratedType x = assertEqual "" (map snd x) (map (genCGTy . fst) x)

intRangeTest :: [(CGTy, Type)]
intRangeTest =
  [ (CGTySimple CGInt{cgIntMin=  0, cgIntMax=0}, IntegerType{typeBits=1})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=1}, IntegerType{typeBits=2})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=2}, IntegerType{typeBits=3})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=3}, IntegerType{typeBits=3})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=4}, IntegerType{typeBits=4})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=7}, IntegerType{typeBits=4})
  , (CGTySimple CGInt{cgIntMin=  0, cgIntMax=8}, IntegerType{typeBits=5})
  , (CGTySimple CGInt{cgIntMin= -1, cgIntMax=0}, IntegerType{typeBits=1})
  , (CGTySimple CGInt{cgIntMin= -2, cgIntMax=0}, IntegerType{typeBits=2})
  , (CGTySimple CGInt{cgIntMin= -3, cgIntMax=0}, IntegerType{typeBits=3})
  , (CGTySimple CGInt{cgIntMin= -4, cgIntMax=0}, IntegerType{typeBits=3})
  , (CGTySimple CGInt{cgIntMin= -5, cgIntMax=0}, IntegerType{typeBits=4})
  , (CGTySimple CGInt{cgIntMin= -8, cgIntMax=0}, IntegerType{typeBits=4})
  , (CGTySimple CGInt{cgIntMin= -9, cgIntMax=0}, IntegerType{typeBits=5})
  ]

