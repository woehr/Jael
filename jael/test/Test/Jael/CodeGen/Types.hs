{-# Language FlexibleContexts #-}

module Test.Jael.CodeGen.Types
( codeGenTypeTests
) where

import           Jael.CodeGen.Convert
import           Jael.Seq.Types
import           Jael.Seq.Literal
import           LLVM.General.AST
import           LLVM.General.AST.Constant
import qualified Test.Framework as T

codeGenTypeTests :: [T.Test]
codeGenTypeTests =
  [ testCase "Integer ranges represented with proper num bits" $ testGeneratedLLVM intTypeTest
  , testCase "Int literal converted to expected values/types" $ testGeneratedLLVM intLitTest
  , testCase "Bitfield literal converted to expected values/types" $ testGeneratedLLVM bfLitTest
  , testCase "Bool literal converted to expected values/types" $ testGeneratedLLVM boolLitTest
  ]

-- Tests that the first element of the input tuple converts to the second.
testGeneratedLLVM :: (GenLLVM a, Eq a,              Show a,
                                 Eq (LLVMOutput a), Show (LLVMOutput a))
                  => [(a, LLVMOutput a)] -> Assertion
testGeneratedLLVM x = assertEqual "" (map snd x) (map (toLLVM . fst) x)

intTypeTest :: [(S2Ty, Type)]
intTypeTest =
  [ (S2TySimple BTInt{btIntMin =  0, btIntMax = 0}, IntegerType{typeBits = 1})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 1}, IntegerType{typeBits = 2})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 2}, IntegerType{typeBits = 3})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 3}, IntegerType{typeBits = 3})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 4}, IntegerType{typeBits = 4})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 7}, IntegerType{typeBits = 4})
  , (S2TySimple BTInt{btIntMin =  0, btIntMax = 8}, IntegerType{typeBits = 5})
  , (S2TySimple BTInt{btIntMin = -1, btIntMax = 0}, IntegerType{typeBits = 1})
  , (S2TySimple BTInt{btIntMin = -2, btIntMax = 0}, IntegerType{typeBits = 2})
  , (S2TySimple BTInt{btIntMin = -3, btIntMax = 0}, IntegerType{typeBits = 3})
  , (S2TySimple BTInt{btIntMin = -4, btIntMax = 0}, IntegerType{typeBits = 3})
  , (S2TySimple BTInt{btIntMin = -5, btIntMax = 0}, IntegerType{typeBits = 4})
  , (S2TySimple BTInt{btIntMin = -8, btIntMax = 0}, IntegerType{typeBits = 4})
  , (S2TySimple BTInt{btIntMin = -9, btIntMax = 0}, IntegerType{typeBits = 5})
  ]
bitStringToBitLiteral :: String -> Literal
bitStringToBitLiteral = LBit .
  map (\c -> case c of
                  '0' -> BVNotSet
                  '1' -> BVSet
                  _   -> error "Invalid character"
      )

bfLitTest :: [(Literal, Constant)]
bfLitTest =
  [ (bitStringToBitLiteral "1111",    Int{integerBits = 4, integerValue =  15})
  , (bitStringToBitLiteral "00000",   Int{integerBits = 5, integerValue =   0})
  , (bitStringToBitLiteral "001100",  Int{integerBits = 6, integerValue =  12})
  , (bitStringToBitLiteral "0000011", Int{integerBits = 7, integerValue =   3})
  ]

intLitTest :: [(Literal, Constant)]
intLitTest =
  [ (LInt   0,  Int{integerBits = 1, integerValue =  0})
  , (LInt   8,  Int{integerBits = 5, integerValue =  8})
  , (LInt (-8), Int{integerBits = 4, integerValue = -8})
  ]

boolLitTest :: [(Literal, Constant)]
boolLitTest =
  [ (LBool True,  Int{integerBits = 1, integerValue = -1})
  , (LBool False, Int{integerBits = 1, integerValue =  0})
  ]

