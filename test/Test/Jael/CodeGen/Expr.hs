{-# Language RecordWildCards #-}

-- Tests for generating expression code. Tests that program fragments are
-- converted as expected into un-optimized llvm ast fragments.

module Test.Jael.CodeGen.Expr
( codeGenExprTests
) where

import qualified Data.Map as M
import           LLVM.General.AST
import           LLVM.General.AST.Constant
import           LLVM.General.AST.Global
import qualified Test.Framework as T

codeGenExprTests :: [T.Test]
codeGenExprTests =
  [ testCase "generate a simple global expression" $ testGenerated simpleIntGlob
  , testCase "generate a simple function" $ testGenerated simpleFunc1
  ]

-- A set of parameters from the LLVM.General.AST.Global that we are interested
-- in testing. This is not the full list of possible parameters.
-- The testGenerated{Glob,Func} functions may ignore or require other values.
data TestParams = GlobParams
                    { gpName        :: Name
                    , gpType'       :: Type
                    , gpInitializer :: Constant
                    }
                | FuncParams
                    { fpName        :: Name
                    , fpReturnType  :: Type
                    , fpParameters  :: [Parameter]
                    , fpBasicBlocks :: [BasicBlock]
                    }
                deriving (Eq, Show)

-- A fragment of a program which is prepended to the input program of every test
testPrependInput :: Text
testPrependInput = [raw|
|]

-- Parses the first element as a program and return a map of generated LLVM
-- globals
generateExprAST :: Text -> M.Map Text Global
generateExprAST prog = undefined (testPrependInput <> prog) name

testGenerated :: (Text, M.Map Text TestParams) -> Assertion
testGenerated (prog, expected) =
  let genMap = generateExprAST prog
      (errMap, checkMap) = M.mapEither
        (\x -> case x of
                -- Cases for functions
                Function{parameters=(_,True)} -> Left "Varargs flag is set"
                Function{..} -> Right
                  FuncParams { fpReturnType = returnType
                             , fpName = name
                             , fpParameters = fst parameters
                             , fpBasicBlocks = basicBlocks
                             }
                -- Cases for global variables
                GlobalVariable{initializer=Nothing} -> Left "No global initializer"
                GlobalVariable{initializer=Just initializer, ..} -> Right
                  GlobParams { gpName = name
                             , gpType' = type'
                             , gpInitializer = initializer
                             }
                -- Not expecting aliases
                _ -> Left "Not a LLVM.General.AST.Global.{Function,GlobalVariable}"
        ) genMap
   in if not $ null errMap
         then assertFailure . unpack
              $ intercalate "\n"
              $ ("Errors:":)
              $ map (\(k, v) -> k <> " -> " <> v)
              $ M.toList errMap
         else assertEqual "" expected checkMap

simpleIntGlob :: (Text, M.Map Text TestParams)
simpleIntGlob = ([raw|
|], M.fromList
  [ ("", GlobParams { gpName=Name ""
                    , gpType'=VoidType
                    , gpInitializer=Null{constantType=VoidType}
                    }
    )
  ])

simpleFunc1 :: (Text, M.Map Text TestParams)
simpleFunc1 = ([raw|
|], M.fromList
  [ ("", FuncParams { fpName=Name ""
                    , fpReturnType=VoidType
                    , fpParameters=[]
                    , fpBasicBlocks=[]
                    }
    )
  ])

