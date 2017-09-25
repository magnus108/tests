{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/sdiehl/kaleidoscope/blob/69267cd5c3bce513f8b17db0b6e896b228640cbe/src/chapter7/Main.hs
-- MORE MONAD FUN NEEDED
-- have fun
-- or you can generate code at each level.. either using string or using module for purity
-- HUsk emit.hs er eval functionene

module SimpleLLVM
where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import Data.ByteString.Char8 as BS

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                [],
          Name "gog" :=
            Add False False
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []
            ]

        (Do $ Ret (Just (LocalReference int (Name "result"))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
  }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm


main :: IO ()
main = toLLVM module_



