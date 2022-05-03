module IR.Emit (emitIR) where

import IR.Codegen
import qualified Parse.AST as AST

import qualified Data.ByteString.Char8 as BS
import Control.Monad.State

import LLVM.Module
import LLVM.Context
import qualified LLVM.AST
import qualified LLVM.AST.Type as Type

emitIR :: AST.Program -> IO BS.ByteString
emitIR = modToIR . codegenProgram

modToIR :: LLVM.AST.Module -> IO BS.ByteString 
modToIR mod = withContext $ \context ->
    withModuleFromAST context mod moduleLLVMAssembly