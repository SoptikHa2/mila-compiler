module IR.Emit () where

import IR.Codegen
import qualified Parse.AST as AST

import qualified Data.ByteString.Char8 as BS

import LLVM.Module
import LLVM.Context
import qualified LLVM.AST as AST

emit :: AST.Module -> IO BS.ByteString 
emit mod = withContext $ \context ->
    withModuleFromAST context mod moduleLLVMAssembly

--generateIR :: AST.Program -> IO BS.ByteString
--generateIR _ = emit test
