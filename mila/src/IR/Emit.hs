module IR.Emit (emitIR) where

import IR.Codegen
import qualified Parse.AST as AST

import qualified Data.ByteString.Char8 as BS
import Control.Monad.State

import LLVM.Module
import LLVM.Context
import qualified LLVM.AST
import qualified LLVM.AST.Type as Type

typeToLLVM :: AST.Type -> LLVM.AST.Type
typeToLLVM AST.Integer = Type.i32
typeToLLVM AST.Float   = Type.double
typeToLLVM AST.Boolean = Type.i32
typeToLLVM AST.Nil     = Type.void

annotatedIdentifierToLLVM :: AST.AnnotatedIdentifier -> (LLVM.AST.Type, LLVM.AST.Name)
annotatedIdentifierToLLVM (str, typ) = (typeToLLVM typ, LLVM.AST.mkName str)

emitIR :: AST.Program -> IO BS.ByteString
emitIR = modToIR . codegenProgram

modToIR :: LLVM.AST.Module -> IO BS.ByteString 
modToIR mod = withContext $ \context ->
    withModuleFromAST context mod moduleLLVMAssembly