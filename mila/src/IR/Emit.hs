module IR.Emit () where

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

codegenProg :: AST.Program -> LLVM ()
codegenProg (name, funcs, main@(_, _, _, vars, consts, body)) =
    define Type.i32 "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName 
            setBlock entry
            codegenStatement body >>= ret
            mapM_ codegenFunc funcs

codegenFunc :: AST.Function -> LLVM ()
codegenFunc (name, params, retType, vars, consts, body) =
    define (typeToLLVM retType) name args bls
    where
        args = map annotatedIdentifierToLLVM params
        bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM_ params $ \(name, typ) -> do
                var <- alloca (typeToLLVM typ)
                store var (local (LLVM.AST.mkName name) (typeToLLVM typ)) (typeToLLVM typ)
                assign name (gettype name) var
            codegenStatement body >>= ret
        
codegenExpr :: AST.Expression  -> Codegen LLVM.AST.Operand
codegenExpr (AST.VarRead str) = getvar str >>= load

codegenStatement :: AST.Statement -> Codegen LLVM.AST.Operand
codegenStatement _ = pure $ int32 (fromIntegral 42)

--
--emitProg :: AST.Program -> LLVM.AST.Module
--emitProg prog@(name, funcs, main) = 
--    flip evalState (LLVM)
--        $ buildModuleT ( strToSBS name )
--        $ do
--            print <- externVarArgs (LLVM.AST.mkName "printf") [charStar] integer
--            assign "print" print

emit :: LLVM.AST.Module -> IO BS.ByteString 
emit mod = withContext $ \context ->
    withModuleFromAST context mod moduleLLVMAssembly

--generateIR :: AST.Program -> IO BS.ByteString
--generateIR _ = emit test
