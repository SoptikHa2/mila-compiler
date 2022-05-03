{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.Codegen (codegenProgram)
where

import Parse.AST

import qualified LLVM.AST.IntegerPredicate     as IP
import qualified LLVM.AST.FloatingPointPredicate
                                               as FP
import           LLVM.AST                       ( Operand )
import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Float                as AST
import qualified LLVM.AST.Type                 as AST
import qualified LLVM.AST.Constant             as C
import           LLVM.AST.Name
import           LLVM.AST.Typed                 ( typeOf )

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L

import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.String                    ( fromString )

import           Data.Word                      ( Word32 )
import           Data.List                      ( find )
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (ShortByteString, toShort)

-- When using the IRBuilder, both functions and variables have the type Operand
data Env = Env { operands :: M.Map String Operand }
  deriving (Eq, Show)

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

registerOperand :: MonadState Env m => String -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

ltypeOfTyp :: Type -> AST.Type
ltypeOfTyp Nil = AST.void
ltypeOfTyp Integer = AST.i32
ltypeOfTyp Boolean = AST.i32
ltypeOfTyp Float = AST.double

locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result <- computation
  put oldState
  return result

charStar :: AST.Type
charStar = AST.ptr AST.i8

strToSBS :: String -> ShortByteString
strToSBS = toShort . pack

codegenFunc :: Function -> LLVM ()
codegenFunc f@(name, args, retType, vars, consts, body) = mdo
  -- forward reference: to allow recursive calls
  registerOperand name function
  -- define body itself inside `locally` to prevent local variables from escaping scope
  function <- locally $ do
    let retty = ltypeOfTyp retType
    let fargs = mkParam args
    L.function (mkName name) fargs retty genBody
  return ()
  where
    fname = mkName name
    mkParam :: [AnnotatedIdentifier] -> [(AST.Type, L.ParameterName)]
    mkParam = map (\(pname, ptype) -> (ltypeOfTyp ptype, L.ParameterName $ strToSBS pname))
    genBody :: [Operand] -> Codegen ()
    genBody ops = do
      _entry <- L.block `L.named` strToSBS "entry"
      codegenStatement body

codegenStatement :: Statement -> Codegen ()
codegenStatement _ = L.retVoid

codegenProgram :: Program -> AST.Module
codegenProgram (modName, funcs, main@(_, params, _, vars, consts, body)) =
  flip evalState (Env { operands = M.empty })
  $ L.buildModuleT (strToSBS modName)
  $ do
    printf <- L.externVarArgs (mkName "printf") [charStar] AST.i32
    registerOperand "print" printf
    mapM_ codegenFunc funcs