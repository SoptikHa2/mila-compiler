{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
module IR.Codegen (codegenProgram)
where

import Parse.AST
import StaticAnalysis.TypeCheck

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
import qualified Control.Monad                 as M
import           Control.Monad.State
import           Data.String                    ( fromString )

import           Data.Word                      ( Word32 )
import           Data.List                      ( find )
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Maybe
import qualified LLVM.AST.AddrSpace

-- When using the IRBuilder, both functions and variables have the type Operand
data Env = Env { operands :: M.Map String Operand, function :: Function,
                 functions :: M.Map String Operand, strings :: M.Map String Operand,
                 finalizeBlock :: Maybe Name, program :: Program,
                 labels :: M.Map String [(Bool, Name)] }
  deriving (Eq, Show)

-- LLVM and Codegen type synonyms allow us to emit module definitions and basic
-- block instructions at the top level without being forced to pass explicit
-- module and builder parameters to every function
type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

milaStdlib :: [(String, [AST.Type], AST.Type)]
milaStdlib = map (\(n,a,r)->(n, map ltypeOfTyp a, ltypeOfTyp r)) stdlib

hasPtrArg :: String -> Int -> Bool
hasPtrArg str = hasPtrArg' milaStdlib str
  where
    hasPtrArg' :: [(String, [AST.Type], AST.Type)] -> String -> Int -> Bool
    hasPtrArg' [] str nth = False
    hasPtrArg' [(fn, tp, _)] str nth
      | fn == str = tp!!nth == AST.ptr AST.i32 -- TODO: more general way
      | otherwise = False
    hasPtrArg' ((fn, tp, _):xs) str nth
      | fn == str = tp!!nth == AST.ptr AST.i32 -- TODO: more general way
      | otherwise = hasPtrArg' xs str nth

registerOperand :: MonadState Env m => String -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

registerFunction :: MonadState Env m => String -> AST.Type -> [AST.Type] -> m ()
registerFunction fname fRetType fArgTypes =
  modify $ \env -> env { functions = M.insert fname fType (functions env) }
  where
    fType = AST.ConstantOperand $ C.GlobalReference fptp (mkName fname)
    fptp = AST.PointerType (AST.FunctionType fRetType fArgTypes False) (LLVM.AST.AddrSpace.AddrSpace 0)

setFunctionEnv :: MonadState Env m => Function -> m ()
setFunctionEnv fun = modify $ \env -> env { function = fun }

getCurrentFunction :: MonadState Env m => m Function
getCurrentFunction = gets function

getCurrentProgram :: MonadState Env m => m Program
getCurrentProgram = gets program

addLabel :: MonadState Env m => String -> Name -> m ()
addLabel comeFromId block = do
  labels <- gets labels
  let oldLabels = fromMaybe [] (M.lookup comeFromId labels)
  modify $ \env -> env { labels = M.insert comeFromId ((True, block) : oldLabels) labels }

getLabels :: MonadState Env m => String -> m [Name]
getLabels str = do
  lbls <- gets ((M.! str) . labels)
  return $ map snd lbls

useUpLabel :: MonadState Env m => String -> m Name
useUpLabel comeFromId = do
  labels <- gets labels
  let (newLabelList, nameToProcess) = getLabelAndUseItUp (labels M.! comeFromId)
  modify $ \env -> env { labels = M.insert comeFromId newLabelList labels }
  return nameToProcess
  where
    getLabelAndUseItUp :: [(Bool, Name)] -> ([(Bool, Name)], Name)
    getLabelAndUseItUp [] = error "Not label to use up. AST mismatch."
    getLabelAndUseItUp [(True, n)] = ([(False, n)], n)
    getLabelAndUseItUp ((True, n):xs) = ((False, n) : xs, n)
    getLabelAndUseItUp (fp@(False, _):xs) =
      let (lst, res) = getLabelAndUseItUp xs in
        (fp:lst, res)

ltypeOfTyp :: Type -> AST.Type
ltypeOfTyp Nil = AST.void
ltypeOfTyp Integer = AST.i32
ltypeOfTyp Boolean = AST.i32
ltypeOfTyp Double = AST.double
ltypeOfTyp String = AST.ptr AST.i8
ltypeOfTyp (Ptr t) = AST.ptr $ ltypeOfTyp t

locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result <- computation
  put oldState
  return result

strToSBS :: String -> ShortByteString
strToSBS = toShort . pack

ltypeFromLiteral :: ExpLiteral -> AST.Type
ltypeFromLiteral = ltypeOfTyp . typeFromLiteral

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr

codegenFunctionDef :: Function -> LLVM ()
codegenFunctionDef f@(name, params, retType, _, _, _) =
  registerFunction name (ltypeOfTyp retType) (map (ltypeOfTyp . snd) params)

codegenFunc :: Function -> LLVM ()
codegenFunc f@(name, args, retType, vars, consts, (_,body)) = mdo
  -- set function as currently working with
  setFunctionEnv f
  genComeFromLabels body
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
      -- Add parameters
      forM_ (zip ops args) $ \(op, (pname, ptype)) -> do
        addr <- L.alloca (ltypeOfTyp ptype) Nothing 0
        L.store addr 0 op
        registerOperand pname addr
      -- Add variables
      forM_ vars $ \(pname, ptype) -> do
        addr <- L.alloca (ltypeOfTyp ptype) Nothing 0
        registerOperand pname addr
      -- Add consts
      forM_ consts $ \(pname, lit) -> do
        addr <- L.alloca (ltypeFromLiteral lit) Nothing 0
        litop <- literalOperand lit
        L.store addr 0 litop
        registerOperand pname addr
      -- Add 'return variable' (if not procedure)
      if retType /= Nil then do
        retVarAddr <- L.alloca(ltypeOfTyp retType) Nothing 0
        registerOperand name retVarAddr
      else do pure ()
      -- Generate body itself
      codegenStatement body
      if retType /= Nil then do
        retVar <- codegenExpr (VarRead name)
        L.ret retVar
      else do L.retVoid
    genComeFromLabels :: Statement -> LLVM ()
    genComeFromLabels (Block stx) = mapM_ (genComeFromLabels . snd) stx
    genComeFromLabels (Condition _ (_,st) mst) = do
      genComeFromLabels st
      mapM_ genComeFromLabels (snd <$> mst)
    genComeFromLabels (WhileLoop _ (_,st)) = genComeFromLabels st
    genComeFromLabels (ComeFrom cfId) = do
      cmfrCnt <- comeFromsCnt cfId
      addLabel cfId (mkName (cfId ++ show cmfrCnt))
    genComeFromLabels _ = do return ()
    comeFromsCnt :: String -> LLVM Int
    comeFromsCnt cfId = do
      lbls <- gets labels
      case M.lookup cfId lbls of
        Nothing -> return 0
        Just a -> return $ length a

-- literal
literalOperand :: ExpLiteral -> Codegen Operand
literalOperand (IntegerLiteral val) = return $ L.int32 val
literalOperand (DoubleLiteral val) = return $ L.double val
literalOperand (StringLiteral str) = do
  strs <- gets strings
  case M.lookup str strs of
    Nothing -> do
      let nm = mkName (show (M.size strs) <> ".str")
      op <- L.globalStringPtr str nm
      modify $ \env -> env { strings = M.insert str (AST.ConstantOperand op) strs }
      pure (AST.ConstantOperand op)
    Just op -> return op

-- statements
codegenStatement :: Statement -> Codegen ()
-- block
codegenStatement (Block stmts) = mapM_ (codegenStatement . snd) stmts
-- assignment
codegenStatement (Assignment target expr) = do
  rTarget <- gets ((M.! target) . operands)
  rExpr <- codegenExpr expr
  L.store rTarget 0 rExpr
-- if
codegenStatement (Condition cond (_,truBody) falsBody) = mdo
  condResult <- codegenExpr cond
  L.condBr condResult thenBlock elseBlock
  thenBlock <- L.block `L.named` strToSBS "then"
  do
    codegenStatement truBody
    mkTerminator $ L.br mergeBlock
  elseBlock <- L.block `L.named` strToSBS "else"
  do
    mapM_ codegenStatement (maybeToList (snd <$> falsBody))
    mkTerminator $ L.br mergeBlock
  mergeBlock <- L.block `L.named` strToSBS "merge"
  return ()
-- throwaway
codegenStatement (ThrowawayResult exp) = M.void (codegenExpr exp)
-- while
codegenStatement (WhileLoop cond (_,body)) = mdo
  condResult <- codegenExpr cond
  L.condBr condResult whileBlock mergeBlock
  -- setup break target
  prevBlockVar <- gets finalizeBlock
  modify $ \env -> env { finalizeBlock = Just mergeBlock }
  -- start executing loop
  whileBlock <- L.block `L.named` strToSBS "whileLoop"
  do
    codegenStatement body
    condResult <- codegenExpr cond
    let retOp = L.condBr condResult whileBlock mergeBlock
    modify $ \env -> env { finalizeBlock = prevBlockVar }
    retOp
  mergeBlock <- L.block `L.named` strToSBS "merge"
  return ()
-- for
codegenStatement (ForLoop (var, initVal) iterOp cond body@(pos,bodyStmt)) = mdo
  codegenStatement (Assignment var initVal)
  -- we do a little bamboozle here
  codegenStatement
    (WhileLoop (Computation (EBinOp ENequal (EExp cond) (EExp (VarRead var)))) (pos, Block (body : [iterOp])))
-- exit
codegenStatement Exit = mdo
  fun <- getCurrentFunction
  if funType fun == Nil then L.retVoid else do
    retVar <- codegenExpr (VarRead (funName fun))
    mkTerminator $ L.ret retVar
codegenStatement (Assert lhs rhs) = mdo
  condRes <- codegenExpr (Computation (EBinOp ENequal (EExp lhs) (EExp rhs)))
  L.condBr condRes assertFailedBranch mergeBranch
  assertFailedBranch <- L.block `L.named` strToSBS "assertFail"
  do
    codegenExpr (FunctionCall "writelns" [Literal $ StringLiteral $ "Assertion failed: " ++ show lhs ++ "!=" ++ show rhs ++ "."])
    codegenExpr (FunctionCall "die" [])
    mkTerminator $ L.br mergeBranch
  mergeBranch <- L.block `L.named` strToSBS "merge"
  return ()
-- break
codegenStatement Break = do
  -- branch to current loop finalize block
  finBlock <- gets finalizeBlock
  case finBlock of
    Nothing -> error "Failed to break, there is no loop to break out of."
    Just block -> do
      mkTerminator $ L.br block
codegenStatement (ComeFrom name) = mdo
  mkTerminator $ L.br blockName
  blockName <- useUpLabel name
  L.emitBlockStart blockName
  codegenStatement Exit
codegenStatement (Label name) = mdo
  labels <- getLabels name
  case labels of
    [] -> error $ "No comeFrom's to jump from " ++ name
    [nm] -> mkTerminator $ L.br nm
    xs -> do
      -- call custom fork
      forkRes <- codegenExpr (FunctionCall "comeFrom" [Literal $ IntegerLiteral $ toInteger $ length xs])
      genMultipleJumps xs 0 forkRes
  mergeBlock <- L.block `L.named` strToSBS "labelOut"
  return ()
  where
    -- jumps to generate, current n# of jump gen, variable with fork result
    genMultipleJumps :: [Name] -> Int -> Operand -> Codegen ()
    genMultipleJumps [] _ _ = codegenStatement Exit
    genMultipleJumps (x:xs) n var = mdo
      -- generate n'th if
      condRes <- L.icmp IP.EQ (L.int32 $ toInteger n) var
      mkTerminator $ L.condBr condRes thenBlock mergeBlock
      thenBlock <- L.block `L.named` strToSBS "then"
      do
        mkTerminator $ L.br x
      mergeBlock <- L.block `L.named` strToSBS "merge"
      do
        genMultipleJumps xs (n+1) var

-- expressions
codegenExpr :: Expression -> Codegen Operand
-- literal
codegenExpr (Literal eLit) = literalOperand eLit
-- variable read
codegenExpr (VarRead name) = gets ((M.! name) . operands) >>= flip L.load 0
-- funciton call
codegenExpr (FunctionCall fname params) = do
  ps <- mapM (fmap (, []) . ( \(param, nth) ->
    -- there are some stdlib functions that modify variable, so sometimes we need to
    -- get a pointer to variable, instead of just it's value. But it is not refelcted in syntax.
    if hasPtrArg fname nth
      then
        extractPtr param
      else codegenExpr param )) (zip params [0..])
  fp <- gets ((M.! fname) . functions)
  L.call fp ps
  where
    extractPtr exp@(VarRead vname) = gets ((M.! vname) . operands)
    extractPtr exp@(Computation arith) = extractPtrFromArith arith
    extractPtr exp = error $ "Side-effect library function " ++ fname ++ " has to be called with variable, " ++ show exp ++ " is invalid."
    extractPtrFromArith (EExp exp) = extractPtr exp
    extractPtrFromArith (EParens arithm) = extractPtrFromArith arithm
    extractPtrFromArith arith = error $ "Side-effect library function " ++ fname ++ " has to be called with variable, " ++ show arith ++ " is invalid."
-- arighmetics
codegenExpr (Computation arithm) = codegenArithm arithm

castToFPArithmCodegen :: ExpArithmetics -> Codegen Operand
castToFPArithmCodegen arithm = do
  intCodegen <- codegenArithm arithm
  -- cast to float
  L.sitofp intCodegen AST.double

-- arighmetics
codegenArithm :: ExpArithmetics  -> Codegen Operand
-- (A)
codegenArithm (EParens arithm) = codegenArithm arithm
-- (A `x` B)
codegenArithm (EBinOp op lhs rhs) = do
  prog <- getCurrentProgram
  fun <- getCurrentFunction
  let initTypecheckingCtx = initialCtx prog fun

  let lhsType = inferArithm initTypecheckingCtx lhs
  let rhsType = inferArithm initTypecheckingCtx rhs

  -- decide how to codegen left and right side based on types
  let (isFP, lhs', rhs') = case (lhsType, rhsType) of
        (Integer, Integer) -> (False, codegenArithm lhs, codegenArithm rhs)
        (Double, Double) -> (True, codegenArithm lhs, codegenArithm rhs)
        (Double, Integer) -> (True, codegenArithm lhs, castToFPArithmCodegen rhs)
        (Integer, Double) -> (True, castToFPArithmCodegen rhs, codegenArithm lhs)
        (_, _) -> error $ "Unkown types of arithmetics: " ++ show lhsType ++ ", " ++ show rhsType

  let fun = case op of
        EAdd -> if isFP then L.fadd else L.add
        ESub -> if isFP then L.fsub else L.sub
        EMul -> if isFP then L.fmul else L.mul
        EDiv -> if isFP then L.fdiv else L.sdiv
        EMod -> L.srem
        EEqual -> if isFP then L.fcmp FP.UEQ else L.icmp IP.EQ
        ENequal -> if isFP then L.fcmp FP.UNE else L.icmp IP.NE
        ELeq -> if isFP then L.fcmp FP.ULE else L.icmp IP.SLE
        ELt -> if isFP then L.fcmp FP.ULT else L.icmp IP.SLT
        EGeq -> if isFP then L.fcmp FP.UGE else L.icmp IP.SGE
        EGt -> if isFP then L.fcmp FP.UGT else L.icmp IP.SGT
        ELand -> L.and
        ELor -> L.or

  lhsCg <- lhs'
  rhsCg <- rhs'
  fun lhsCg rhsCg
-- binary negation (xor with -1)
codegenArithm (ENegate arithm) = do
  ar' <- codegenArithm arithm
  m1 <- literalOperand $ IntegerLiteral $ -1
  L.xor ar' m1
-- logical not (param: %0)
--  %1 = icmp ne %0, 0
--  %2 = xor i1 %1, true
--  %3 = zext i1 %2 to i32
--  result is %3
codegenArithm (ENot arithm) = do
  ar' <- codegenArithm arithm
  op <- literalOperand $ IntegerLiteral 0
  p1 <- L.icmp IP.NE ar' op
  --p1asi1 <- L.zext p1 AST.i1
  p2 <- L.xor p1 (L.bit 1)
  L.zext p2 AST.i32
-- numerical negation (* -1)
codegenArithm (EMinus arithm) = codegenArithm (EBinOp EMul arithm (EExp $ Literal $ IntegerLiteral $ -1))
-- Expr
codegenArithm (EExp expr) = codegenExpr expr

-- generate builtins
codegenBuildIn :: (String, [AST.Type], AST.Type) -> LLVM ()
codegenBuildIn (name, args, retty) = do
  func <- L.extern (mkName name) args retty
  registerFunction name retty args

-- generate wrapper program
codegenProgram :: Program -> AST.Module
codegenProgram prog@(modName, funcs, main) =
  flip evalState (Env { operands = M.empty, function = main,
    functions = M.empty, strings = M.empty, finalizeBlock = Nothing,
    program = prog, labels = M.empty })
  $ L.buildModuleT (strToSBS modName)
  $ do
    mapM_ codegenBuildIn milaStdlib
    mapM_ codegenFunctionDef (main:funcs)
    mapM_ codegenFunc funcs
    codegenFunc main