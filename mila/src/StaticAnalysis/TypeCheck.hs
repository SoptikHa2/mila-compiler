{-# LANGUAGE BlockArguments #-}
module StaticAnalysis.TypeCheck where

import Data.Either
import Parse.AST

type Context = [(String, Type)]
type FunctionContext = [(String, [Type])] -- context for function arguments
type TypingResult = (Either String Type)

stdFunctionsNotYetResolved = [
    ("int", Integer),
    ("float", Double)
  ]

stdlib =
  [
    ("write",   [Integer],             Nil),
    ("writeln", [Integer],             Nil),
    ("readln",  [Ptr Integer],         Integer),
    ("dec",     [Ptr Integer],         Nil),
    ("inc",     [Ptr Integer],         Nil),

    ("writed",   [Double],             Nil),
    ("writelnd", [Double],             Nil),
    ("readlnd",  [Ptr Double],         Integer),

    ("writes",   [String],             Nil),
    ("writelns", [String],             Nil),

    ("conv_int", [Double],             Integer),
    ("conv_dbl", [Integer],            Double),

    ("setupSharedInts", [Integer],     Nil),
    ("getSharedInt", [Integer],        Integer),
    ("getAndIncrement", [Integer],     Integer),
    ("setSharedInt",[Integer, Integer],Nil),
    ("trashSharedInts", [],            Nil),

    ("comeFrom", [Integer],            Integer)
  ]

initialCtx :: Program -> Function -> Context
initialCtx prog@(pname, fx, main) f@(fname, params, typ, vars, consts, body) =
        stdFunctionsNotYetResolved ++ (map \(n,_,t)->(n,t)) stdlib ++ globalCtx ++
        params ++ vars ++ map annotateConst consts ++ [(fname, typ)]
        where
          globalCtx = map (\(fname, _, rtype, _, _, _) -> (fname, rtype)) fx

initialFunctionCtx :: Program -> FunctionContext
initialFunctionCtx prog@(_, fx, _) = map (\(fname, params, _, _, _, _) -> (fname, map snd params)) fx ++
  map (\(fname, args, _) -> (fname, args)) stdlib

-- check if expression matches given type, throws error otherwise
assertExp :: Context -> Expression -> Type -> Maybe String
assertExp ctx exp tp =
    if tp /= inferredType then
        Just $ "Type " ++ show inferredType ++ " of expression "
        ++ show exp ++ " doesn't match required type " ++ show tp ++ "."
    else Nothing
    where inferredType = inferExp ctx exp

inferExp :: Context -> Expression -> Type
-- literal
inferExp _ exp@(Literal (IntegerLiteral _)) = Integer
inferExp _ exp@(Literal (DoubleLiteral _)) = Double
inferExp _ exp@(Literal (StringLiteral _)) = String
inferExp ctx (FunctionCall fname _) = case lookup fname ctx of
  Nothing -> error $ "Function " ++ fname ++ " is not known at given context."
  Just ty -> ty
inferExp ctx (VarRead varname) = case lookup varname ctx of
  Nothing -> error $ "Variable " ++ varname ++ " is not known at given context."
  Just ty -> ty
inferExp ctx (Computation arith) = inferArithm ctx arith

inferArithm :: Context -> ExpArithmetics -> Type
inferArithm ctx (EParens arith) = inferArithm ctx arith
inferArithm ctx arithm@(EBinOp op lhs rhs)
    | op `elem` [EAdd, ESub, EMul, EDiv, EMod] = case (lhs', rhs') of
        (Double, Integer) -> Double
        (Integer, Double) -> Double
        (Double, Double) -> Double
        (Integer, Integer) -> Integer
        _ -> error $ "Failed to infer type of " ++ show arithm ++ ". Bad L/R operands type."
    | otherwise = Integer -- logical things
    where
        lhs' = inferArithm ctx lhs
        rhs' = inferArithm ctx rhs
inferArithm ctx (ENegate arithm) = inferArithm ctx arithm
inferArithm ctx (EMinus arithm) = inferArithm ctx arithm
inferArithm ctx (ENot arithm) = Integer
inferArithm ctx (EExp exp) = inferExp ctx exp

annotateConst :: ConstIdentifier -> AnnotatedIdentifier
annotateConst (str, lit) = (str, typeFromLiteral lit)

typeFromLiteral :: ExpLiteral -> Type
typeFromLiteral (IntegerLiteral _) = Integer
typeFromLiteral (DoubleLiteral _) = Double
typeFromLiteral (StringLiteral _) = String