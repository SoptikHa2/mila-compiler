module StaticAnalysis.TypeCheck where

import Data.Either
import Parse.AST

type Context = [(String, Type)]
type TypingResult = (Either String Type)

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
inferExp _ exp@(Literal (DoubleLiteral _)) = Float
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
        (Float, Integer) -> Float
        (Integer, Float) -> Float
        (Float, Float) -> Float
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
typeFromLiteral (DoubleLiteral _) = Float
typeFromLiteral (StringLiteral _) = String