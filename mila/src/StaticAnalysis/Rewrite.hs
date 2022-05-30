module StaticAnalysis.Rewrite (replaceFunc, insertImplicitCasts) where

import Parse.AST
import StaticAnalysis.TypeCheck
import Control.Monad.State

funcReplacementRules :: String -> [Type] -> String
funcReplacementRules "write" [Double] = "writed"
funcReplacementRules "write" [String] = "writes"
funcReplacementRules "writeln" [Double] = "writelnd"
funcReplacementRules "writeln" [String] = "writelns"
funcReplacementRules "int" _ = "conv_int"
funcReplacementRules "float" _ = "conv_dbl"
funcReplacementRules str _ = str

replaceFunc :: Program -> Program
replaceFunc prog@(pname, fx, main) =
    (pname, map (replaceFuncF prog) fx, replaceFuncF prog main)

-- Replace function calls by it's typed-name equialent
replaceFuncF :: Program -> Function -> Function
replaceFuncF prog f@(fname, params, typ, vars, consts, body) =
    (fname, params, typ, vars, consts, stmtReplace (initialCtx prog f) body)
    where
        stmtReplace :: Context -> Statement -> Statement
        stmtReplace ctx (Block xs) = Block $ map (stmtReplace ctx) xs
        stmtReplace ctx (Assignment trg ex) = Assignment trg (expReplace ctx ex)
        stmtReplace ctx (Condition cond tru fals) =
            Condition (expReplace ctx cond) (stmtReplace ctx tru) (stmtReplace ctx <$> fals)
        stmtReplace ctx (WhileLoop cond body) = WhileLoop (expReplace ctx cond) (stmtReplace ctx body)
        stmtReplace ctx (ForLoop (initVar, initExp) iterOp cond body) =
            ForLoop (initVar, expReplace ctx initExp) (stmtReplace ctx iterOp)
                (expReplace ctx cond) (stmtReplace ctx body)
        stmtReplace ctx Exit = Exit
        stmtReplace ctx Break = Break
        stmtReplace ctx l@(Label _) = l
        stmtReplace ctx c@(ComeFrom _) = c
        stmtReplace ctx a@(Assert lhs rhs) = Assert (expReplace ctx lhs) (expReplace ctx rhs)
        stmtReplace ctx (ThrowawayResult exp) = ThrowawayResult $ expReplace ctx exp

        expReplace :: Context -> Expression -> Expression
        expReplace ctx l@(Literal _) = l
        expReplace ctx fc@(FunctionCall fname fargs) =
            FunctionCall (funcReplacementRules fname fargTypes) (map (expReplace ctx) fargs)
            where fargTypes = map (inferExp ctx) fargs
        expReplace ctx r@(VarRead _) = r
        expReplace ctx (Computation ari) = Computation $ arithReplace ctx ari

        arithReplace :: Context -> ExpArithmetics -> ExpArithmetics
        arithReplace ctx (EParens arith) = EParens $ arithReplace ctx arith
        arithReplace ctx (EBinOp op lhs rhs) =
            EBinOp op (arithReplace ctx lhs) (arithReplace ctx rhs)
        arithReplace ctx (ENegate arith) = ENegate $ arithReplace ctx arith
        arithReplace ctx (ENot arith) = ENot $ arithReplace ctx arith
        arithReplace ctx (EMinus arith) = EMinus $ arithReplace ctx arith
        arithReplace ctx (EExp exp) = EExp $ expReplace ctx exp


insertImplicitCasts :: Program -> Program
insertImplicitCasts prog@(pname, fx, main) =
    (pname, map (insertImplicitCastsF prog) fx, insertImplicitCastsF prog main)

insertImplicitCastsF :: Program -> Function -> Function
insertImplicitCastsF prog f@(fname, params, typ, vars, consts, body) =
    (fname, params, typ, vars, consts, insertImplicitCastsStmt (initialCtx prog f) (initialFunctionCtx prog) body)
    where
        insertImplicitCastsStmt :: Context -> FunctionContext -> Statement -> Statement
        insertImplicitCastsStmt ctx fctx (Block sx) = Block $ map (insertImplicitCastsStmt ctx fctx) sx
        insertImplicitCastsStmt ctx fctx stmt@(Assignment target expr) =
            let exprType = inferExp ctx expr in
                case (lookup target ctx, exprType) of
                    (Nothing, _) -> error $ "Variable " ++ target ++ " is not in typing context."
                    (Just varType, expType) -> Assignment target $ rewriteExpr ctx fctx varType expr
        insertImplicitCastsStmt ctx fctx (Condition cond truSt falsSt) =
            Condition (insertImplicitCastsExpr ctx fctx cond) (insertImplicitCastsStmt ctx fctx truSt) (insertImplicitCastsStmt ctx fctx <$> falsSt)
        insertImplicitCastsStmt ctx fctx (WhileLoop expr body) =
            WhileLoop (insertImplicitCastsExpr ctx fctx expr) (insertImplicitCastsStmt ctx fctx body)
        insertImplicitCastsStmt ctx fctx (ForLoop (iterVar, initExpr) iterOp cond body) =
            case lookup iterVar ctx of
                Nothing -> error $ "Variable " ++ iterVar ++ " is not in typing context."
                Just iterVarType ->
                    ForLoop (iterVar, rewriteExpr ctx fctx iterVarType initExpr) (insertImplicitCastsStmt ctx fctx iterOp)
                        cond (insertImplicitCastsStmt ctx fctx body)
        insertImplicitCastsStmt ctx _ stmt@Exit = stmt
        insertImplicitCastsStmt ctx _ stmt@Break = stmt
        insertImplicitCastsStmt ctx _ stmt@(Label _) = stmt
        insertImplicitCastsStmt ctx _ stmt@(ComeFrom _) = stmt
        insertImplicitCastsStmt ctx fctx stmt@(Assert lhs rhs) = Assert (insertImplicitCastsExpr ctx fctx lhs) (insertImplicitCastsExpr ctx fctx rhs)
        insertImplicitCastsStmt ctx fctx stmt@(ThrowawayResult expr) = ThrowawayResult $ insertImplicitCastsExpr ctx fctx expr

        -- expect function argument context and expression
        insertImplicitCastsExpr :: Context -> FunctionContext -> Expression -> Expression
        insertImplicitCastsExpr ctx fctx expr@(Literal _) = expr
        insertImplicitCastsExpr ctx fctx (FunctionCall fname args) =
            FunctionCall fname (zipWith (curry modArg) funcType args)
            where
                funcType = case lookup fname fctx of
                    Nothing -> error $ "Function " ++ fname ++ " is not in typing context."
                    Just ftype -> ftype
                modArg (argType, argExpr)
                    | argType == inferExp ctx argExpr = insertImplicitCastsExpr ctx fctx argExpr
                    | otherwise = case targetConv argType of
                        Just targetConvName -> FunctionCall targetConvName [insertImplicitCastsExpr ctx fctx argExpr]
                        Nothing -> insertImplicitCastsExpr ctx fctx argExpr
        insertImplicitCastsExpr ctx fctx expr@(VarRead _) = expr
        insertImplicitCastsExpr ctx fctx (Computation arith) = Computation $ insertImplicitCastsArith ctx fctx arith

        insertImplicitCastsArith :: Context -> FunctionContext -> ExpArithmetics -> ExpArithmetics
        insertImplicitCastsArith ctx fctx (EParens arith) = EParens $ insertImplicitCastsArith ctx fctx arith
        insertImplicitCastsArith ctx fctx (EBinOp op lhs rhs) =
            EBinOp op (insertImplicitCastsArith ctx fctx lhs) (insertImplicitCastsArith ctx fctx rhs)
        insertImplicitCastsArith ctx fctx (ENegate arith) = ENegate $ insertImplicitCastsArith ctx fctx arith
        insertImplicitCastsArith ctx fctx (EMinus arith) = EMinus $ insertImplicitCastsArith ctx fctx arith
        insertImplicitCastsArith ctx fctx (ENot arith) = ENot $ insertImplicitCastsArith ctx fctx arith
        insertImplicitCastsArith ctx fctx (EExp expr) = EExp $ insertImplicitCastsExpr ctx fctx expr

        targetConv :: Type -> Maybe String
        targetConv Integer = Just "conv_int"
        targetConv Double = Just "conv_dbl"
        targetConv _ = Nothing
        rewriteExpr :: Context -> FunctionContext -> Type -> Expression -> Expression
        rewriteExpr ctx fctx typ expr
            | typ == inferExp ctx expr = insertImplicitCastsExpr ctx fctx expr
            | otherwise = case targetConv typ of
                Just targetConvName -> FunctionCall targetConvName [insertImplicitCastsExpr ctx fctx expr]
                Nothing -> insertImplicitCastsExpr ctx fctx expr