module StaticAnalysis.Rewrite (replaceFunc) where

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
    (pname, map (funcCallReplaceF prog) fx, funcCallReplaceF prog main)
    where
        initctx = map (\(fname, _, rtype, _, _, _) -> (fname, rtype)) fx

-- Replace function calls by it's typed-name equialent
funcCallReplaceF :: Program -> Function -> Function
funcCallReplaceF prog f@(fname, params, typ, vars, consts, body) =
    (fname, params, typ, vars, consts, stmtReplace (initialCtx prog f) body)
    where
        stmtReplace :: Context -> Statement -> Statement
        stmtReplace ctx (Block xs) = Block $ map (stmtReplace ctx) xs
        stmtReplace ctx (Assignment trg ex) = Assignment trg (expReplace ctx ex)
        stmtReplace ctx (Condition cond tru fals) =
            Condition (expReplace ctx cond) (stmtReplace ctx tru) (stmtReplace ctx <$> fals)
        stmtReplace ctx (WhileLoop cond body) = WhileLoop (expReplace ctx cond) (stmtReplace ctx body)
        stmtReplace ctx (ForLoop (initVar, initExp) iterOp cond body) =
            ForLoop (initVar, expReplace ctx initExp) (expReplace ctx iterOp)
                (expReplace ctx cond) (stmtReplace ctx body)
        stmtReplace ctx Exit = Exit
        stmtReplace ctx Break = Break
        stmtReplace ctx l@(Label _) = l
        stmtReplace ctx c@(ComeFrom _) = c
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