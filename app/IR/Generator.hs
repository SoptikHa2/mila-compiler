module IR.Generator (generateFromTree) where

import Parse.AST
import Data.List (intercalate)

retType :: Type -> String
retType Nil = "void"
retType Integer = "i32"
retType Float = "float"

generateFuncArgs :: [AnnotatedIdentifier] -> [(String, Integer)] -> String
generateFuncArgs parameters registers =
    intercalate ", " $ map (\param@(pName, pType) -> retType pType ++ " %" ++ show (lookup pName registers)) parameters

generateFunction :: Function -> String
generateFunction (name, parameters, returnType, variables, consts, body) =
    "define " ++ retType returnType ++ " @" ++ name ++ "(" ++ generateFuncArgs parameters registers ++ ") {\n"

    ++ "}\n"
    -- mapping from variable name to register number
    -- there is a throwaway fake variable "__" to consume one number, that cannot be used b/c LLVM
    where registers = zip (map fst parameters ++ ["__"] ++ map fst variables ++ map fst consts) [0..]
          nextRegId = length registers

generateFromTree :: Program -> String
generateFromTree (progName, functions, (mainIdentifiers, mainConstIdentifiers, mainStatement)) =
    functionsIR ++ mainIR
    where
        moduleHeader = "; ModuleId = '" ++ progName ++ "'\n" -- TODO: source filename?
        functionsIR = foldl (++) moduleHeader (map generateFunction functions)
        mainIR = generateFunction ("main", [], Integer, mainIdentifiers, mainConstIdentifiers, mainStatement)
