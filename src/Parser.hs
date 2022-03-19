module Parser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec, optionMaybe)
import Control.Applicative
import Tokens
import AST
import qualified Lexer

import StatementParser
import ExpressionParser

tokenToType :: Token -> Type
tokenToType IntegerType = Integer
tokenToType FloatType = Float
tokenToType _ = Nil

-- program factorial;
programHeader = Lexer.programDirective >> Lexer.identifierStr <* Lexer.semicolon

-- integer
dataType = tokenToType <$> (Lexer.colon >> (try Lexer.integerType <|> try Lexer.floatType))

-- answerToEverything : integer
identifierWithType = do
    name <- Lexer.identifierStr
    idDataType <- dataType
    return (name, idDataType)

-- function add (a:integer; b:integer) : integer;
functionHeader = do
    funcName <- Lexer.function >> Lexer.identifierStr
    Lexer.leftParen
    arguments <- many (try $ identifierWithType <* (try Lexer.semicolon <|> Lexer.nop))
    Lexer.rightParen
    returnType <- dataType
    Lexer.semicolon
    return (funcName, arguments, returnType)

-- var n: integer;
--     f: integer;
-- var k, h, l: float;
variableDeclarationBlock = (concat . concat) <$> (many1 (Lexer.var >> many1 variableDeclaration))

-- a, b, c: integer;
variableDeclaration = do
    -- comma separated identifiers of first type ...
    idents <- many1 (try $ Lexer.identifierStr <* (try Lexer.comma <|> Lexer.nop))
    identType <- dataType
    Lexer.semicolon
    return (map (\a -> (a, identType)) idents)

-- const A = 5;
--       B = 6;
-- const C = 7;
constDeclarationBlock = concat <$> many1 (Lexer.const >> many1 constDeclaration)

-- A=5;
constDeclaration = do
    ident <- Lexer.identifierStr
    Lexer.equalSign
    val <- literal
    Lexer.semicolon
    return (ident, val)

parseFunction = do
    header <- functionHeader
    variables <- variableDeclarationBlock
    body <- statement
    optionMaybe Lexer.semicolon
    return (header, variables, body)

main = ([], AST.Exit)

--program = do
--    programName <- programHeader
--    functions <- many func
--    mainFunc <- main
--    return (programName, func, mainFunc)
