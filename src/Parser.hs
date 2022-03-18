module Parser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec)
import Control.Applicative
import Tokens
import AST
import Lexer

import StatementParser

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
    funcName <- Lexer.function >> identifierStr
    leftParen
    arguments <- many (try $ identifierWithType <* (try Lexer.semicolon <|> Lexer.nop))
    rightParen
    returnType <- dataType
    semicolon
    return (funcName, arguments, returnType)

-- var n: integer;
--     f: integer;
-- var k, h, l: float;
variableDeclarationBlock = (concat . concat) <$> (many1 (var >> many1 variableDeclaration))

-- a, b, c: integer;
variableDeclaration = do
    -- comma separated identifiers of first type ...
    idents <- many1 (try $ Lexer.identifierStr <* (try Lexer.comma <|> Lexer.nop))
    identType <- dataType
    semicolon
    return (map (\a -> (a, identType)) idents)

expression = semicolon -- TODO



parseFunction = do
    header <- functionHeader
    variables <- variableDeclarationBlock
    begin
    body <- expression
    end
    semicolon
    return (header, variables, body)

main = ([], AST.Exit)

--program = do
--    programName <- programHeader
--    functions <- many func
--    mainFunc <- main
--    return (programName, func, mainFunc)
