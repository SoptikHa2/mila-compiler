module Parse.Parser (getAST) where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec, optionMaybe, option, parse, ParseError)
import Control.Applicative
import Lex.Tokens
import Parse.AST
import qualified Lex.Lexer as Lexer

import Parse.StatementParser
import Parse.ExpressionParser
import Data.Maybe

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
    -- optionally: forward keyword
    -- TODO: saner way to do this
    try (Lexer.forward >> Lexer.semicolon >> return (funcName, arguments,returnType, True)) <|> (Lexer.nop >> return (funcName, arguments, returnType, False))

-- var n: integer;
--     f: integer;
-- var k, h, l: float;
variableDeclarationBlock = try $ (concat . concat) <$> many1 (try (Lexer.var >> many1 variableDeclaration))

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
constDeclarationBlock = try $ concat <$> many1 (try (Lexer.const >> many1 constDeclaration))

-- A=5;
constDeclaration = do
    ident <- Lexer.identifierStr
    Lexer.equalSign
    Literal val <- literal
    Lexer.semicolon
    return (ident, val)

parseFunction :: Parsec String () (Maybe Function)
parseFunction = do
    (name, arguments, returnType, detached) <- functionHeader
    consts <- option [] constDeclarationBlock
    variables <- option [] variableDeclarationBlock
    if detached then return Nothing else do
        body <- statement
        return $ Just (name, arguments, returnType, variables, consts, body)

main :: Parsec String () ([AnnotatedIdentifier], [ConstIdentifier], Statement)
main = do
    consts <- option [] constDeclarationBlock
    variables <- option [] variableDeclarationBlock
    body <- statement
    return (variables, consts, body)

program :: Parsec String () Program
program = do
    programName <- programHeader
    functions <- many $ try parseFunction
    mainFunc <- main
    return (programName, catMaybes functions, mainFunc)

getAST :: String -> String -> Either Text.Parsec.ParseError Program
getAST = parse program
