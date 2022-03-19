
module ExpressionParser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec, sepBy, (<?>))
import Control.Applicative
import Tokens
import AST
import qualified Lexer

expression :: Parsec String () Expression
expression = try literal <|> try functionCall <|> try variableRead <?> "expression"

literal :: Parsec String () Expression
literal = try integerLiteral

integerLiteral :: Parsec String () Expression
integerLiteral = do
    res <- Lexer.integerLiteral
    let num = tokToInt res
    either fail (return . Literal . AST.IntegerLiteral) num
    where tokToInt :: Token -> Either String Int
          tokToInt (Tokens.IntegerLiteral num) = Right num
          tokToInt _ = Left "Expected integer literal"

functionCall :: Parsec String () Expression
functionCall = do
    funName <- Lexer.identifierStr
    Lexer.leftParen
    params <- expression `sepBy` Lexer.comma
    Lexer.rightParen
    return $ FunctionCall funName params

variableRead :: Parsec String () Expression
variableRead = VarRead <$> Lexer.identifierStr
