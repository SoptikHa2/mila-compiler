module StatementParser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, optionMaybe, Parsec, (<?>))
import Control.Applicative
import AST
import qualified Lexer
import ExpressionParser
import qualified Language.Haskell.TH.Syntax as Lexer

statement :: Parsec String () Statement
statement = try block <|> try condition <|> try whileLoop <|> try exit <|> try throwawayResult <?> "statement"

block :: Parsec String () Statement
block = do
    Lexer.begin
    st <- many statement
    Lexer.end
    return $ Block st

assignment :: Parsec String () Statement
assignment = do
    id <- Lexer.identifierStr
    Lexer.assignment
    value <- expression
    return $ Assignment id value

condition :: Parsec String () Statement
condition = do
    Lexer.kIf
    cond <- expression
    Lexer.kThen
    body <- statement
    -- potentially else branch
    elseBr <- optionMaybe (try $ Lexer.kElse >> statement)
    return $ Condition cond body elseBr

whileLoop :: Parsec String () Statement
whileLoop = do
    Lexer.while 
    cond <- expression
    Lexer.kDo
    body <- statement
    return $ WhileLoop cond body

exit :: Parsec String () Statement
exit = Lexer.exit >> Lexer.semicolon >> return AST.Exit

throwawayResult :: Parsec String () Statement
throwawayResult = do
    expr <- expression
    Lexer.semicolon
    return $ ThrowawayResult expr