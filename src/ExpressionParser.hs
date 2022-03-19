
module ExpressionParser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec)
import Control.Applicative
import Tokens
import AST
import Lexer

expression :: Parsec String () Expression
expression = return AST.Literal

literal :: Parsec String () Expression
literal = return AST.Literal
