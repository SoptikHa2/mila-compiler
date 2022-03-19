
module ExpressionParser where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec, sepBy, (<?>), between)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import Control.Applicative
import Tokens
import AST
import qualified Lexer
import Text.Parsec.Token

expression :: Parsec String () Expression
expression = try functionCall <|> try variableRead <|> try (Computation <$> exprArithm) <|> try literal <?> "expression"

expressionWithoutArithmetics :: Parsec String () Expression
expressionWithoutArithmetics = try functionCall <|> try variableRead <|> try literal <?> "expression"

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


-- Arithmetics

exprArithm :: Parser ExpArithmetics
exprArithm = E.buildExpressionParser arithmTable arithmTerm

arithmTerm :: Parser ExpArithmetics
arithmTerm = (EExp <$> expressionWithoutArithmetics) <|> arithmParens

arithmParens :: Parser ExpArithmetics
arithmParens = EParens <$> between (string "(") (string ")") exprArithm

arithmTable = [
        [E.Prefix (ENot <$ string "not"), E.Prefix (ENegate <$ string "-"),
            E.Infix (ELand <$ string "and") E.AssocLeft, E.Infix (ELor <$ string "or") E.AssocLeft],
        [E.Infix (EEqual <$ string "=") E.AssocLeft, E.Infix (ENequal <$ string "<>") E.AssocLeft,
            E.Infix (ELt <$ string "<") E.AssocLeft, E.Infix (EGt <$ string ">") E.AssocLeft],
        [E.Infix (EMod <$ string "mod") E.AssocLeft, E.Infix (EDiv <$ string "div") E.AssocLeft,
            E.Infix (EMul <$ string "*") E.AssocLeft ],
        [E.Infix (EAdd <$ string "+") E.AssocLeft, E.Infix (ESub <$ string "-") E.AssocLeft]
    ]
