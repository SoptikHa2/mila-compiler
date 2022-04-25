module Parse.ExpressionParser (expression, literal) where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec, sepBy, (<?>), between)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import Control.Applicative
import Lex.Tokens as Tokens
import Parse.AST as AST
import qualified Lex.Lexer as Lexer
import Text.Parsec.Token

expression :: Parsec String () Expression
expression = try (Computation <$> exprArithm) <|> try functionCall <|> try variableRead <|> try literal <?> "expression"

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
arithmTerm = (EExp <$> try expressionWithoutArithmetics) <|> try arithmParens

arithmParens :: Parser ExpArithmetics
arithmParens = EParens <$> between (arithmConsume "(") (arithmConsume ")") exprArithm

arithmTable = [
        [E.Prefix (ENot <$ arithmConsume "not"), E.Prefix (ENegate <$ arithmConsume "-"),
            E.Infix (ELand <$ arithmConsume "and") E.AssocLeft, E.Infix (ELor <$ arithmConsume "or") E.AssocLeft],
        [E.Infix (EEqual <$ arithmConsume "=") E.AssocLeft, E.Infix (ENequal <$ arithmConsume "<>") E.AssocLeft,
            E.Infix (ELeq <$ arithmConsume "<=") E.AssocLeft, E.Infix (EGeq <$ arithmConsume ">=") E.AssocLeft,
            E.Infix (ELt <$ arithmConsume "<") E.AssocLeft, E.Infix (EGt <$ arithmConsume ">") E.AssocLeft],
        [E.Infix (EMod <$ arithmConsume "mod") E.AssocLeft, E.Infix (EDiv <$ arithmConsume "div") E.AssocLeft,
            E.Infix (EMul <$ arithmConsume "*") E.AssocLeft ],
        [E.Infix (EAdd <$ arithmConsume "+") E.AssocLeft, E.Infix (ESub <$ arithmConsume "-") E.AssocLeft]
    ]

arithmConsume :: String -> Parser String
arithmConsume s = try (spaces >> string s <* spaces)