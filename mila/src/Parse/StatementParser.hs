module Parse.StatementParser (statement) where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, optionMaybe, Parsec, (<?>), getPosition)
import Control.Applicative
import Parse.AST as AST
import qualified Lex.Lexer as Lexer
import Parse.ExpressionParser
import qualified Lex.Tokens as Token

statement :: Parsec String () PositionStatement
statement = try condition <|> try whileLoop <|> try forLoop <|> try assignment <|> try exit <|> try loopBreak <|> try label <|> try comeFrom <|> try assert <|> try block <|> try throwawayResult <?> "statement"

block :: Parsec String () PositionStatement
block = do
    spaces
    pos <- getPosition
    Lexer.begin
    st <- many (statement <* optionMaybe (try Lexer.semicolon <|> try Lexer.dot))
    Lexer.end
    optionMaybe (try Lexer.semicolon <|> try Lexer.dot)
    return (pos, Block st)

assignment :: Parsec String () PositionStatement
assignment = do
    spaces
    pos <- getPosition
    id <- Lexer.identifierStr
    Lexer.assignment
    value <- expression
    return (pos, Assignment id value)

condition :: Parsec String () PositionStatement
condition = do
    spaces
    pos <- getPosition
    Lexer.kIf
    cond <- expression
    Lexer.kThen
    body <- statement
    -- potentially else branch
    elseBr <- optionMaybe (try $ Lexer.kElse >> statement)
    return (pos, Condition cond body elseBr)

whileLoop :: Parsec String () PositionStatement
whileLoop = do
    spaces
    pos <- getPosition
    Lexer.while
    cond <- expression
    Lexer.kDo
    stmt <- statement
    return (pos, WhileLoop cond stmt)

forLoop :: Parsec String () PositionStatement
forLoop = do
    spaces
    pos <- getPosition
    Lexer.for
    asgnTarget <- Lexer.identifierStr
    Lexer.assignment
    spaces
    asgnPos <- getPosition
    asgnValue <- expression
    upOrDown <- try Lexer.to <|> try Lexer.downTo
    targetValue <- expression
    Lexer.kDo
    stmt <- statement
    let opName = if upOrDown == Token.To then "inc" else "dec"
    let iterationOp = FunctionCall opName [VarRead asgnTarget]
    return (pos, ForLoop (asgnTarget, asgnValue) (asgnPos, ThrowawayResult iterationOp) targetValue stmt)

exit :: Parsec String () PositionStatement
exit = do
    spaces
    pos <- getPosition
    Lexer.exit
    return (pos, AST.Exit)

assert :: Parsec String () PositionStatement
assert = do
    spaces
    pos <- getPosition
    Lexer.assert
    lhs <- expression
    rhs <- expression
    return (pos, Assert lhs rhs)

throwawayResult :: Parsec String () PositionStatement
throwawayResult = do
    spaces
    pos <- getPosition
    expr <- expression
    return (pos, ThrowawayResult expr)

loopBreak :: Parsec String () PositionStatement
loopBreak = do
    spaces
    pos <- getPosition
    Lexer.break
    return (pos, AST.Break)

label :: Parsec String () PositionStatement
label = do
    spaces
    pos <- getPosition
    Lexer.colon
    labelName <- Lexer.identifierStr
    return (pos, Label labelName)

comeFrom :: Parsec String () PositionStatement
comeFrom = do
    spaces
    pos <- getPosition
    Lexer.comeFrom
    comeFromName <- Lexer.identifierStr
    return (pos, ComeFrom comeFromName)
