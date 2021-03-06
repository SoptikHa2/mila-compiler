module Parse.StatementParser (statement) where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, optionMaybe, Parsec, (<?>))
import Control.Applicative
import Parse.AST as AST
import qualified Lex.Lexer as Lexer
import Parse.ExpressionParser
import qualified Lex.Tokens as Token

statement :: Parsec String () Statement
statement = try condition <|> try whileLoop <|> try forLoop <|> try assignment <|> try exit <|> try loopBreak <|> try label <|> try comeFrom <|> try block <|> try throwawayResult <?> "statement"

block :: Parsec String () Statement
block = do
    Lexer.begin
    st <- many (statement <* optionMaybe (try Lexer.semicolon <|> try Lexer.dot))
    Lexer.end
    optionMaybe (try Lexer.semicolon <|> try Lexer.dot)
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
    WhileLoop cond <$> statement

forLoop :: Parsec String () Statement
forLoop = do
    Lexer.for
    asgnTarget <- Lexer.identifierStr
    Lexer.assignment
    asgnValue <- expression
    upOrDown <- try Lexer.to <|> try Lexer.downTo
    targetValue <- expression
    Lexer.kDo
    let opName = if upOrDown == Token.To then "inc" else "dec"
    let iterationOp = FunctionCall opName [VarRead asgnTarget]
    ForLoop (asgnTarget, asgnValue) (ThrowawayResult iterationOp) targetValue <$> statement

exit :: Parsec String () Statement
exit = Lexer.exit >> return AST.Exit

throwawayResult :: Parsec String () Statement
throwawayResult = do
    expr <- expression
    return $ ThrowawayResult expr

loopBreak :: Parsec String () Statement
loopBreak = Lexer.break >> return AST.Break

label :: Parsec String () Statement
label = do
    Lexer.colon
    Label <$> Lexer.identifierStr

comeFrom :: Parsec String () Statement
comeFrom = do
    Lexer.comeFrom
    ComeFrom <$> Lexer.identifierStr
