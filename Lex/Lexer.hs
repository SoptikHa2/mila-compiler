module Lex.Lexer where

import Text.Parsec (spaces, alphaNum, string, char, oneOf, many1, try, digit, letter, Parsec)
import Control.Applicative ( (<|>), many )

import Lex.Tokens

reservedNames = ["program", "function", "var", "const", "integer", "float", "begin", "end",
                "if", "then", "else", "while", "do", "for", "to", "downTo", "break", "continue",
                "exit", "mod", "div", "comeFrom"]

programDirective :: Parsec String () Token
programDirective = spaces >> string "program" >> return ProgramDirective

identifierStr :: Parsec String () String
identifierStr = try $ do
  spaces
  lettersOrUnderscores <- many1 (letter <|> char '_')
  alphaNum <- many alphaNum
  let result = lettersOrUnderscores ++ alphaNum
  if result `elem` reservedNames then fail ("Identifier cannot be " ++ result) else return result

identifier :: Parsec String () Token
identifier = Identifier <$> identifierStr

function :: Parsec String () Token
function = spaces >> string "function" >> return Function

leftParen :: Parsec String () Token
leftParen = spaces >> char '(' >> return LeftParenthesis

rightParen :: Parsec String () Token
rightParen = spaces >> char ')' >> return RightParenthesis

sqLeftParen :: Parsec String () Token
sqLeftParen = spaces >> char '[' >> return SqLeftParenthesis

sqRightParen :: Parsec String () Token
sqRightParen = spaces >> char ']' >> return SqRightParenthesis

colon :: Parsec String () Token
colon = spaces >> char ':' >> return Colon

semicolon :: Parsec String () Token
semicolon = spaces >> char ';' >> return Semicolon

dot :: Parsec String () Token
dot = spaces >> char '.' >> return Dot

comma :: Parsec String () Token
comma = spaces >> char ',' >> return Comma

dollar :: Parsec String () Char
dollar = spaces >> char '$'

and :: Parsec String () Char
and = spaces >> char '&'

integerLiteral :: Parsec String () Token
integerLiteral = spaces >> do
  num <- (try (read <$> many1 digit)) <|>
    (try (read <$> ((++) "0x") <$> (char '$' >> (many1 (oneOf "1234567890abcdefABCDEF"))))) <|>
    (try (read <$> ((++) "0o") <$> (char '&' >> (many1 (oneOf "12345670")))))
  return (IntegerLiteral num)

-- todo: floats

integerType :: Parsec String () Token
integerType = spaces >> string "integer" >> return IntegerType

floatType :: Parsec String () Token
floatType = spaces >> string "float" >> return FloatType

var :: Parsec String () Token
var = spaces >> string "var" >> return Var

const :: Parsec String () Token
const = spaces >> string "const" >> return Const

begin :: Parsec String () Token
begin = spaces >> string "begin" >> return Begin

end :: Parsec String () Token
end = spaces >> string "end" >> return End

kIf :: Parsec String () Token
kIf = spaces >> string "if" >> return If

kThen :: Parsec String () Token
kThen = spaces >> string "then" >> return Then

kElse :: Parsec String () Token
kElse = spaces >> string "else" >> return Else

while :: Parsec String () Token
while = spaces >> string "while" >> return While

kDo :: Parsec String () Token
kDo = spaces >> string "do" >> return Do

for :: Parsec String () Token
for = spaces >> string "for" >> return For

to :: Parsec String () Token
to = spaces >> string "to" >> return To

downTo :: Parsec String () Token
downTo = spaces >> string "downTo" >> return DownTo

break :: Parsec String () Token
break = spaces >> string "break" >> return Break

continue :: Parsec String () Token
continue = spaces >> string "continue" >> return Continue

exit :: Parsec String () Token
exit = spaces >> string "exit" >> return Exit

assignment :: Parsec String () Token
assignment = spaces >> string ":=" >> return Assignment

equalSign :: Parsec String () Token
equalSign = spaces >> string "=" >> return EqualSign

opPlus :: Parsec String () Token
opPlus = spaces >> char '+' >> return OpPlus

opMinus :: Parsec String () Token
opMinus = spaces >> char '-' >> return OpMinus

opTimes :: Parsec String () Token
opTimes = spaces >> char '*' >> return OpTimes

opMod :: Parsec String () Token
opMod = spaces >> string "mod" >> return OpMod

opDiv :: Parsec String () Token
opDiv = spaces >> string "div" >> return OpDiv

label :: Parsec String () Token
label = spaces >> do
  idt <- identifierStr
  char ':'
  return (Label idt)

comeFrom :: Parsec String () Token
comeFrom = spaces >> string "comeFrom" >> spaces >>
  ComeFrom <$> identifierStr

nop :: Parsec String () Token
nop = return NoTok
