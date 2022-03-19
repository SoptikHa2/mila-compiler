module AST where

data Type
    = Integer
    | Float
    | Nil
    deriving (Show)

type AnnotatedIdentifier = (String, Type)

-- program directive name, list of functions, (main with parameters and consts)
type Program = (String, [Function], ([AnnotatedIdentifier], [(String, ExpLiteral)], Statement))

type Function = (
    String, -- name
    [AnnotatedIdentifier], -- parameters
    Type, -- return type
    [AnnotatedIdentifier], -- variables
    [(String, ExpLiteral)], -- consts
    Statement
    )

data Statement
    = Block [Statement]
    | Assignment String Expression
    | Condition Expression Statement (Maybe Statement)
    | WhileLoop Expression Statement
    | Exit
    | ThrowawayResult Expression
    deriving (Show)

data Expression
    = Literal ExpLiteral
    | Mod Expression Expression
    | IsEqual Expression Expression
    | Not Expression
    | Plus Expression Expression
    | Minus Expression Expression
    | Times Expression Expression
    | Div Expression Expression
    | Lt Expression Expression
    | Gt Expression Expression
    | FunctionCall String [Expression]
    | VarRead String
    deriving (Show)

data ExpLiteral
    = IntegerLiteral Int
    deriving (Show)
