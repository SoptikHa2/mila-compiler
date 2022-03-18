module AST where

data Type
    = Integer
    | Float
    | Nil
    deriving (Show)

type AnnotatedIdentifier = (String, Type)

-- program directive name, list of functions, (main with parameters)
type Program = (String, [Function], ([AnnotatedIdentifier], Statement))

-- name, parameters, returnType, variables, body
type Function = (
    String,
    [AnnotatedIdentifier],
    Type,
    [AnnotatedIdentifier],
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
    = Literal
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
    deriving (Show)

