module AST where

data Type
    = Integer
    | Float
    | Nil

type AnnonatedIdentifier = (String, Type)

-- program directive name, global variables, list of functions, main
type Program = (String, [AnnonatedIdentifier], [Function], Statement)

-- name, parameters, returnType, variables, body
type Function = (
    String,
    [AnnonatedIdentifier],
    Type,
    [AnnonatedIdentifier],
    Statement
    )

data Statement
    = Block [Statement]
    | Assignment String Expression
    | Condition Expression Statement (Maybe Statement)
    | WhileLoop Expression Statement
    | Exit
    | ThrowawayResult Expression

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

