module Parse.AST where

data Type
    = Integer
    | Float
    | Nil
    deriving (Show)

type AnnotatedIdentifier = (String, Type)
type ConstIdentifier = (String, ExpLiteral)

-- program directive name, list of functions, (main with parameters and consts)
type Program = (String, [Function], ([AnnotatedIdentifier], [ConstIdentifier], Statement))

type Function = (
    String, -- name
    [AnnotatedIdentifier], -- parameters
    Type, -- return type
    [AnnotatedIdentifier], -- variables
    [ConstIdentifier], -- consts
    Statement
    )

data Statement
    = Block [Statement]
    | Assignment String Expression
    | Condition Expression Statement (Maybe Statement)
    | WhileLoop Expression Statement
    | Exit
    | Break
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
    | Computation ExpArithmetics
    deriving (Show)

data ExpArithmetics = EParens ExpArithmetics
                    | EAdd ExpArithmetics ExpArithmetics
                    | ESub ExpArithmetics ExpArithmetics
                    | EMul ExpArithmetics ExpArithmetics
                    | EDiv ExpArithmetics ExpArithmetics
                    | EMod ExpArithmetics ExpArithmetics
                    | EEqual ExpArithmetics ExpArithmetics
                    | ENequal ExpArithmetics ExpArithmetics
                    | ELand ExpArithmetics ExpArithmetics
                    | ELor ExpArithmetics ExpArithmetics
                    | ELt ExpArithmetics ExpArithmetics
                    | EGt ExpArithmetics ExpArithmetics
                    | ENegate ExpArithmetics
                    | ENot ExpArithmetics
                    | EExp Expression
                    deriving (Show)

data ExpLiteral
    = IntegerLiteral Int
    deriving (Show)
