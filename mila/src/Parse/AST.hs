module Parse.AST where

data Type
    = Integer
    | Float
    | Nil
    deriving (Show)

type AnnotatedIdentifier = (String, Type)
type ConstIdentifier = (String, ExpLiteral)

-- program directive name, list of functions, Main (has to be named 'main' and have return type int)
type Program = (String, [Function], Function)

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
    | Label String
    | ComeFrom String
    | ThrowawayResult Expression
    deriving (Show)

data Expression
    = Literal ExpLiteral
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
                    | ELeq ExpArithmetics ExpArithmetics
                    | EGt ExpArithmetics ExpArithmetics
                    | EGeq ExpArithmetics ExpArithmetics
                    | ENegate ExpArithmetics
                    | ENot ExpArithmetics
                    | EExp Expression
                    deriving (Show)

data ExpLiteral
    = IntegerLiteral Int
    deriving (Show)
