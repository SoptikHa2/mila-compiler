module Parse.AST where

data Type
    = Integer
    | Float
    | Boolean
    | Nil
    deriving (Show, Eq)

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
                    | EBinOp ExpBinOp ExpArithmetics ExpArithmetics
                    | ENegate ExpArithmetics
                    | ENot ExpArithmetics
                    | EExp Expression
                    deriving (Show)

data ExpBinOp = EAdd | ESub | EMul | EDiv | EMod | EEqual | ENequal
              | ELand | ELor | ELt | ELeq | EGt | EGeq
              deriving (Show)

data ExpLiteral
    = IntegerLiteral Integer
    deriving (Show)
