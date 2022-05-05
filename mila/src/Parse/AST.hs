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

-- Either a func (with return type) or procedure (without return type)
type Function = (
    String, -- name
    [AnnotatedIdentifier], -- parameters
    Type, -- return type
    [AnnotatedIdentifier], -- variables
    [ConstIdentifier], -- consts
    Statement
    )
funName :: Function -> String
funName (name, _, _, _, _, _) = name
funType :: Function -> Type
funType (_, _, typ, _, _, _) = typ

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
    deriving (Show, Eq)

data Expression
    = Literal ExpLiteral
    | FunctionCall String [Expression]
    | VarRead String
    | Computation ExpArithmetics
    deriving (Show, Eq)

data ExpArithmetics = EParens ExpArithmetics
                    | EBinOp ExpBinOp ExpArithmetics ExpArithmetics
                    | ENegate ExpArithmetics
                    | ENot ExpArithmetics
                    | EExp Expression
                    deriving (Show, Eq)

data ExpBinOp = EAdd | ESub | EMul | EDiv | EMod | EEqual | ENequal
              | ELand | ELor | ELt | ELeq | EGt | EGeq
              deriving (Show, Eq)

data ExpLiteral
    = IntegerLiteral Integer
    deriving (Show, Eq)
