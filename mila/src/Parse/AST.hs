module Parse.AST where

data Type
    = Integer
    | Double
    | Boolean
    | String
    | Nil
    | Ptr Type
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
    -- list of statements in block
    = Block [Statement]
    -- set variable [1:string] to value [2:expression]
    | Assignment String Expression
    -- if [1:expression] is true, run [2], else run [3]
    | Condition Expression Statement (Maybe Statement)
    -- while [1] is true, run [2]
    | WhileLoop Expression Statement
    -- 1. set [1.1] to [1.2]. Then run [4], each time executing [2],
    -- until [3] is true, then stop
    -- for (1.1 = 1.2; 3; 2) { 4; }
    | ForLoop (String, Expression) Statement Expression Statement
    -- return from function
    | Exit
    -- break out of a loop
    | Break
    -- jump to all corresponding comefrom's
    | Label String
    -- define where to jump from labels
    | ComeFrom String
    -- execute [1], and do nothing with the result
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
                    | ENegate ExpArithmetics -- binary negation
                    | EMinus ExpArithmetics -- number * -1
                    | ENot ExpArithmetics -- logic not
                    | EExp Expression
                    deriving (Show, Eq)

data ExpBinOp = EAdd | ESub | EMul | EDiv | EMod | EEqual | ENequal
              | ELand | ELor | ELt | ELeq | EGt | EGeq
              deriving (Show, Eq)

data ExpLiteral
    = IntegerLiteral Integer
    | DoubleLiteral Double
    | StringLiteral String
    deriving (Show, Eq)
