module Tokens where

data Token
  = ProgramDirective
  | Identifier String
  | Function
  | LeftParenthesis
  | RightParenthesis
  | SqLeftParenthesis
  | SqRightParenthesis
  | Colon
  | Comma
  | Semicolon
  | Dot
  | EqualSign
  | IntegerLiteral Int
  | FloatLiteral Float
  | IntegerType
  | FloatType
  | Var
  | Const
  | Begin
  | End
  | If
  | Then
  | Else
  | While
  | Do
  | For
  | To
  | DownTo
  | Break
  | Continue
  | Exit
  | Assignment
  | OpPlus
  | OpMinus
  | OpTimes
  | OpMod
  | OpDiv
  | Label String
  | ComeFrom String
  | NoTok
  deriving (Show)