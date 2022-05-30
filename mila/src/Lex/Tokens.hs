module Lex.Tokens where

data Token
  = ProgramDirective
  | Identifier String
  | Function
  | Procedure
  | Forward
  | LeftParenthesis
  | RightParenthesis
  | SqLeftParenthesis
  | SqRightParenthesis
  | Colon
  | Comma
  | Semicolon
  | Dot
  | EqualSign
  | IntegerLiteral Integer
  | DoubleLiteral Double
  | StringLiteral String
  | IntegerType
  | DoubleType
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
  | Assert
  | Assignment
  | OpPlus
  | OpMinus
  | OpTimes
  | OpMod
  | OpDiv
  | ComeFrom
  | NoTok
  deriving (Show, Eq)