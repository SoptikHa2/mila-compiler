module Lex.Tokens where

data Token
  = ProgramDirective
  | Identifier String
  | Function
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
  | ComeFrom
  | NoTok
  deriving (Show)