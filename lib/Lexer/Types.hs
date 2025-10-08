module Lexer.Types where

import Protolude (Eq, Int, Maybe, Show, Text)

data TokenType
  = -- Single-character tokens.
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | STAR
  | EOF
  | -- Literals
    IDENTIFIER
  | STRING
  | NUMBER
  | -- Reserved Keywords
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  deriving (Show, Eq)

data Token
  = Token
  { tokenType :: TokenType
  , tokenLexeme :: Text
  , tokenLiteral :: Maybe Text
  , tokenLine :: Int
  }
