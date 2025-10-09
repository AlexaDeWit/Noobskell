module Lexer.Types where

import qualified Data.Sequence as Seq
import Protolude (Eq, Int, Maybe (..), Show, Text)

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
  -- ^ Line numbers are 1-indexed to fit with natural reading patterns
  }
  deriving (Eq, Show)

initToken :: TokenType -> Text -> Token
initToken tType lexeme =
  Token
    { tokenType = tType
    , tokenLexeme = lexeme
    , tokenLiteral = Nothing
    , tokenLine = 1
    }

data ScanningError
  = ScanningError
  { scanningErrorStart :: Int
  , scanningErrorEnd :: Int
  , scanningErrorText :: Text
  }
  deriving (Eq, Show)

makeScanningError :: Int -> Int -> Text -> ScanningError
makeScanningError start end text =
  ScanningError
    { scanningErrorStart = start
    , scanningErrorEnd = end
    , scanningErrorText = text
    }

data ScanResult = ScanResult
  { scanResultTokens :: Seq.Seq Token
  , scanResultErrors :: Seq.Seq ScanningError
  }
  deriving (Eq, Show)

initScanResult :: ScanResult
initScanResult =
  ScanResult
    { scanResultTokens = Seq.empty
    , scanResultErrors = Seq.empty
    }
