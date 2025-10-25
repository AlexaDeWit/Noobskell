module Lexer.Scanning where

import Data.Char
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Lexer.Types (Token)
import qualified Lexer.Types as LexTypes
import Protolude (Maybe (..), (+), (.))

-- Placeholder implementation
-- Actual scanning logic to be implemented
scanTokens :: Text.Text -> Seq.Seq Token
scanTokens = LexTypes.scanResultTokens . scan

scan :: Text.Text -> LexTypes.ScanResult
scan = Text.foldl reducer LexTypes.initScanResult

addToken :: LexTypes.Token -> LexTypes.ScanResult -> LexTypes.ScanResult
addToken token result =
  result
    { LexTypes.scanResultTokens = LexTypes.scanResultTokens result |> token
    }

tokenOf :: LexTypes.TokenType -> Text.Text -> LexTypes.ScanResult -> LexTypes.Token
tokenOf tType lexeme scanResult =
  LexTypes.Token
    { LexTypes.tokenType = tType
    , LexTypes.tokenLexeme = lexeme
    , LexTypes.tokenLiteral = Nothing
    , LexTypes.tokenLine = LexTypes.lineNumber (LexTypes.currentContext scanResult)
    }

reducer :: LexTypes.ScanResult -> Char -> LexTypes.ScanResult
reducer acc c = case c of
  '\n' -> acc{LexTypes.currentContext = (LexTypes.currentContext acc){LexTypes.lineNumber = LexTypes.lineNumber (LexTypes.currentContext acc) + 1}}
  '(' -> addToken (tokenOf LexTypes.LEFT_PAREN "(" acc) acc
  ')' -> addToken (tokenOf LexTypes.RIGHT_PAREN ")" acc) acc
  '{' -> addToken (tokenOf LexTypes.LEFT_BRACE "{" acc) acc
  '}' -> addToken (tokenOf LexTypes.RIGHT_BRACE "}" acc) acc
  ',' -> addToken (tokenOf LexTypes.COMMA "," acc) acc
  '.' -> addToken (tokenOf LexTypes.DOT "." acc) acc
  '-' -> addToken (tokenOf LexTypes.MINUS "-" acc) acc
  '+' -> addToken (tokenOf LexTypes.PLUS "+" acc) acc
  ';' -> addToken (tokenOf LexTypes.SEMICOLON ";" acc) acc
  '*' -> addToken (tokenOf LexTypes.STAR "*" acc) acc
  _ -> acc
