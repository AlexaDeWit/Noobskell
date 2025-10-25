module Lexer.Scanning where

import Data.Char
import Data.Sequence ((><))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Lexer.Types (Token)
import qualified Lexer.Types as LexTypes
import Protolude (Maybe (..), pure, (.))

-- Placeholder implementation
-- Actual scanning logic to be implemented
scanTokens :: Text.Text -> Seq.Seq Token
scanTokens = LexTypes.scanResultTokens . scan

scan :: Text.Text -> LexTypes.ScanResult
scan text =
  -- A final pass on the reconciliation to avoid the off-by-one error
  -- due to the fact that it is in a sense a "lookahead" system
  reconcile scanningResult
 where
  scanningResult = Text.foldl reducer LexTypes.initScanResult text

tokenOf :: LexTypes.TokenType -> Text.Text -> LexTypes.ScanResult -> LexTypes.Token
tokenOf tType lexeme scanResult =
  LexTypes.Token
    { LexTypes.tokenType = tType
    , LexTypes.tokenLexeme = lexeme
    , LexTypes.tokenLiteral = Nothing
    , LexTypes.tokenLine = LexTypes.lineNumber (LexTypes.currentContext scanResult)
    }

reconcile :: LexTypes.ScanResult -> LexTypes.ScanResult
reconcile scanResult =
  scanResult
    { LexTypes.scanResultTokens = existingTokens >< newTokens
    }
 where
  existingTokens = LexTypes.scanResultTokens scanResult
  LexTypes.RefinementResult newTokens _ = LexTypes.scanResultRefinementResult scanResult

reducer :: LexTypes.ScanResult -> Char -> LexTypes.ScanResult
reducer acc c =
  reconciled
    { LexTypes.scanResultRefinementResult = nextRefinement
    }
 where
  reconciled = reconcile acc
  LexTypes.RefinementResult _ furtherRefinement = LexTypes.scanResultRefinementResult acc
  nextRefinement = case furtherRefinement of
    Just refineFunc -> refineFunc c
    Nothing -> newRefinement
  newRefinement =
    case c of
      '(' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.LEFT_PAREN "(" acc)) Nothing
      ')' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.RIGHT_PAREN ")" acc)) Nothing
      '{' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.LEFT_BRACE "{" acc)) Nothing
      '}' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.RIGHT_BRACE "}" acc)) Nothing
      ',' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.COMMA "," acc)) Nothing
      '.' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.DOT "." acc)) Nothing
      '-' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.MINUS "-" acc)) Nothing
      '+' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.PLUS "+" acc)) Nothing
      ';' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.SEMICOLON ";" acc)) Nothing
      '*' -> LexTypes.RefinementResult (pure (tokenOf LexTypes.STAR "*" acc)) Nothing
      _ -> LexTypes.RefinementResult Seq.Empty Nothing
