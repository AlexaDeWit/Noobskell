module Lexer.Test where

import qualified Lexer.Scanning as LS
import qualified Lexer.Types as LexTypes
import Protolude (pure, ($))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

test :: TestTree
test =
  testGroup
    "Lexer"
    [singleCharacterTokens]

singleCharacterTokens :: TestTree
singleCharacterTokens =
  testGroup
    "SingleCharacterTokens"
    [ testCase "Left Paren" $ do
        pure (LexTypes.initToken LexTypes.LEFT_PAREN "(") @=? LS.scanTokens "("
    , testCase "Right Paren" $ do
        pure (LexTypes.initToken LexTypes.RIGHT_PAREN ")") @=? LS.scanTokens ")"
    , testCase "Left Brace" $ do
        pure (LexTypes.initToken LexTypes.LEFT_BRACE "{") @=? LS.scanTokens "{"
    , testCase "Right Brace" $ do
        pure (LexTypes.initToken LexTypes.RIGHT_BRACE "}") @=? LS.scanTokens "}"
    , testCase "Comma" $ do
        pure (LexTypes.initToken LexTypes.COMMA ",") @=? LS.scanTokens ","
    , testCase "Dot" $ do
        pure (LexTypes.initToken LexTypes.DOT ".") @=? LS.scanTokens "."
    , testCase "Minus" $ do
        pure (LexTypes.initToken LexTypes.MINUS "-") @=? LS.scanTokens "-"
    , testCase "Plus" $ do
        pure (LexTypes.initToken LexTypes.PLUS "+") @=? LS.scanTokens "+"
    , testCase "Semicolon" $ do
        pure (LexTypes.initToken LexTypes.SEMICOLON ";") @=? LS.scanTokens ";"
    , testCase "Star" $ do
        pure (LexTypes.initToken LexTypes.STAR "*") @=? LS.scanTokens "*"
    ]
