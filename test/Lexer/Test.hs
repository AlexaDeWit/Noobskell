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
    ]
