module Main (main) where

import qualified Lexer.Test as Lexer
import Protolude (IO, ($))
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "All Tests" [Lexer.test]
