module Main (main) where

import qualified Lexer.Test as Lexer
import Test.Tasty (defaultMain, testGroup)
import Prelude (IO, putStrLn)

main :: IO ()
main = defaultMain $ testGroup "All Tests" [Lexer.test]
