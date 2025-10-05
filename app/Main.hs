module Main where

import qualified MyLib (someFunc)
import Prelude (IO, putStrLn)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
