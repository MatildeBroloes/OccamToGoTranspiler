module Main where

import GoAST
import Parser
import Generator
import qualified UnitTest

main :: IO ()
main = UnitTest.main --write [FFun "test" [] [] (SExit)] "tmp" --putStrLn "Hello, Haskell!"
