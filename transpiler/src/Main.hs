module Main where

import GoAST
import Parser
import Generator
import qualified UnitTest

main :: IO ()
main = UnitTest.main

trans :: String -> String -> IO ()
trans s f = do writeParse s f; writeGen (f ++ ".txt") f
