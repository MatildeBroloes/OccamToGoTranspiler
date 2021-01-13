module BlackBox (main) where

import Parser
import System.IO

parseFile :: String -> String -> IO ()
parseFile fileName ref = do
                          file <- openFile fileName ReadMode
                          contents <- hGetContents file
                          case parseString contents of
                            Left e -> putStrLn $ "Error:" ++ (show e)
                            Right p -> do
                                        r <- openFile ref ReadMode
                                        c <- hGetContents r
                                        case (show p) ++ "\n" == c of
                                          True -> putStrLn $ "Success!"
                                          _ -> putStrLn $ "Error: incorrect parse: \n" ++ show p ++ "\n" ++ c
                                        hClose r
                          hClose file

test :: [String] -> IO ()
test (s:ss) = do
               putStr (s ++ ": ")
               parseFile ("../tests/test_examples/" ++ s ++ ".occ") ("../tests/test_examples/" ++ s ++ ".txt")
               test ss
test [] = putStrLn "Done!"

examples :: [String]
examples = ["test_basic1", "test_basic2", "test_basic3", "count"]

main :: IO ()
main = do
        putStrLn "------ Running test programs ------"
        test examples
