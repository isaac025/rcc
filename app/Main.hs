module Main where

import Language
import Parser
import Semantics
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

compile :: FilePath -> IO ()
compile f = do
    contents <- readFile f
    case parser contents :: Either String Program of
        Left err -> putStrLn err >> exitFailure
        Right ast -> do
            stack <- initStack
            result <- semanticsEval stack (analyzeProgram ast)
            print result
            exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> compile f
        _ -> putStrLn "Usage: rcc FILE" >> exitFailure
