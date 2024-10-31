module Main where

import Eval
import Language
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

compile :: FilePath -> IO ()
compile f = do
    contents <- readFile f
    case parser contents :: Either String Program of
        Left err -> putStrLn err >> exitFailure
        Right val -> do
            case runEval (eval val) of
                Left err -> putStrLn err >> exitFailure
                Right value -> print value >> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> compile f
        _ -> putStrLn "Usage: rcc FILE" >> exitFailure
