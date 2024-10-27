module Main where

import Language (Expr)
import Parser
import System.Exit (exitFailure, exitSuccess)

compile :: FilePath -> IO ()
compile _ = undefined

main :: IO ()
main = do
    let res = parser "VAR x : I64" :: Either String Expr
    case res of
        Right v -> print v >> exitSuccess
        Left _ -> putStrLn "Failure!" >> exitFailure
