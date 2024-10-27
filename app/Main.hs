module Main where

import Parser
import System.Exit (exitFailure, exitSuccess)

compile :: FilePath -> IO ()
compile _ = undefined

main :: IO ()
main = do
    let res = parser "1 + 1 * 2" :: Either String Int
    case res of
        Right v -> print v >> exitSuccess
        Left _ -> putStrLn "Failure!" >> exitFailure
