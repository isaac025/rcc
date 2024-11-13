module Main where

import System.Environment (getArgs)
import Data.Text.IO (readFile)
import Data.Text (Text)
import Prelude hiding (readFile)

run :: Text -> IO ()
run _ = undefined

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= run
        _ -> putStrLn "Usage: rdm [FILE]"
