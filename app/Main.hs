module Main where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import Parser
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)
import System.Environment (getArgs)
import Prelude hiding (readFile)

run :: Text -> IO ()
run input =
    case parser input of
        Left err -> print err
        Right m -> putDoc $ pretty m

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= run
        _ -> putStrLn "Usage: rdm [FILE]"
