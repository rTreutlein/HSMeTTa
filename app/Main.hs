module Main (main) where

import qualified Control.Monad.State as M
import Iso
import OpenCog.MeTTa.Parser
import System.Environment

main :: IO ()
main = do
    [path] <- getArgs
    file <- readFile path
    case M.evalStateT (apply parseFile ()) (State {sText = file}) of
        Right as -> case M.execStateT (unapply parseMeTTa as) (State {sText = ""}) of
                        Right s -> putStrLn $ sText s
                        Left e -> print e
        Left e -> print e
