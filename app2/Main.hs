module Main (main) where

import qualified Control.Monad.State as M
import Iso
import OpenCog.MeTTa.Parser
import System.Environment

main :: IO ()
main = do
    [dir,path] <- getArgs
    file <- readFile path
    case M.evalStateT (apply parseMeTTa ()) (State {sText = file}) of
        Right a -> case M.execStateT (unapply parseFile a) (State {sText = ""}) of
                        Right s -> putStrLn $ sText s
                        Left e -> print e
        Left e -> print e
