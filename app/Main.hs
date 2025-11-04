module Main (main) where

import qualified Control.Monad.State as M
import Iso
import OpenCog.MeTTa.Parser
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Mode = ToMeTTa | ToHSM

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["to-metta", path] -> runConversion ToMeTTa path
        ["to-hsm", path] -> runConversion ToHSM path
        _ -> usage

runConversion :: Mode -> FilePath -> IO ()
runConversion mode path = do
    file <- readFile path
    let (parser, unparser) = case mode of
            ToMeTTa -> (parseFile, parseMeTTa)
            ToHSM -> (parseMeTTa, parseFile)
    case M.evalStateT (apply parser ()) (State {sText = file}) of
        Right value ->
            case M.execStateT (unapply unparser value) (State {sText = ""}) of
                Right s -> putStrLn $ sText s
                Left e -> print e
        Left e -> print e

usage :: IO ()
usage = do
    hPutStrLn stderr "Usage: hsmetta (to-metta|to-hsm) <path>"
    exitFailure
