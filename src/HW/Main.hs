module HW.Main where

import HW.Client (clientMain)
import HW.Server (serverMain)
import System.Environment
import System.IO
import System.Exit

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " <> name <> " <client|server>"

args :: [String] -> IO ()
args ["client"] = clientMain
args ["server"] = serverMain
args _ = usage

hwMain :: IO ()
hwMain = getArgs >>= args
