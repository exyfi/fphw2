module HW.Main where

import HW.Client (clientMain)
import HW.Server (serverMain)
import System.Environment
import System.IO
import System.Exit

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " <> name <> " server <port>"
    putStrLn $ "       " <> name <> " client <host> <port>"

args :: [String] -> IO ()
args ["client", host, port] = clientMain host (read port)
args ["server", port] = serverMain (read port)
args _ = usage

hwMain :: IO ()
hwMain = getArgs >>= args
