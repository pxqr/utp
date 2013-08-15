module Main (main) where

import Network.UTP
import System.Environment


main :: IO ()
main = do
  [str] <- getArgs
  let port = fromIntegral (read str :: Int)
  let addr = SockAddrInet port iNADDR_ANY

  sock <- socket
  putStrLn "socket opened"

  connect sock addr
  putStrLn $ "connected to " ++ show addr

  close sock
  putStrLn "socket closed"
