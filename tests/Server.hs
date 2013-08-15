module Main (main) where

import Control.Monad
import Network.UTP
import System.Environment


main :: IO ()
main = do
  [str] <- getArgs
  let port = fromIntegral (read str :: Int)
  let addr = SockAddrInet port iNADDR_ANY

  sock <- socket
  putStrLn "socket opened"

  bind sock addr
  putStrLn "socket bound"

--  listen sock 1
--  putStrLn "socket listening"

  _ <- forever $ do
    (_, conn_addr) <- accept sock
    putStrLn $ "accepted " ++ show conn_addr

  close sock
  putStrLn "socket closed"
