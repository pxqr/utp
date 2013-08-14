module Main (main) where

import Control.Concurrent
import Network.UTP

port = 10000;

runServer :: IO ()
runServer = do
  s <- listenOn port
  print "listening"

  accept s
  print "accepted"

main :: IO ()
main = do
  forkIO runServer

  c <- socket
  connect c $ SockAddrInet port 0

  print "connected"
