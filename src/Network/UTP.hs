{-# LANGUAGE ForeignFunctionInterface #-}
module Network.UTP
       ( c_socket
       ) where

foreign import ccall "usocket"
  c_socket :: IO ()