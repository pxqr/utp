{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Network.UTP
       ( Socket, PortNumber, SockAddr(..), iNADDR_ANY
       , socket, close, withSocket

       , connect
       , bind, listen, accept
       , listenOn

       , send, recv
       , sendAll

       , userver, uclient
       ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Internal as BS
import Data.Word
import Network.Socket (iNADDR_ANY)
import Network.Socket.Internal

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.ForeignPtr


data SockStruct
type Socket = Ptr SockStruct
type CSockLen = CInt
-- TODO type Socket = MVar (ForeignPtr SockStruct) ?

foreign import ccall unsafe "usocket"
  c_socket :: IO Socket

foreign import ccall unsafe "uclose"
  c_close :: Socket -> IO CInt

foreign import ccall unsafe "uconnect"
  c_connect :: Socket -> Ptr SockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "ubind"
  c_bind :: Socket -> Ptr SockAddr -> CSockLen -> IO CInt

foreign import ccall unsafe "ulisten"
  c_listen :: Socket -> CInt -> IO CInt

foreign import ccall unsafe "uaccept"
  c_accept :: Socket -> Ptr SockAddr -> Ptr CSockLen -> IO Socket

foreign import ccall unsafe "urecv"
  c_recv :: Socket -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "usend"
  c_send :: Socket -> Ptr () -> CSize -> IO CInt

socket :: IO Socket
socket = throwErrnoIfNull "open" c_socket

close :: Socket -> IO ()
close sock =
  throwErrnoIfMinus1_ "close" $ do
    c_close sock

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket socket close

connect :: Socket -> SockAddr -> IO ()
connect sock addr =
  throwErrnoIfMinus1_ "connect" $ do
    withSockAddr addr $ \sock_addr sock_len -> do
      c_connect sock sock_addr (fromIntegral sock_len)

bind :: Socket -> SockAddr -> IO ()
bind sock addr =
  throwErrnoIfMinus1_ "bind" $ do
    withSockAddr addr $ \sock_addr sock_len -> do
      c_bind sock sock_addr (fromIntegral sock_len)

listen :: Socket -> Int -> IO ()
listen sock qlen =
  throwErrnoIfMinus1_ "listen" $ do
    c_listen sock (fromIntegral qlen)

defaultBacklog :: Int
defaultBacklog = 128

listenOn :: PortNumber -> IO Socket
listenOn port = do
  sock <- socket
  bind sock (SockAddrInet port iNADDR_ANY) `onException` close sock
  listen sock defaultBacklog  `onException` close sock
  return sock

-- TODO use Foreign.Marshal.Pool to avoid alloca?
accept :: Socket -> IO (Socket, SockAddr)
accept sock =
  withNewSockAddr AF_INET $ \sock_addr sock_len_val -> do
    with (fromIntegral sock_len_val) $ \sock_len -> do
      conn <- throwErrnoIfNull "accept" $ do
                c_accept sock sock_addr sock_len
      sockAddr <- peekSockAddr sock_addr
      return (conn, sockAddr)

recv :: Socket -> Int -> IO ByteString
recv sock len = do
  createAndTrim len $ \ptr -> do
    throwErrnoIfMinus1 "recv" $ do
      ret <- c_recv sock ptr (fromIntegral len)
      return $ fromIntegral ret

send :: Socket -> ByteString -> IO Int
send sock bs =
  throwErrnoIfMinus1 "send" $ do
    let (fptr, off, len) = toForeignPtr bs
    withForeignPtr fptr $ \ptr -> do
      ret <- c_send sock (ptr `plusPtr` off) (fromIntegral len)
      return $ fromIntegral ret

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs = do
  len <- send sock bs
  when (len /= BS.length bs) $ do
    sendAll sock $ BS.drop len bs

type Connection = (Socket, SockAddr)

userver :: PortNumber -> (Connection -> IO ()) -> IO ()
userver port action =
  bracket (listenOn port) close $ \sock -> do
    forever $ do
      conn <- accept sock
      forkIO $ action conn
      return ()

uclient :: SockAddr -> (Socket -> IO a) -> IO a
uclient addr action =
  withSocket $ \sock -> do
    connect sock addr
    action sock
