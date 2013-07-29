module Network.UTP.Socket where

import Control.Applicative
import Control.Concurrent
import Data.ByteString as BS
import Data.Serialize
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as BS

import Network.UTP.Packet


data SocketStatus = NotConnected
                  | Connected
--                  | Connected SockAddr ConnectionId
                  | Bound
                  | Listening
                  | Closed
                    deriving (Show, Eq)

data SocketState = SocketState
    { -- | advertised window from the other end;
      wndSize    :: {-# UNPACK #-} !Int

      -- | current number of bytes in-flight;
    , curWindow  :: {-# UNPACK #-} !Int

    , replyMicro :: {-# UNPACK #-} !Timestamp

      -- | next sequence number of last sent packet;
    , seqNr      :: {-# UNPACK #-} !PacketId

      -- | sequence number of last received packet;
    , ackNr      :: {-# UNPACK #-} !PacketId

    , status     ::                !SocketStatus
    } deriving Show

initialState :: SocketState
initialState = SocketState
    { wndSize    = defaultWindowSize
    , curWindow  = 0
    , replyMicro = 0
    , seqNr      = 0
    , ackNr      = 0
    , status     = NotConnected
    }

data Socket = Socket
    { -- | upper bound of the 'wndSize';
      maxWindow  :: {-# UNPACK #-} !Int
    , state      :: {-# UNPACK #-} !(MVar SocketState)
    , sock       :: N.Socket
    }

defaultWindowSize :: Int
defaultWindowSize = 10000

socket :: IO Socket
socket = Socket <$> pure     defaultWindowSize
                <*> newMVar  initialState
                <*> N.socket N.AF_INET N.Datagram N.defaultProtocol

close :: Socket -> IO ()
close Socket {..} =
  modifyMVar_ state $ \ st -> do
    -- TODO send fin
    N.close sock
    return $ st { status = Closed }

genConnectionId :: IO ConnectionId
genConnectionId = return 0

sendRecv :: N.Socket -> N.SockAddr -> Header -> IO Header
sendRecv sock addr hdr = do
  BS.sendTo sock (encode hdr) addr
  (resp, addr') <- BS.recvFrom sock headerSize
  if addr /= addr'
    then error "addr mismatch"
    else case decode resp of
      Left  msg  -> error  msg
      Right rhdr -> return rhdr

connect :: Socket -> N.SockAddr -> IO ()
connect Socket {..} addr = modifyMVar_ state connect'
  where
    connect' st @ SocketState {..}
      | status /= NotConnected = error $ "socket already " ++ show status
      | otherwise = do
        connId <- genConnectionId
        Header {..} <- sendRecv sock addr (synMsg connId)
        return $ st { status = Connected
                    , seqNr  = succ seqNr
                    , ackNr  = seqNum
                    }
      where
        synMsg connId = Header
          { ptype     = ST_SYN
          , version   = defaultVersion
          , extension = 0
          , connectionId = connId
          , timestampU   = 0
          , timestampDU  = 0
          , windSize     = fromIntegral wndSize
          , seqNum = 0
          , ackNum = 0
          }

bind :: Socket -> N.SockAddr -> IO ()
bind Socket {..} addr =
  modifyMVar_ state $ \ st -> do
    N.bindSocket sock addr
    return $ st { status = Bound
                }

listen :: Socket -> Int -> IO ()
listen Socket {..} qsize =
  modifyMVar_ state $ \st -> do
--    N.listen sock qsize
    return $ st { status = Listening
                }

listenOn :: N.PortNumber -> IO Socket
listenOn port = do
  sock <- socket
  bind sock $ N.SockAddrInet port 0
  listen sock N.maxListenQueue
  return sock

accept :: Socket -> IO (Socket, N.SockAddr)
accept Socket {..} = modifyMVar state accept'
  where
    accept' st @ SocketState {..}
      | status /= Listening = error $ "socket status" ++ show status
      | otherwise = do
        (msg, addr) <- BS.recvFrom sock headerSize
        BS.sendTo sock undefined addr

        conn   <- N.socket N.AF_INET N.Datagram N.defaultProtocol
        astate <- newMVar initialState
        return ( undefined
               , (Socket defaultWindowSize astate conn, addr)
               )

recv :: Socket -> Int -> IO ByteString
recv = undefined

send :: Socket -> ByteString -> IO Int
send = undefined
