-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  expermental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.UTP.Manager
       ( -- * Manager
         Options (..)
       , Manager

         -- ** Initialization
       , newManager
       , closeManager
       , withManager

         -- ** Operations
       , Network.UTP.Manager.listen
       , Network.UTP.Manager.accept
       , Network.UTP.Manager.connect

         -- * Socket
       , Sock
       , Network.UTP.Manager.recv
       , Network.UTP.Manager.send
       , Network.UTP.Manager.close

         -- * Re-export
       , SockAddr (..)
       ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Default
import Data.Fixed
import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX
import Network.Socket as S
import Network.Socket.ByteString as BS

import Network.UTP.Packet as Pkt
import Network.UTP.Connection as Conn


data Options = Options
  { optMaxPacketSize :: {-# UNPACK #-} !Int

    -- | Maximum length to which queue of pending connections may
    -- grow. (Solaris style)
  , optBacklog       :: {-# UNPACK #-} !Int
  }

instance Default Options where
  def = Options
    { optMaxPacketSize = 2000
    , optBacklog       = 5
    }

data Manager = Manager
  { sock           :: !Socket
  , options        :: !Options
  , pendingAccept  :: !(TBQueue Connection)
  , connections    :: !(TVar    ConnectionMap)
  }

-- detect half-open connections and close them
-- Quiet Time Concept
{-----------------------------------------------------------------------
--  Initialization
-----------------------------------------------------------------------}

sockAddrFamily :: SockAddr -> Family
sockAddrFamily (SockAddrInet  _ _    ) = AF_INET
sockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
sockAddrFamily (SockAddrUnix  _      ) = AF_UNIX

bindServ :: SockAddr -> IO Socket
bindServ servAddr = do
  let family = sockAddrFamily servAddr
  sock <- socket family Datagram defaultProtocol
  when (family == AF_INET6) $ do
    setSocketOption sock IPv6Only 0
  bindSocket sock servAddr
  return sock

-- | Application can have arbitrary number of managers sitting on
-- distinct ports. Each manager can handle 65k connections per remote
-- (IP addr, port) pair.
newManager :: Options -> SockAddr -> IO Manager
newManager opts addr = do
  Manager <$> bindServ addr
          <*> pure     opts
          <*> newTBQueueIO (optBacklog opts)
          <*> newTVarIO def

closeManager :: Manager -> IO ()
closeManager Manager {..} = do
  -- TODO unblock all blocked send/recv
  -- TODO send ST_RESET to the all open connections!
  S.close sock

-- | Normally you need to use 'Control.Monad.Trans.Resource.allocate'.
withManager :: Options -> SockAddr -> (Manager -> IO a) -> IO a
withManager opts addr = bracket (newManager opts addr) closeManager

{-----------------------------------------------------------------------
--  Utils
-----------------------------------------------------------------------}

useconds :: NominalDiffTime -> Timestamp
useconds dt = fromIntegral (fromEnum (realToFrac dt :: Micro))

getTimestamp :: IO Timestamp
getTimestamp = useconds <$> getPOSIXTime

recvPacket :: Manager -> IO (Packet, SockAddr)
recvPacket m @ Manager {..} = do
  (bs, addr) <- BS.recvFrom sock (optMaxPacketSize options)
  case decode bs of
    Left  _   -> recvPacket m
    Right pkt -> return (pkt, addr)

sendPacket :: Manager -> Packet -> SockAddr -> IO ()
sendPacket Manager {..} pkt addr = BS.sendAllTo sock (encode pkt) addr

{-----------------------------------------------------------------------
--  Listener
-----------------------------------------------------------------------}

handlePacket :: Timestamp -> Connection -> Packet
             -> Manager -> STM [Packet]
handlePacket timestamp conn pkt @ Packet {..} m =
  case packetType of
    ST_DATA {..} -> return [] -- error "handlePacket"
    ST_FIN  {..} -> do putTMVar (closed conn) ()
                       return []
    ST_STATE
      | ackNr == isn -> do
        putTMVar (established conn) ()
        return []
      |   otherwise  -> return [] -- error "handlePacket"
    ST_RESET -> error "handlePacket"
    ST_SYN   -> do -- may arrive due to retransmission
      synAck <- acceptSYN timestamp conn pkt
      writeTBQueue (pendingAccept m) conn
      return [synAck]

persistId :: Packet -> ConnectionId
persistId Packet {..}
  | packetType == ST_SYN = senderId + 1
  |       otherwise      = senderId

listen :: Manager -> IO ()
listen m @ Manager {..} = forever $ do
  (pkt, addr) <- recvPacket m
  print pkt
  timestamp   <- getTimestamp
  let cid = persistId pkt
  print cid
  -- TODO respond with ST_RESET if we don't have such connection and
  -- pty /= ST_SYN
  conn        <- atomically $ getConnection timestamp addr cid connections
  print conn
  xs   <- atomically $ handlePacket timestamp conn pkt m
  forM_ xs $ \ x -> sendPacket m x addr
  -- TODO remove connection from connection table if handlePacket throws an
  -- exception

{-----------------------------------------------------------------------
--  QVar
-----------------------------------------------------------------------}

data QVar v r = QVar
  { qval :: MVar v
  , qres :: MVar r
  }

takeQVar :: QVar v r -> v -> IO r
takeQVar = error "takeQVar"

{-----------------------------------------------------------------------
--  Socket API
-----------------------------------------------------------------------}

data SocketFailure
  = ConnectionRefused

data Sock = Sock
  { sockAddr   :: !SockAddr
  , manager    :: !Manager
  , sockConn   :: !Connection

    -- | Number of bytes to receive.
  , recvBuf    :: !(MVar Int)

    -- | Recv result.
  , waitRecv   :: !(MVar BS.ByteString)

    -- | Enqueued write.
  , sendBuf    :: !(MVar BS.ByteString)

    -- | Number of bytes have been sent by the socket.
  , waitSend   :: !(MVar Int)
  }

nullSocket :: SockAddr -> Manager -> Connection -> IO Sock
nullSocket addr m c = Sock addr m c
  <$> newEmptyMVar <*> newEmptyMVar
  <*> newEmptyMVar <*> newEmptyMVar

-- | Returned socket /should/ be properly 'close'd after use.
--
--   This operation block until remote endpoint confirms this
--   connection request. Otherwise 'ConnectionRefused' will be thrown.
--
connect :: Manager -> SockAddr -> IO Sock
connect m @ Manager {..} addr = do
  timestamp <- getTimestamp
  conn <- atomically $ alloc addr connections
  syn  <- atomically $ initSYN timestamp conn
  sendPacket m syn addr
  _    <- atomically $ waitEstablished conn
  print "established"
  nullSocket addr m conn

-- | Unlike BSD sockets, here is no passive socket - manager used
-- instead. Multiple threads are /allowed/ to block on 'accept'
-- operation, though usually there is no reason to do this.
--
--   Returned socket /should/ be properly 'close'd after use.
--
accept :: Manager -> IO (Sock, SockAddr)
accept Manager {..} = error "accept"

shutdown :: Sock -> IO ()
shutdown = error "shutdown"

close :: Sock -> IO ()
close Sock {..} = do
  print "closing"
  timestamp <- getTimestamp
  fin       <- atomically $ initFIN timestamp sockConn
  print fin
  sendPacket manager fin sockAddr
  print "fin_sent"
  _         <- atomically $ waitClosed sockConn
  print "closed"

recv :: Sock -> Int -> IO BS.ByteString
recv Sock {..} n = do
  putMVar  recvBuf n
  takeMVar waitRecv

send :: Sock -> BS.ByteString -> IO Int
send Sock {..} bs = do
  putMVar  sendBuf bs
  takeMVar waitSend
