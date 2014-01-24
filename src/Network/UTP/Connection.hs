-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  expermental
--   Portability :  portable
--
--   A UTP connection is uniquely identified by a 6-tuple:
--
--   ( local   IP address, local   UDP port, local 'ConnectionId'
--   , foreign IP address, foreign UDP port, foreign 'ConnectionId')
--
--   where initiator_connection_id == acceptor_connection_id + 1.
--
{-# LANGUAGE RecordWildCards #-}
module Network.UTP.Connection
       ( -- * State transition
         -- $state-transitions

         -- * State
         Status     (..)
       , ConnState  (..)

         -- * Connection
       , Connection (sendId, recvId, established, closed)
       , isAccepted
       , isInitiated

         -- * Packet handling
       , acceptPacket

         -- ** Establishment
         -- $connection-establishment
       , initSYN
       , acceptSYN
       , waitEstablished

         -- ** Transfer
         -- $data-transfer
       , initDATA

         -- ** Finalization
         -- $connection-termination
       , initFIN
       , waitClosed

         -- ** Termination
       , initRESET

         -- * Connection table
       , ConnectionMap
       , Network.UTP.Connection.empty
       , Network.UTP.Connection.lookup
       , Network.UTP.Connection.insert
       , Network.UTP.Connection.delete

       , getConnection
       , alloc
       ) where

import Control.Applicative
import Control.Concurrent.STM
import Data.ByteString as BS
import Data.Default
import Data.IntMap as IM
import Data.Map as M
import Network.Socket (SockAddr)

import Network.UTP.Packet
import Network.UTP.Window as W


-- TODO handle simultaneous open: both ends decides to establish
-- connection with each other.
{-----------------------------------------------------------------------
--  Connection status
-----------------------------------------------------------------------}
-- $state-transitions
--
--   Normal state transition:
--
-- @
--  UNINIT >----> SYN_SENT >----> ESTABLISHED >----> FIN_SENT >--> CLOSED
--     |             |              |     |                           |
--     |             |              |     |                           |
--     .--------> SYN_RCVD >--------.     .--------> FIN_RCVD >-------.
-- @
--
--   TODO describe ST_RESET
--
--   [@UNINIT -> SYN_SENT@] SYN
--
--   [@UNINIT -> SYN_RCVD@] SYN
--
--   [@SYN_RCVD -> SYN_SENT@] SYN
--
--   [@SYN_SENT -> SYN_RCVD@] SYN
--
--   [@SYN_SENT -> ESTABLISHED@] SYN
--
--   [@SYN_RCVD -> ESTABLISHED@] SYN
--

-- | State of connection.
data Status
  = UNINIT

    -- | Half-open connection initiated by this endpoint. Sequence
    -- number is always 1;
  | SYN_SENT

    -- | Half-open connection initiated by remote endpoint. Sequence
    -- number is always 1;
  | SYN_RCVD

    -- | 'ST_DATA' can flow in both directions.
  | ESTABLISHED

    -- | Half-closed connection initiated by this endpoint. Data can
    -- flow only from remote to this endpoint.
  | FIN_SENT { eofPkt :: SequenceNr }

    -- | Half-closed connection initiated by remote endpoint. Data can
    -- flow only from this to the remote enpoint.
  | FIN_RCVD { eofPkt :: SequenceNr }

  | CLOSED
    deriving (Show, Eq)

data Foo
  = U
  | S { initiatedBy :: Bool, synNr :: SequenceNr }
  | E { initiatedBy :: Bool, synNr :: SequenceNr }
  | F { initiatedBy :: Bool, synNr :: SequenceNr
      , halfBy :: Bool, finNr :: SequenceNr }
  | C { initiatedBy :: Bool, synNr :: SequenceNr
      , halfBy :: Bool, finNr :: SequenceNr }

-- FIXME handle sequence numbers wrapping properly
-- | On incoming packet.
transition :: SequenceNr -> PacketType -> Status -> Maybe Status
transition _ ST_DATA {..}  UNINIT        = Nothing

-- note utp uses *two*-way handshake
-- this *may* happen if ST_DATA arrives before ST_STATE (out of order)
transition _ ST_DATA {..}  SYN_SENT      = Just SYN_SENT

-- initiating endpoint MUST not send data before we've replied with
-- ST_STATE
transition _ ST_DATA {..}  SYN_RCVD {..} = Nothing

-- note utp uses *two*-way handshake so we unable to track case when
-- ST_DATA have been sent *before* ST_STATE arrives to the remote
-- endpoint
transition _ ST_DATA {..}  ESTABLISHED   = Just ESTABLISHED

--
transition _ ST_DATA {..}  FIN_SENT {..} = Just (FIN_SENT eofPkt)
transition i ST_DATA {..}  FIN_RCVD {..}
  |       i < eofPkt                = Just (FIN_RCVD eofPkt)
  |       otherwise                 = Nothing
transition _ ST_DATA {..}  CLOSED        = Nothing

transition _ ST_FIN   UNINIT        = Nothing
transition _ ST_FIN   SYN_SENT      = undefined
transition _ ST_FIN   SYN_RCVD {..} = undefined
transition i ST_FIN   ESTABLISHED   = Just (FIN_RCVD i)
transition _ ST_FIN   FIN_SENT {..} = Just CLOSED
transition _ ST_FIN   FIN_RCVD {..} = Nothing
transition _ ST_FIN   CLOSED        = Nothing

transition _ ST_STATE UNINIT        = Nothing
transition _ ST_STATE SYN_SENT      = Just ESTABLISHED
transition _ ST_STATE SYN_RCVD {..} = undefined
transition _ ST_STATE ESTABLISHED   = Just ESTABLISHED
transition _ ST_STATE FIN_SENT {..} = Just (FIN_SENT eofPkt)
transition _ ST_STATE FIN_RCVD {..} = Just (FIN_RCVD eofPkt)
transition _ ST_STATE CLOSED        = undefined

transition _ ST_RESET UNINIT        = Nothing
transition _ ST_RESET SYN_SENT      = Just CLOSED

transition i ST_SYN   UNINIT
  |  i == isn                       = Just SYN_RCVD
  | otherwise                       = Nothing
transition _ ST_SYN   SYN_SENT      = Nothing

-- this *may* happen if remote endpoint did 1 or more ST_SYN
-- retransmissions
transition i ST_SYN   SYN_RCVD
  |         i == isn                = Just SYN_RCVD
  |         otherwise               = Nothing
transition _ ST_SYN   ESTABLISHED   = Nothing
transition _ ST_SYN   FIN_SENT {..} = Nothing
transition _ ST_SYN   FIN_RCVD {..} = Nothing
transition _ ST_SYN   CLOSED        = Nothing

-- FIXME handle sequence numbers wrapping properly
-- | On outcoming packet.
transit :: ConnectionId -> PacketType -> Status -> Maybe Status
transit    _ ST_DATA {..}  UNINIT        = Nothing
transit    _ ST_DATA {..}  SYN_SENT      = Nothing
transit    _ ST_DATA {..}  SYN_RCVD      = Nothing
transit    _ ST_DATA {..}  ESTABLISHED   = Just ESTABLISHED

-- this *may* happen if this endpoint want to *retransmit* data packet
transit    i ST_DATA {..}  FIN_SENT {..}
  |       i < eofPkt                = Just (FIN_SENT eofPkt)
  |       otherwise                 = Nothing

-- this endpoing is still allowed to send *new* data
transit    _ ST_DATA {..}  FIN_RCVD {..} = Just (FIN_RCVD eofPkt)
transit    _ ST_DATA {..}  CLOSED        = Nothing

-- MUST be impossible to do with sane socket API
transit    _ ST_FIN   UNINIT        = Nothing

-- MUST be impossible to do with sane socket API
transit    _ ST_FIN   SYN_SENT      = Nothing

-- MUST be impossible to do with sane socket API
transit    _ ST_FIN   SYN_RCVD      = Nothing
transit    _ ST_FIN   ESTABLISHED   = Just SYN_SENT

-- this happen when this endpoind decides to retransmit ST_FIN packet
transit    i ST_FIN   FIN_SENT {..}
  |        i == eofPkt              = Just (FIN_SENT eofPkt)
  |        otherwise                = Nothing
transit    _ ST_FIN   FIN_RCVD {..} = Just CLOSED
transit    _ ST_FIN   CLOSED   {..} = Nothing

-- there is no reason to do this
transit    _ ST_STATE UNINIT   {..} = Nothing

-- there is no reason to do this
transit    _ ST_STATE SYN_SENT      = Nothing

-- used to ack ST_SYN
transit    _ ST_STATE SYN_RCVD      = Just ESTABLISHED

-- used to ack ST_DATA
transit    _ ST_STATE ESTABLISHED   = Just ESTABLISHED

-- *may* be used to ack ST_DATA
transit    _ ST_STATE FIN_SENT {..} = Just (FIN_SENT eofPkt)

-- *may* be used to ack ST_DATA
transit    _ ST_STATE FIN_RCVD {..} = Just (FIN_RCVD eofPkt)
transit    _ ST_STATE CLOSED        = Nothing

-- there is no reason to do this
transit    _ ST_RESET UNINIT        = Nothing
transit    _ ST_RESET SYN_SENT      = Just CLOSED
transit    _ ST_RESET SYN_RCVD      = Just CLOSED
transit    _ ST_RESET ESTABLISHED   = Just CLOSED
transit    _ ST_RESET FIN_SENT {..} = Just CLOSED
transit    _ ST_RESET FIN_RCVD {..} = Just CLOSED

-- there is no reason to do this
transit    _ ST_RESET CLOSED        = Nothing

-- first initiated ST_SYN
transit    i ST_SYN   UNINIT
  |           i == isn              = Just SYN_SENT
  |           otherwise             = Nothing

-- initiated ST_SYN retransmission
transit    i ST_SYN   SYN_SENT
  |           i == isn              = Just SYN_SENT
  |           otherwise             = Nothing

transit    _ ST_SYN   SYN_RCVD      = Nothing
transit    _ ST_SYN   ESTABLISHED   = Nothing
transit    _ ST_SYN   FIN_SENT {..} = Nothing
transit    _ ST_SYN   FIN_RCVD {..} = Nothing
transit    _ ST_SYN   CLOSED        = Nothing

data Access
  = NONE
  | READ
  | WRTE
  | RDWR

access :: Status -> Access
access UNINIT        = NONE
access SYN_SENT      = NONE
access SYN_RCVD {..} = NONE
access ESTABLISHED   = RDWR
access FIN_SENT {..} = READ
access FIN_RCVD {..} = WRTE
access CLOSED        = NONE

{-----------------------------------------------------------------------
--  Connection state
-----------------------------------------------------------------------}

data ConnState  = ConnState
  { -- | Current status of connection.
    status       :: !Status

  , nextSeqNr    :: {-# UNPACK #-} !SequenceNr
  , lastAckNr    :: {-# UNPACK #-} !SequenceNr

    -- | Each socket keeps a state for the last delay measurement from
    -- the other endpoint. Whenever a packet is received, this state
    -- is updated by subtracting 'Timestamp' from the hosts /current
    -- time/, in microseconds (see 'Header' format).
    --
    -- Every time a packet is sent, the sockets reply_micro value is
    -- put in the 'diffTime' field of the packet header.
  , replyMicro   :: {-# UNPACK #-} !DiffTime
--  , baseDelay  :: {-# UNPACK #-} !DiffTime

  , curWindow    :: !Window
  } deriving Show

-- | used at init time when there is no acked packets yet
tmpAckNr :: SequenceNr
tmpAckNr = 0

instance Default ConnState where
  def = ConnState
    { status     = UNINIT
    , nextSeqNr  = isn
    , lastAckNr  = tmpAckNr
    , replyMicro = 0
    , curWindow  = def
    }

{-----------------------------------------------------------------------
--  Connection
-----------------------------------------------------------------------}

data Connection = Connection
  { recvId     :: {-# UNPACK #-} !ConnectionId
  , sendId     :: {-# UNPACK #-} !ConnectionId
  , connState  :: {-# UNPACK #-} !(TVar ConnState)
  , established :: TMVar ()
  , closed      :: TMVar ()
  }

data ConnShow = ConnShow
  { _recvId    :: ConnectionId
  , _sendId    :: ConnectionId
  } deriving Show

connShow :: Connection -> ConnShow
connShow Connection {..} = ConnShow recvId sendId

instance Show Connection where
  showsPrec i = showsPrec i . connShow

initiatedConnection :: ConnectionId -> STM Connection
initiatedConnection cid = Connection cid (cid + 1)
  <$> newTVar def
  <*> newEmptyTMVar <*> newEmptyTMVar

-- | Used as first sequence number of an accepting endpoint.
initNr :: Timestamp -> SequenceNr
initNr = fromIntegral

acceptedConnection :: Timestamp -> ConnectionId -> STM Connection
acceptedConnection timestamp cid = Connection cid (cid - 1)
  <$> newTVar def { nextSeqNr = initNr timestamp }
  <*> newEmptyTMVar <*> newEmptyTMVar

isAccepted :: Connection -> Bool
isAccepted Connection {..} = sendId + 1 == recvId

isInitiated :: Connection -> Bool
isInitiated Connection {..} = recvId + 1 == sendId

sameSeq :: Connection -> STM ConnState
sameSeq Connection {..} = readTVar connState

incSeq :: Connection -> STM ConnState
incSeq Connection {..} = do
  st @ ConnState {..} <- readTVar connState
  writeTVar connState st { nextSeqNr = succ nextSeqNr }
  return st

-- type UTP a = ReaderT Connection STM ()

{-----------------------------------------------------------------------
--  Packet handling
-----------------------------------------------------------------------}

fillHeader :: Timestamp -> PacketType -> ConnectionId -> ConnState -> Packet
fillHeader timestamp pty cid ConnState {..} = Packet
  { packetType = pty
  , version    = def
  , extension  = []
  , senderId   = cid
  , sentTime   = timestamp
  , diffTime   = replyMicro
  , wndSize    = W.size curWindow
  , seqNr      = nextSeqNr
  , ackNr      = lastAckNr
  }

grabHeader :: Timestamp -> Packet -> ConnState -> Maybe ConnState
grabHeader timestamp Packet {..} ConnState {..} = do
  s <- transition seqNr packetType status
  return ConnState
    { status     = s
    , nextSeqNr  = nextSeqNr
    , lastAckNr  = error "putHeader"
    , replyMicro = timestamp - sentTime
    , curWindow  = error "putHeader"
    }

fillPacket :: Timestamp -> PacketType -> Connection -> STM Packet
fillPacket timestamp ty c @ Connection {..} = do
  st <- if isIncreases ty then incSeq c else sameSeq c
  let cid = if ty == ST_SYN then recvId else sendId
  return $ fillHeader timestamp ty cid st

scrapePacket :: Timestamp -> Packet -> Connection -> STM ()
scrapePacket timestamp Packet {..} c @ Connection {..} = do
  modifyTVar' connState $ \ ConnState {..} -> ConnState
    { status     = SYN_RCVD -- FIXME
    , nextSeqNr  = nextSeqNr
    , lastAckNr  = seqNr
    , replyMicro = timestamp - sentTime
    , curWindow  = W.delete ackNr curWindow
    }

acceptPacket :: Timestamp -> Connection -> Packet -> STM (Maybe Packet)
acceptPacket = undefined

{-----------------------------------------------------------------------
--  Initialization
-----------------------------------------------------------------------}
-- $connection-establishment
--
-- Normal connection establishment:
--
-- @
-- initiaing endpoint              accepting endpoint
-- |                                                |
-- |SYN_SENT             ST_SYN                     |
-- |>---------------------------------------------->|
-- |                                       SYN_RCVD |
-- |                                                |
-- |                  ST_STATE                      |
-- |<----------------------------------------------<|
-- | ESTABLISHED                                    |
-- |                                                |
-- @
--
-- Simultaneous connection establishment:
--
-- TODO
--

-- |
initSYN :: Timestamp -> Connection -> STM Packet
initSYN timestamp = fillPacket timestamp ST_SYN

acceptSYN :: Timestamp -> Connection -> Packet -> STM Packet
acceptSYN timestamp c p @ Packet {..}
  | packetType /= ST_SYN = error "acceptSYN"
  | seqNr      /= isn    = error "acceptSYN"
  | otherwise = do
    scrapePacket timestamp p c
    fillPacket timestamp ST_STATE c

waitEstablished :: Connection -> STM ()
waitEstablished Connection {..} = do
  takeTMVar established
  putTMVar  established ()

{-----------------------------------------------------------------------
--  Transfer
-----------------------------------------------------------------------}
-- $data-transfer
--
-- Normal connection transfer:
--
-- @
-- A                                                B
-- |                                                |
-- | ESTABLISHED       ST_DATA          ESTABLISHED |
-- |>---------------------------------------------->|
-- | ESTABLISHED                        ESTABLISHED |
-- |                                                |
-- |                                                |
-- | ESTABLISHED       ST_DATA          ESTABLISHED |
-- |<----------------------------------------------<|
-- | ESTABLISHED                        ESTABLISHED |
-- |                                                |
-- @
--

initDATA :: Timestamp -> Connection -> BS.ByteString -> STM Packet
initDATA timestamp c @ Connection {..} bs
  | BS.null bs = error "initData"
  | otherwise = do
    fillHeader timestamp (ST_DATA bs) sendId  <$> incSeq c

acceptACK :: Connection -> Packet -> STM ()
acceptACK = error "acceptACK"

acceptDATA :: Connection -> Packet -> STM ()
acceptDATA = error "acceptDATA"

{-----------------------------------------------------------------------
--  Finalization
-----------------------------------------------------------------------}
-- $connection-termination
--
-- Normal connection finalization:
--
-- @
-- initiaing endpoint              accepting endpoint
-- |                                                |
-- | FIN_SENT             ST_FIN                    |
-- |>---------------------------------------------->|
-- |                                       FIN_RCVD |
-- |                                                |
-- |                  ST_STATE                      |
-- |<----------------------------------------------<|
-- | FIN_SENT                                       |
-- |                                                |
-- @

-- |
initFIN :: Timestamp -> Connection -> STM Packet
initFIN timestamp = fillPacket timestamp ST_FIN

-- |
acceptFIN :: Connection -> Packet -> STM ()
acceptFIN = error "acceptFIN"

waitClosed :: Connection -> STM ()
waitClosed Connection {..} = do
  takeTMVar closed
  putTMVar  closed ()

{-----------------------------------------------------------------------
--  Termination
-----------------------------------------------------------------------}

initRESET :: Connection -> STM ()
initRESET = error "initRESET"

acceptRESET :: Connection -> Packet -> STM ()
acceptRESET = error "acceptRESET"

{-----------------------------------------------------------------------
--  Connection table
-----------------------------------------------------------------------}

-- | Key is the 'recvId' of Connection.
type SlotMap = IntMap Connection

-- FIXME occupy all connection id space
vacantId :: SlotMap -> Maybe ConnectionId
vacantId m = do
  ((k, _), _) <- IM.maxViewWithKey m
  return $ succ (fromIntegral k)

-- HashMap?
type ConnectionMap = Map SockAddr SlotMap

empty :: ConnectionMap
empty = M.empty

lookup :: SockAddr -> ConnectionId -> ConnectionMap -> Maybe Connection
lookup addr cid m = M.lookup addr m >>= IM.lookup (fromIntegral cid)

-- | On accept.
--
--   If there are already connection with specified then nothing is
--   returned.
--
insert :: SockAddr -> Connection -> ConnectionMap -> Maybe ConnectionMap
insert = error "insert"

-- | On close.
--
--   If there is no such connection then nothing is returned.
--
delete :: SockAddr -> Connection -> ConnectionMap -> Maybe ConnectionMap
delete = error "delete"

defConnId :: ConnectionId
defConnId = 0

getConnection :: Timestamp -> SockAddr -> ConnectionId
              -> TVar ConnectionMap -> STM Connection
getConnection timestamp addr cid var  = do
  m <- readTVar var
  case M.lookup addr m of
    Nothing -> do
      conn <- acceptedConnection timestamp cid
      let newSlot = IM.singleton (fromIntegral cid) conn
      writeTVar var $! M.insert addr newSlot m
      return conn

    Just slot -> do
      case IM.lookup (fromIntegral cid) slot of
        Just conn -> return conn
        Nothing   -> do
          conn <- acceptedConnection timestamp cid
          let slot' = IM.insert (fromIntegral cid) conn slot
          writeTVar var $! M.insert addr slot' m
          return conn


-- | On connect.
--
--   If there are no empty connection id nothing is returned.
--
alloc :: SockAddr -> TVar ConnectionMap -> STM Connection
alloc addr var = do
  m <- readTVar var
  case M.lookup addr m of
    Nothing -> do
      conn <- initiatedConnection defConnId
      let newSlot = IM.singleton (fromIntegral defConnId) conn
      writeTVar var $! M.insert addr newSlot m
      return conn

    Just slot -> do
      case vacantId slot of
        Nothing  -> error "allocM" -- FIXME retry?
        Just cid -> do
          conn <- initiatedConnection cid
          let slot' = IM.insert (fromIntegral cid) conn slot
          writeTVar var $! M.insert addr slot' m
          return conn

-- | Send ST_RESET to all connections.
killall :: TVar ConnectionMap -> STM [Packet]
killall = undefined