-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  expermental
--   Portability :  portable
--
--   For more info see:
--   <http://www.bittorrent.org/beps/bep_0029.html#header-format>
--
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.UTP.Packet
       ( -- * Common Types
         ConnectionId
       , Timestamp
       , DiffTime

       , WindowSize
       , defMaxWindow
       , defMinWindow

       , SequenceNr
       , isn

         -- * Packet Types
       , Version
       , PacketType (..)
       , isIncreases

         -- * Extensions
       , BitMask
       , ExtensionId
       , Extension (..)

         -- * Packet
       , Packet (..)
       , size
       ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString as BS
import Data.Default
import Data.List as L
import Data.Serialize as S
import Data.Word


{-----------------------------------------------------------------------
-- Common Types
-----------------------------------------------------------------------}

-- | This is a random, unique, number identifying all the packets that
-- belong to the same connection. Each socket has one connection ID
-- for sending packets and a different connection ID for receiving
-- packets. The endpoint initiating the connection decides which ID to
-- use, and the return path has the same ID + 1.
type ConnectionId = Word16

-- | This value is not meaningful as an absolute value. The clocks in
-- the machines are most likely not synchronized, especially not down
-- to microsecond resolution, and the time the packet is in transit is
-- also included in the difference of these timestamps. However, the
-- value is useful in comparison to previous values.
type Timestamp    = Word32

type DiffTime     = Word32

-- | This is 32 bits wide and specified in bytes. The window size is
-- the number of bytes currently in-flight, i.e. sent but not acked.
type WindowSize   = Word32

defMinWindow :: WindowSize
defMinWindow = 150

defMaxWindow :: WindowSize
defMaxWindow = 1024 * 1024

-- | As opposed to TCP, uTP sequence numbers are not referring to
-- bytes, but packets. The sequence number tells the other end in
-- which order packets should be served back to the application layer.
type SequenceNr    = Word16

-- | Initial sequnce number is always one.
isn :: SequenceNr
isn = 1

{-----------------------------------------------------------------------
-- Header type
-----------------------------------------------------------------------}

data PacketType
   -- | Connect SYN. Similar to TCP SYN flag, this packet initiates a
   -- connection. The sequence number is initialized to 1. The
   -- connection ID is initialized to a random number. The syn packet
   -- is special, all subsequent packets sent on this connection
   -- (except for re-sends of the ST_SYN) are sent with the connection
   -- ID + 1. The connection ID is what the other end is expected to
   -- use in its responses.
   --
   -- When receiving an ST_SYN, the new socket should be initialized
   -- with the ID in the packet header. The send ID for the socket
   -- should be initialized to the ID + 1. The sequence number for the
   -- return channel is initialized to a random number. The other end
   -- expects an ST_STATE packet (only an ACK) in response.
  = ST_SYN

   -- | State packet. Used to transmit an ACK with no data.
   --
   --   Packets that don't include any payload do not increase the
   --   connection sequence number.
   --
  | ST_STATE

   -- | Regular data packet. Socket is in connected state and has data
   -- to send. An ST_DATA packet always has a data payload.
   --
   --   Increases connection sequence number.
  | ST_DATA BS.ByteString

   -- | Finalize the connection. This is the last packet. It closes
   -- the connection, similar to TCP FIN flag. This connection will
   -- never have a sequence number greater than the sequence number in
   -- this packet. The socket records this sequence number as
   -- eof_pkt. This lets the socket wait for packets that might still
   -- be missing and arrive out of order even after receiving the
   -- ST_FIN packet.
   --
   --   Always increases connection sequence number.
   --
  | ST_FIN

   -- | Terminate connection forcefully. Similar to TCP RST flag. The
   -- remote host does not have any state for this connection. It is
   -- stale and should be terminated.
   --
   --   TODO do not increase sequnce number?
   --
  | ST_RESET
    deriving (Show, Eq)

isIncreases :: PacketType -> Bool
isIncreases ST_DATA {..} = True
isIncreases ST_FIN       = False
isIncreases ST_STATE     = False
isIncreases ST_RESET     = False
isIncreases ST_SYN       = True

-- | This is the protocol version.
newtype Version = Version Word8
  deriving (Show, Eq, Serialize)

-- | The current version is 1.
instance Default Version where
  def = Version 1

tyIx :: PacketType -> Word8
tyIx ST_SYN   {..} = 4
tyIx ST_STATE {..} = 2
tyIx ST_DATA  {..} = 0
tyIx ST_FIN   {..} = 1
tyIx ST_RESET {..} = 3

fromIx :: Word8 -> PacketType
fromIx 0 = ST_DATA BS.empty
fromIx 1 = ST_FIN
fromIx 2 = ST_STATE
fromIx 3 = ST_RESET
fromIx 4 = ST_SYN

setPayload :: PacketType -> BS.ByteString -> PacketType
setPayload (ST_DATA _) bs = ST_DATA bs
setPayload  ty         _  = ty

getPayload :: PacketType -> Maybe BS.ByteString
getPayload (ST_DATA bs) = Just bs
getPayload  _           = Nothing

maxTyIx :: Word8
maxTyIx = 4

validateTyVer :: Monad m => (Word8, Word8) -> m (PacketType, Version)
validateTyVer (tw, vw) = do
  t <- if tw <= maxTyIx
       then return (fromIx tw)
       else fail "invalid packet type"

  unless (vw == 1) $ do
    fail $ "invalid version: " ++ show vw

  return (t, Version vw)

splitTyVer :: Word8 -> (Word8, Word8)
splitTyVer w = (w `shiftR` 4, 0x0f .&. w)

getTyVer :: Get (PacketType, Version)
getTyVer = getWord8 >>= validateTyVer . splitTyVer

putTyVer :: Putter (PacketType, Version)
putTyVer (t, Version v) = do
  putWord8 $ fromIntegral (tyIx t `shiftL` 4) .|. (v .&. 0x0f)

{-----------------------------------------------------------------------
-- Extensions
-----------------------------------------------------------------------}

-- TODO use IntervalSet and correct byte order
type BitMask = ByteString

toBitMask :: BS.ByteString -> BitMask
toBitMask = id

fromBitMask :: BitMask -> BS.ByteString
fromBitMask = id

type ExtensionId = Word8

terminationId :: ExtensionId
terminationId = 0

selectiveAckId :: ExtensionId
selectiveAckId = 1

data Extension
  = SelectiveAck BitMask
  | UnknownExt   ExtensionId BS.ByteString
    deriving (Show, Read, Eq)

getSelAck :: Get BitMask
getSelAck = do
  len <- getWord16be
  unless (rem len 4 == 0) $ do
    fail $ "bitmask length is not multiple of 4: " ++ show len
  toBitMask <$> getByteString (fromIntegral len)

getUnkExt :: Get BS.ByteString
getUnkExt = do
  len <- getWord16be
  getByteString (fromIntegral len)

getExtList :: Get [Extension]
getExtList = do
    ext <- getWord8
    case ext of
      0   -> return []
      1   -> (:) <$> (SelectiveAck   <$> getSelAck) <*> getExtList
      eid -> (:) <$> (UnknownExt eid <$> getUnkExt) <*> getExtList

putExt :: Putter Extension
putExt (SelectiveAck mask) = do
  putWord8 selectiveAckId
  let bs = fromBitMask mask
  putWord16be (fromIntegral (BS.length bs))
  putByteString bs
putExt (UnknownExt eid bs) = do
  putWord8 eid
  putWord16be (fromIntegral (BS.length bs))
  putByteString bs

putExts :: Putter [Extension]
putExts []       = putWord8 terminationId
putExts (x : xs) = putExt x >> putExts xs

extIdSize :: Int
extIdSize = 1

lenPrefSize :: Int
lenPrefSize = 2

extSize :: Extension -> Int
extSize (SelectiveAck bm)
   = extIdSize + lenPrefSize + BS.length (fromBitMask bm)
extSize (UnknownExt _ bs) = extIdSize + lenPrefSize + BS.length bs

extsSize :: [Extension] -> Int
extsSize = L.foldr (\ e s -> s + extSize e) extIdSize

{-----------------------------------------------------------------------
-- Packet
-----------------------------------------------------------------------}

-- | Correspond to TCP segment.
data Packet = Packet
  { -- | Type of this packet.
    packetType ::                !PacketType

    -- | Version of protocol.
  , version    :: {-# UNPACK #-} !Version

    -- |
  , extension  ::                ![Extension]

    -- | This is a random, unique, number identifying all the packets
    -- that belong to the same connection. Each socket has one
    -- connection ID for sending packets and a different connection ID
    -- for receiving packets. The endpoint initiating the connection
    -- decides which ID to use, and the return path has the same ID +
    -- 1.
  , senderId   :: {-# UNPACK #-} !ConnectionId

    -- | This is the 'microseconds' parts of the timestamp of when
    -- this packet was sent. This is set using 'gettimeofday()' on
    -- posix and 'QueryPerformanceTimer()' on windows. The higher
    -- resolution this timestamp has, the better. The closer to the
    -- actual transmit time it is set, the better.
  , sentTime   :: {-# UNPACK #-} !Timestamp

    -- | This is the difference between the local time and the
    -- timestamp in the last received packet, at the time the last
    -- packet was received. This is the latest one-way delay
    -- measurement of the link from the remote peer to the local
    -- machine.
  , diffTime   :: {-# UNPACK #-} !Timestamp

    -- | Advertised receive window. The advertised receive window lets
    --   the other end cap the window size if it cannot receive any
    --   faster, if its receive buffer is filling up.
    --
    --   When sending packets, this should be set to the number of
    --   bytes left in the socket's receive buffer.
  , wndSize    :: {-# UNPACK #-} !WindowSize

    -- | This is the sequence number of this packet.
  , seqNr      :: {-# UNPACK #-} !SequenceNr

    -- | This is the sequence number the sender of the packet last
    -- received in the other direction.
  , ackNr      :: {-# UNPACK #-} !SequenceNr
  } deriving (Show, Eq)

instance Serialize Packet where
  get = do
   (t, v) <- getTyVer
   exts   <- getExtList
   connId <- getWord16be
   time   <- getWord32be
   diff   <- getWord32be
   wnd    <- getWord32be
   snr    <- getWord16be
   ack    <- getWord16be
   bs     <- remaining >>= getBytes
   return $ Packet (setPayload t bs) v exts connId time diff wnd snr ack

  put Packet {..} = do
    putTyVer (packetType, version)
    putExts extension
    putWord16be senderId
    putWord32be sentTime
    putWord32be diffTime
    putWord32be wndSize
    putWord16be seqNr
    putWord16be ackNr
    maybe (return ()) putByteString $ getPayload packetType

minHdrSize :: Int
minHdrSize = 20

-- | Packet size in bytes. Limited by UDP datagram size.
size :: Packet -> Int
size Packet {..} = minHdrSize + extsSize extension + payloadSize packetType
  where
    payloadSize (ST_DATA bs) = BS.length bs
    payloadSize  _           = 0
