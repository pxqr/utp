module Network.UTP.Packet  where

import Control.Applicative
import Data.Bits
import Data.Serialize
import Data.Word

data PacketType
  = ST_DATA  -- ^ regular data packet which always has a payload;
  | ST_FIN   -- ^ last packet which finalize connection;
  | ST_STATE -- ^
  | ST_RESET -- ^ terminate connection _forcefully_;
  | ST_SYN   -- ^ synchonize sequence numbers.
    deriving (Show, Eq, Ord, Enum)

type Version = Word8

defaultVersion :: Version
defaultVersion = 1

type ConnectionId = Word16
type Capabilities = Word8
type Timestamp = Word32
type PacketId  = Word16

data Header = Header
    { ptype        ::                !PacketType
    , version      :: {-# UNPACK #-} !Version
    , extension    :: {-# UNPACK #-} !Capabilities
    , connectionId :: {-# UNPACK #-} !ConnectionId
    , timestampU   :: {-# UNPACK #-} !Timestamp
    , timestampDU  :: {-# UNPACK #-} !Timestamp
    , windSize     :: {-# UNPACK #-} !Word32
    , seqNum       :: {-# UNPACK #-} !PacketId
    , ackNum       :: {-# UNPACK #-} !PacketId
    }

instance Serialize Header where
  get = uncurry Header
      <$> (decodeTy <$> getWord8)
      <*> getWord8
      <*> getWord16be
      <*> getWord32be
      <*> getWord32be
      <*> getWord32be
      <*> getWord16be
      <*> getWord16be
    where
      decodeTy bits = ( toEnum $ fromIntegral $ bits `shiftR` 4
                      , bits .&. 0x0F)

  put Header {..} = do
      putWord8  $ encodeTy ptype version
      putWord8    extension
      putWord16be connectionId
      putWord32be timestampU
      putWord32be timestampDU
      putWord32be windSize
      putWord16be seqNum
      putWord16be ackNum
    where
      encodeTy ty ver = (fromIntegral (fromEnum ty) `shiftL` 4)
                    .|. (ver .&. 0x0F)

headerSize :: Int
headerSize = 160

syn :: a -> Header
syn = undefined