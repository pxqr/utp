-- |
--   Copyright   :  (c) Sam Truzjan 2014
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  expermental
--   Portability :  portable
--
{-# LANGUAGE RecordWildCards #-}
module Network.UTP.Window
       ( Window
       , Network.UTP.Window.empty
       , Network.UTP.Window.size
       , Network.UTP.Window.insert
       , Network.UTP.Window.delete
       ) where

import Data.Default
import Data.IntMap as IM
import Network.UTP.Packet as Pkt


type PacketSize = Int

minPacketSize :: PacketSize
minPacketSize = 150

defMaxSize :: WindowSize
defMaxSize = 1000

data Window = Window
  { inflight :: IntMap Packet

    -- | The number of bytes in-flight.
  , curSize  :: {-# UNPACK #-} !WindowSize

    -- | Determines the maximum number of bytes the socket may have
    -- in-flight at any given time. Any packet that has been sent, but
    -- not yet acked, is considered to be in-flight.
  , maxSize  :: {-# UNPACK #-} !WindowSize
  } deriving Show

instance Default Window where
  def = Network.UTP.Window.empty defMaxSize

empty :: WindowSize -> Window
empty n = Window
  { inflight = IM.empty
  , curSize  = 0
  , maxSize  = n
  }

size :: Window -> WindowSize
size = curSize

-- TODO An implementation MAY violate the above rule if the max_window is
--smaller than the packet size, and it paces the packets so that the
--average cur_window is less than or equal to max_window.

-- | Used before send.
insert :: SequenceNr -> Packet -> Window -> Maybe Window
insert nr pkt @ Packet {..} Window {..}
  | curSize + fromIntegral packetSize > maxSize = Nothing
  | otherwise = Just Window
    { inflight = inflight -- todo
    , curSize  = curSize + packetSize -- do not add if replace
    , maxSize  = maxSize
    }
  where
    packetSize = fromIntegral (Pkt.size pkt)

-- TODO packet loss
-- TODO compute rtt
-- | Used to process ack.
delete :: SequenceNr -> Window -> Window
delete nr Window {..} = undefined