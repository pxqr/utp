
module Network.UTP
       ( Socket
       , N.SockAddr (..)

         -- * Socket operations
       , socket
       , connect
       , bind
       , listen
       , listenOn
       , accept
       , close

         -- * Sending and receiving data
       , send
       , recv
       ) where

import Network.UTP.Socket
import qualified Network.Socket as N