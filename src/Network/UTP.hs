
module Network.UTP
       ( Socket

         -- * Socket operations
       , socket
       , connect
       , bind
       , listen
       , accept
       , close

         -- * Sending and receiving data
       , send
       , recv
       ) where

import Network.UTP.Socket