module Network.UTP.ManagerSpec (spec) where
import Control.Concurrent
import Data.Default
import Test.Hspec

import Network.UTP.Manager


local :: SockAddr
local = SockAddrInet 6800 (127 + 256 * 256 * 256)

remote :: SockAddr
remote = SockAddrInet 62710 (127 + 256 * 256 * 256)

spec :: Spec
spec = do
  describe "Manager" $ do
    it "connect" $ do
      withManager def local $ \ m -> do
        forkIO $ listen m
        sock <- connect m remote
        close sock
