{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.UTP.PacketSpec (spec) where
import Control.Applicative
import Data.Bits
import Data.ByteString as BS
import Data.Default
import Data.Serialize
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Instances ()

import Network.UTP.Packet

instance Arbitrary PacketType where
  arbitrary = frequency
    [ (1, pure ST_SYN)
    , (1, pure ST_STATE)
    , (1, ST_DATA <$> arbitrary)
    , (1, pure ST_FIN)
    , (1, pure ST_RESET)
    ]

instance Arbitrary Version where
  arbitrary = pure def

instance Arbitrary Extension where
  arbitrary = frequency
    [ (1, SelectiveAck <$> (mult4 <$> arbitrary))
    , (1, UnknownExt   <$> choose (2, 255) <*> arbitrary)
    ]
    where
      mult4 bs = BS.take (BS.length bs .&. complement 0x3) bs

instance Arbitrary Packet where
  arbitrary = Packet <$> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary
                     <*> arbitrary

spec :: Spec
spec = do
  describe "header" $ do
    it "properly encoded (iso)" $ property $ \ pkt ->
      decode (encode pkt) `shouldBe` Right (pkt :: Packet)

    it "properly serialized" $ do
      encode (Packet ST_FIN def [] 1 2 3 4 5 6)
         `shouldBe` "\x11\x00\x00\x01\
                    \\x00\x00\x00\x02\
                    \\x00\x00\x00\x03\
                    \\x00\x00\x00\x04\
                    \\x00\x05\x00\x06"

      pendingWith "serialize payload"

    it "properly deserialized" $ do
      pendingWith "fail to decode invalid packet type"
      pendingWith "fail to decode invalid version"
      pendingWith "fail to decode invalid extension"

  describe "packet" $ do
    it "properly encoded" $ property $ \ hdr ->
      decode (encode hdr) `shouldBe` Right (hdr :: Packet)