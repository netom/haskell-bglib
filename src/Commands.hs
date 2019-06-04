{-# LANGUAGE RecordWildCards #-}

module Commands where

import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
import           System.IO
import           Types
import           Messages

writeBGPacket :: SerialPort -> BgPacket -> IO ()
writeBGPacket s p = do
    send s $ BSL.toStrict $ encode p
    return ()

readBGPacket :: SerialPort -> IO BgPacket
readBGPacket s = do
    bgpHeader@BgPacketHeader{..} <- decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- recv s (fromIntegral bghLength)
    return $ BgPacket {..}

cmd_system_address_get :: SerialPort -> IO BdAddr
cmd_system_address_get s = do
    writeBGPacket s $ BgPacket (BgPacketHeader BgMsgCR BgBlue 0 BgClsSystem 0x02) BSS.empty
    h@BgPacketHeader{..} <- decode . BSL.fromStrict <$> recv s 4
    payload <- recv s (fromIntegral bghLength)
    return $ decode $ BSL.fromStrict payload
