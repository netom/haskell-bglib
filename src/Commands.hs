{-# LANGUAGE RecordWildCards #-}

module Commands where

import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
import           System.IO
import           Types
import           Control.Monad.IO.Class
import           Control.Monad.Reader

askSerialPort :: (MonadReader env m, HasSerialPort env) => m SerialPort
askSerialPort = do
    s <- getSerialPort <$> ask
    return s

writeBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => BgPacket -> m ()
writeBGPacket p = do
    s <- askSerialPort
    liftIO $ send s $ BSL.toStrict $ encode p
    return ()

readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
readBGPacket = do
    s <- askSerialPort
    bgpHeader@BgPacketHeader{..} <- liftIO $ decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- liftIO $ recv s (fromIntegral bghLength)
    return $ BgPacket {..}

xCmd :: (MonadIO m, MonadReader env m, HasSerialPort env, Binary a, Binary b) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m b
xCmd mt tt cc cid inp = do
    let inpBS = BSL.toStrict $ encode inp
    writeBGPacket $ BgPacket (BgPacketHeader mt tt (fromIntegral $ BSS.length inpBS) cc cid) inpBS
    decode . BSL.fromStrict . bgpPayload <$> readBGPacket

cmd_system_address_get :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BdAddr
cmd_system_address_get = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()
