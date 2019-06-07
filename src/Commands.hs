{-# LANGUAGE RecordWildCards #-}

module Commands where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import           Control.Monad.STM
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
import           System.IO
import           Types
import           Control.Monad.IO.Class
import           Control.Monad.Reader

writeBGPacket' :: SerialPort -> BgPacket -> IO ()
writeBGPacket' s p = do
    let packetBS = BSL.toStrict $ encode p
    --putStr "* PACKET: "
    --putStrLn $ prettyShowBS packetBS
    --putStrLn ""
    send s packetBS
    return ()

writeBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => BgPacket -> m ()
writeBGPacket p = do
    s <- askSerialPort
    liftIO $ writeBGPacket' s p

readBGPacket' :: SerialPort -> IO BgPacket
readBGPacket' s = do
    bgpHeader@BgPacketHeader{..} <- decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- recv s (fromIntegral bghLength)
    putStr "* HEADER: "
    putStrLn $ show bgpHeader
    putStrLn "* PAYLOAD: "
    putStrLn $ prettyShowBS bgpPayload
    putStrLn ""
    return $ BgPacket {..}

readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
readBGPacket = do
    s <- askSerialPort
    liftIO $ readBGPacket' s

startPacketReader :: (MonadIO m, MonadReader env m, HasBGChan env, HasSerialPort env) => m () 
startPacketReader = do
    c <- askBGChan
    s <- askSerialPort
    _ <- liftIO $ forkIO $ forever $ do
        --putStrLn "* READ *"
        readBGPacket' s >>= atomically . writeTChan c
    return ()

waitForAnyPacket :: TChan BgPacket -> IO BgPacket
waitForAnyPacket chan = do
    liftIO $ atomically $ readTChan chan

waitForPacket :: TChan BgPacket -> BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> IO BgPacket
waitForPacket chan mt tt cc cid = do
    untilJust $ do
        p@BgPacket{..} <- waitForAnyPacket chan
        return $ if bgHeaderMatches mt tt cc cid bgpHeader then Just p else Nothing

xCmd' :: (MonadIO m, MonadReader env m, HasSerialPort env, Binary a ) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m ()
xCmd' mt tt cc cid inp = do
    let inpBS = BSL.toStrict $ encode inp
    writeBGPacket $ BgPacket (BgPacketHeader mt tt (fromIntegral $ BSS.length inpBS) cc cid) inpBS

xCmd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, Binary a, Binary b) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m b
xCmd mt tt cc cid inp = do
    -- We need to duplicate the channel BEFORE sending the command, so we don't miss the answer by accident
    chan <- askDupBGChan
    xCmd' mt tt cc cid inp
    decode . BSL.fromStrict . bgpPayload <$> ( liftIO $ waitForPacket chan mt tt cc cid )

-- Generic Access Profile

-- This command starts the GAP discovery procedure to scan for advertising devices i.e. to perform a device
-- discovery.
-- Scanning parameters can be configured with the Set Scan Parameters command before issuing this command.
-- To cancel on an ongoing discovery process use the End Procedure command.
cmd_gap_discover :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => GapDiscoveryMode -> m UInt16
cmd_gap_discover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode

-- This command ends the current GAP discovery procedure and stop the scanning of advertising devices.
cmd_gap_end_procedure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m UInt16
cmd_gap_end_procedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

-- System

cmd_system_address_get :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m BdAddr
cmd_system_address_get = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()

cmd_system_aes_decrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
cmd_system_aes_decrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x11 dta

cmd_system_aes_encrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
cmd_system_aes_encrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x10 dta

cmd_system_aes_setkey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m ()
cmd_system_aes_setkey key = xCmd BgMsgCR BgBlue BgClsSystem 0x0f key

cmd_system_hello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m ()
cmd_system_hello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

cmd_system_reset :: (MonadIO m, MonadReader env m, HasSerialPort env) => m ()
cmd_system_reset = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 ()