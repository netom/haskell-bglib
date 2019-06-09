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

-- Write a BgPacket to a SerialPort
writeBGPacket' :: SerialPort -> BgPacket -> IO ()
writeBGPacket' s p = do
    let packetBS = BSL.toStrict $ encode p
    --putStr "* PACKET: "
    --putStrLn $ prettyShowBS packetBS
    --putStrLn ""
    send s packetBS
    return ()

-- Write the BgPacket to the SerialPort in env asked from the MonadReader
writeBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => BgPacket -> m ()
writeBGPacket p = do
    s <- askSerialPort
    liftIO $ writeBGPacket' s p

-- Read one BgPacket from a SerialPort
readBGPacket' :: SerialPort -> IO BgPacket
readBGPacket' s = do
    bgpHeader@BgPacketHeader{..} <- decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- recv s (fromIntegral bghLength)
    --putStr "* HEADER: "
    --putStrLn $ show bgpHeader
    --putStrLn "* PAYLOAD: "
    --putStrLn $ prettyShowBS bgpPayload
    --putStrLn ""
    return $ BgPacket {..}

-- Read one BgPacket from the SerialPort in env asked from the MonadReader
readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
readBGPacket = do
    s <- askSerialPort
    liftIO $ readBGPacket' s

-- Launch a thread that reads packets and sends them down a TChan BgPacket
startPacketReader :: (MonadIO m, MonadReader env m, HasBGChan env, HasSerialPort env) => m () 
startPacketReader = do
    c <- askBGChan
    s <- askSerialPort
    _ <- liftIO $ forkIO $ forever $ do
        --putStrLn "* READ *"
        readBGPacket' s >>= atomically . writeTChan c
    return ()

-- Waits for any BgPacket to appear on the TChan
waitForAnyPacket :: TChan BgPacket -> IO BgPacket
waitForAnyPacket chan = do
    liftIO $ atomically $ readTChan chan

-- Wait for a packet with specific values in it's header
waitForPacket :: TChan BgPacket -> BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> IO BgPacket
waitForPacket chan mt tt cc cid = do
    untilJust $ do
        p@BgPacket{..} <- waitForAnyPacket chan
        return $ if bgHeaderMatches mt tt cc cid bgpHeader then Just p else Nothing

-- eXecute a Command, don't wait for any answer
xCmd' :: (MonadIO m, MonadReader env m, HasSerialPort env, Binary a ) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m ()
xCmd' mt tt cc cid inp = do
    let inpBS = BSL.toStrict $ encode inp
    writeBGPacket $ BgPacket (BgPacketHeader mt tt (fromIntegral $ BSS.length inpBS) cc cid) inpBS

-- Execute a command, wait for the appropriate answer
xCmd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, Binary a, Binary b) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m b
xCmd mt tt cc cid inp = do
    -- We need to duplicate the channel BEFORE sending the command, so we don't miss the answer by accident
    chan <- askDupBGChan
    xCmd' mt tt cc cid inp
    decode . BSL.fromStrict . bgpPayload <$> ( liftIO $ waitForPacket chan mt tt cc cid )

registerEventHandler :: Binary a => (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> (a -> IO Bool) -> m ThreadId
registerEventHandler mt tt cc cid handler = do
    chan <- askDupBGChan
    liftIO $ forkIO $ go chan
    where
        go chan = do
            BgPacket{..} <- waitForPacket chan mt tt cc cid
            continue <- handler $ decode $ BSL.fromStrict $ bgpPayload
            if continue then go chan else return ()

-----------------------------------------------------------------------
-- Attribute Client
-----------------------------------------------------------------------

attclientAttributeWrite = undefined
attclientExecuteWrite = undefined
attclientFindByTypeValue = undefined
attclientFindInformation = undefined
attclientIndicateConfirm = undefined
attclientPrepareWrite = undefined
attclientReadByGroupType = undefined
attclientReadByHandle = undefined
attclientReadByType = undefined
attclientReadLong = undefined
attclientReadMultiple = undefined
attclientWriteCommand = undefined

evtAttclientAttributeValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((UInt8, UInt16, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtAttclientAttributeValue handler = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x05 handler

evtAttclientFindInformationFound = undefined
evtAttclientGroupFound = undefined
evtAttclientIndicated = undefined
evtAttclientProcedureCompleted = undefined
evtAttclientReadMultipleResponse = undefined

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributesRead = undefined
attributesReadType = undefined
attributesSend = undefined
attributesUserReadResponse = undefined
attributesUserWriteResponse = undefined
attributesWrite = undefined

evtAttributesStatus = undefined
evtAttributesUserReadRequest = undefined
evtAttributesValue = undefined

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connectionChannelMapGet = undefined
connectionChannelMapSet = undefined
connectionDisconnect = undefined
connectionGetRssi = undefined
connectionGetStatus = undefined
connectionSlaveLatencyDisable = undefined
connectionUpdate = undefined
connectionVersionUpdate = undefined

evtConnectionDisconnected = undefined
evtConnectionFeatureInd = undefined
evtConnectionStatus = undefined
evtConnectionVersionInd = undefined

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gapConnectDirect = undefined
gapConnectSelective = undefined

-- This command starts the GAP discovery procedure to scan for advertising devices i.e. to perform a device
-- discovery.
-- Scanning parameters can be configured with the Set Scan Parameters command before issuing this command.
-- To cancel on an ongoing discovery process use the End Procedure command.
gapDiscover :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => GapDiscoveryMode -> m UInt16
gapDiscover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode

-- This command ends the current GAP discovery procedure and stop the scanning of advertising devices.
gapEndProcedure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m UInt16
gapEndProcedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

gapSetAdvData = undefined
gapSetAdvParameters = undefined
gapSetDirectedConnectableMode = undefined
gapSetFiltering = undefined
gapSetInitiatingConParameters = undefined
gapSetMode = undefined
gapSetNonresolvableAddress = undefined
gapSetPrivacyFlags = undefined
gapSetScanParameters = undefined

-- Register an event handler for GAP scan responses
evtGapScanResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((Int8, UInt8, BdAddr, UInt8, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtGapScanResponse handler = registerEventHandler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 handler

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardwareAdcRead = undefined
hardwareAnalogComparatorConfigIrq = undefined
hardwareAnalogComparatorEnable = undefined
hardwareAnalogComparatorRead = undefined
hardwareGetTimestamp = undefined
hardwareI2cRead = undefined
hardwareI2cWrite = undefined
hardwareIoPortConfigDirection = undefined
hardwareIoPortConfigFunction = undefined
hardwareIoPortConfigPull = undefined
hardwareIoPortIrqDirection = undefined
hardwareIoPortIrqEnable = undefined
hardwareIoPortRead = undefined
hardwareIoPortWrite = undefined
hardwareSetRxgain = undefined
hardwareSetSoftTimer = undefined
hardwareSetTxpower = undefined
hardwareSleepEnable = undefined
hardwareSpiConfig = undefined
hardwareSpiTransfer = undefined
hardwareTimerComparator = undefined
hardwareUsbEnable = undefined

evtHardwareAdcResult = undefined
evtHardwareAnalogComparatorStatus = undefined
evtHardwareIoPortStatus = undefined
evtHardwareSoftTimer = undefined

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flashErasePage = undefined
flashPsDefrag = undefined
flashPsDump = undefined
flashPsEraseAll = undefined
flashPsErase = undefined
flashPsLoad = undefined
flashPsSave = undefined
flashReadData = undefined
flashWriteData = undefined

evtFlashPsKey = undefined

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding = undefined
smEncryptStart = undefined
smGetBonds = undefined
smPasskeyEntry = undefined
setBondableMode = undefined
smSetOobData = undefined
smSetPairingDistributionKeys = undefined
smSetParameters = undefined
smWhitelistBonds = undefined

evtSmBondingFail = undefined
evtSmBondStatus = undefined
evtSmPasskeyDisplay = undefined
evtSmPasskeyRequest = undefined


-----------------------------------------------------------------------
-- System
-----------------------------------------------------------------------

-- This command reads the local devices public Bluetooth address.
systemAddressGet :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m BdAddr
systemAddressGet = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()

-- This command decrypts the given data using the AES algorithm with the predefined key set with command Aes
-- Setkey .
systemAesDecrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
systemAesDecrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x11 dta

-- This command encrypts the given data using the AES algorithm with the predefined with command Aes Setkey .
systemAesEncrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
systemAesEncrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x10 dta

-- This command defines the encryption key that will be used with the AES encrypt and decrypt commands.
systemAesSetkey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m ()
systemAesSetkey key = xCmd BgMsgCR BgBlue BgClsSystem 0x0f key

systemDelayReset = undefined
systemEndpointRx = undefined
systemEndpointSetWatermarks = undefined
systemEndpointTx = undefined
systemGetBootloaderCrc = undefined
systemGetConnections = undefined
systemGetCounters = undefined
systemGetInfo = undefined


-- This command can be used to test if the local device is functional. Similar to a typical "AT" -> "OK" test.
systemHello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

-- This command resets the local device immediately. The command does not have a response.
systemReset :: (MonadIO m, MonadReader env m, HasSerialPort env) => RebootMode -> m ()
systemReset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

systemUsbEnumerationStatusGet = undefined
systemWhitelistAppend = undefined
systemWhitelistClear = undefined
systemWhitelistRemove = undefined

evtSystemBoot = undefined
evtSystemEndpointWatermarkRx = undefined
evtSystemEndpointWatermarkTx = undefined
evtSystemNoLicenseKey = undefined

-- Event handler for protocol errors
evtSystemProtocolError :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => (UInt16 -> IO Bool) -> m ThreadId
evtSystemProtocolError handler = registerEventHandler BgMsgEvent BgBlue BgClsSystem 0x06 handler

evtSystemScriptFailure = undefined
evtSystemUsbEnumerated = undefined

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode = undefined
testGetChannelMap = undefined
testPhyEnd = undefined
testPhyRx = undefined
testPhyTx = undefined

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress = undefined
dfuFlashUpload = undefined
dfuFlashUploadFinish = undefined
dfuReset = undefined

evtDfuBoot = undefined
