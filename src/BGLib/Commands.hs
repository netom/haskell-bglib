{-# LANGUAGE RecordWildCards #-}

module BGLib.Commands
    ( startPacketReader
    , attclientAttributeWrite
    , attclientExecuteWrite
    , attclientFindByTypeValue
    , attclientFindInformation
    , attclientIndicateConfirm
    , attclientPrepareWrite
    , attclientReadByGroupType
    , attclientReadByHandle
    , attclientReadByType
    , attclientReadLong
    , attclientReadMultiple
    , attclientWriteCommand
    , evtAttclientAttributeValue
    , evtAttclientFindInformationFound
    , evtAttclientGroupFound
    , evtAttclientIndicated
    , evtAttclientProcedureCompleted
    , evtAttclientReadMultipleResponse
    , attributesRead
    , attributesReadType
    , attributesSend
    , attributesUserReadResponse
    , attributesUserWriteResponse
    , attributesWrite
    , evtAttributesStatus
    , evtAttributesUserReadRequest
    , evtAttributesValue
    , connectionChannelMapGet
    , connectionChannelMapSet
    , connectionDisconnect
    , connectionGetRssi
    , connectionGetStatus
    , connectionSlaveLatencyDisable
    , connectionUpdate
    , connectionVersionUpdate
    , evtConnectionDisconnected
    , evtConnectionFeatureInd
    , evtConnectionStatus
    , evtConnectionVersionInd
    , gapConnectDirect
    , gapConnectSelective
    , gapDiscover
    , gapEndProcedure
    , gapSetAdvData
    , gapSetAdvParameters
    , gapSetDirectedConnectableMode
    , gapSetFiltering
    , gapSetInitiatingConParameters
    , gapSetMode
    , gapSetNonresolvableAddress
    , gapSetPrivacyFlags
    , gapSetScanParameters
    , evtGapScanResponse
    , hardwareAdcRead
    , hardwareAnalogComparatorConfigIrq
    , hardwareAnalogComparatorEnable
    , hardwareAnalogComparatorRead
    , hardwareGetTimestamp
    , hardwareI2cRead
    , hardwareI2cWrite
    , hardwareIoPortConfigDirection
    , hardwareIoPortConfigFunction
    , hardwareIoPortConfigPull
    , hardwareIoPortIrqDirection
    , hardwareIoPortIrqEnable
    , hardwareIoPortRead
    , hardwareIoPortWrite
    , hardwareSetRxgain
    , hardwareSetSoftTimer
    , hardwareSetTxpower
    , hardwareSleepEnable
    , hardwareSpiConfig
    , hardwareSpiTransfer
    , hardwareTimerComparator
    , hardwareUsbEnable
    , evtHardwareAdcResult
    , evtHardwareAnalogComparatorStatus
    , evtHardwareIoPortStatus
    , evtHardwareSoftTimer
    , flashErasePage
    , flashPsDefrag
    , flashPsDump
    , flashPsEraseAll
    , flashPsErase
    , flashPsLoad
    , flashPsSave
    , flashReadData
    , flashWriteData
    , evtFlashPsKey
    , smDeleteBonding
    , smEncryptStart
    , smGetBonds
    , smPasskeyEntry
    , setBondableMode
    , smSetOobData
    , smSetPairingDistributionKeys
    , smSetParameters
    , smWhitelistBonds
    , evtSmBondingFail
    , evtSmBondStatus
    , evtSmPasskeyDisplay
    , evtSmPasskeyRequest
    , systemAddressGet
    , systemAesDecrypt
    , systemAesEncrypt
    , systemAesSetkey
    , systemDelayReset
    , systemEndpointRx
    , systemEndpointSetWatermarks
    , systemEndpointTx
    , systemGetBootloaderCrc
    , systemGetConnections
    , systemGetCounters
    , systemGetInfo
    , systemHello
    , systemReset
    , systemUsbEnumerationStatusGet
    , systemWhitelistAppend
    , systemWhitelistClear
    , systemWhitelistRemove
    , evtSystemBoot
    , evtSystemEndpointWatermarkRx
    , evtSystemEndpointWatermarkTx
    , evtSystemNoLicenseKey
    , evtSystemProtocolError
    , evtSystemScriptFailure
    , evtSystemUsbEnumerated
    , testChannelMode
    , testGetChannelMap
    , testPhyEnd
    , testPhyRx
    , testPhyTx
    , dfuFlashSetAddress
    , dfuFlashUpload
    , dfuFlashUploadFinish
    , dfuReset
    , evtDfuBoot    
    ) where

import           BGLib.Types
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import           Control.Monad.STM
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
import           Control.Monad.IO.Class
import           Control.Monad.Reader

-- Write a BgPacket to a SerialPort
writeBGPacket' :: SerialPort -> BgPacket -> IO ()
writeBGPacket' s p = do
    let packetBS = BSL.toStrict $ encode p
    --putStr "* PACKET: "
    --putStrLn $ prettyShowBS packetBS
    --putStrLn ""
    _ <- send s packetBS
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
--readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
--readBGPacket = do
--    s <- askSerialPort
--    liftIO $ readBGPacket' s

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
attclientAttributeWrite :: a
attclientAttributeWrite = error "Not implemented yet."

attclientExecuteWrite :: a
attclientExecuteWrite = error "Not implemented yet."

attclientFindByTypeValue :: a
attclientFindByTypeValue = error "Not implemented yet."

attclientFindInformation :: a
attclientFindInformation = error "Not implemented yet."

attclientIndicateConfirm :: a
attclientIndicateConfirm = error "Not implemented yet."

attclientPrepareWrite :: a
attclientPrepareWrite = error "Not implemented yet."

attclientReadByGroupType :: a
attclientReadByGroupType = error "Not implemented yet."

attclientReadByHandle :: a
attclientReadByHandle = error "Not implemented yet."

attclientReadByType :: a
attclientReadByType = error "Not implemented yet."

attclientReadLong :: a
attclientReadLong = error "Not implemented yet."

attclientReadMultiple :: a
attclientReadMultiple = error "Not implemented yet."

attclientWriteCommand :: a
attclientWriteCommand = error "Not implemented yet."

evtAttclientAttributeValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((UInt8, UInt16, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtAttclientAttributeValue handler = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x05 handler

evtAttclientFindInformationFound :: a
evtAttclientFindInformationFound = error "Not implemented yet."

evtAttclientGroupFound :: a
evtAttclientGroupFound = error "Not implemented yet."

evtAttclientIndicated :: a
evtAttclientIndicated = error "Not implemented yet."

evtAttclientProcedureCompleted :: a
evtAttclientProcedureCompleted = error "Not implemented yet."

evtAttclientReadMultipleResponse :: a
evtAttclientReadMultipleResponse = error "Not implemented yet."

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributesRead :: a
attributesRead = error "Not implemented yet."

attributesReadType :: a
attributesReadType = error "Not implemented yet."

attributesSend :: a
attributesSend = error "Not implemented yet."

attributesUserReadResponse :: a
attributesUserReadResponse = error "Not implemented yet."

attributesUserWriteResponse :: a
attributesUserWriteResponse = error "Not implemented yet."

attributesWrite :: a
attributesWrite = error "Not implemented yet."

evtAttributesStatus :: a
evtAttributesStatus = error "Not implemented yet."

evtAttributesUserReadRequest :: a
evtAttributesUserReadRequest = error "Not implemented yet."

evtAttributesValue :: a
evtAttributesValue = error "Not implemented yet."

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connectionChannelMapGet :: a
connectionChannelMapGet = error "Not implemented yet."

connectionChannelMapSet :: a
connectionChannelMapSet = error "Not implemented yet."

connectionDisconnect :: a
connectionDisconnect = error "Not implemented yet."

connectionGetRssi :: a
connectionGetRssi = error "Not implemented yet."

connectionGetStatus :: a
connectionGetStatus = error "Not implemented yet."

connectionSlaveLatencyDisable :: a
connectionSlaveLatencyDisable = error "Not implemented yet."

connectionUpdate :: a
connectionUpdate = error "Not implemented yet."

connectionVersionUpdate :: a
connectionVersionUpdate = error "Not implemented yet."

evtConnectionDisconnected :: a
evtConnectionDisconnected = error "Not implemented yet."

evtConnectionFeatureInd :: a
evtConnectionFeatureInd = error "Not implemented yet."

evtConnectionStatus :: a
evtConnectionStatus = error "Not implemented yet."

evtConnectionVersionInd :: a
evtConnectionVersionInd = error "Not implemented yet."

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gapConnectDirect :: a
gapConnectDirect = error "Not implemented yet."

gapConnectSelective :: a
gapConnectSelective = error "Not implemented yet."

-- This command starts the GAP discovery procedure to scan for advertising devices i.e. to perform a device
-- discovery.
-- Scanning parameters can be configured with the Set Scan Parameters command before issuing this command.
-- To cancel on an ongoing discovery process use the End Procedure command.
gapDiscover :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => GapDiscoverMode -> m UInt16
gapDiscover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode

-- This command ends the current GAP discovery procedure and stop the scanning of advertising devices.
gapEndProcedure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m UInt16
gapEndProcedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

gapSetAdvData :: a
gapSetAdvData = error "Not implemented yet."

gapSetAdvParameters :: a
gapSetAdvParameters = error "Not implemented yet."

gapSetDirectedConnectableMode :: a
gapSetDirectedConnectableMode = error "Not implemented yet."

gapSetFiltering :: a
gapSetFiltering = error "Not implemented yet."

gapSetInitiatingConParameters :: a
gapSetInitiatingConParameters = error "Not implemented yet."

gapSetMode :: a
gapSetMode = error "Not implemented yet."

gapSetNonresolvableAddress :: a
gapSetNonresolvableAddress = error "Not implemented yet."

gapSetPrivacyFlags :: a
gapSetPrivacyFlags = error "Not implemented yet."

gapSetScanParameters :: a
gapSetScanParameters = error "Not implemented yet."

-- Register an event handler for GAP scan responses
evtGapScanResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((Int8, UInt8, BdAddr, UInt8, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtGapScanResponse handler = registerEventHandler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 handler

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardwareAdcRead :: a
hardwareAdcRead = error "Not implemented yet."

hardwareAnalogComparatorConfigIrq :: a
hardwareAnalogComparatorConfigIrq = error "Not implemented yet."

hardwareAnalogComparatorEnable :: a
hardwareAnalogComparatorEnable = error "Not implemented yet."

hardwareAnalogComparatorRead :: a
hardwareAnalogComparatorRead = error "Not implemented yet."

hardwareGetTimestamp :: a
hardwareGetTimestamp = error "Not implemented yet."

hardwareI2cRead :: a
hardwareI2cRead = error "Not implemented yet."

hardwareI2cWrite :: a
hardwareI2cWrite = error "Not implemented yet."

hardwareIoPortConfigDirection :: a
hardwareIoPortConfigDirection = error "Not implemented yet."

hardwareIoPortConfigFunction :: a
hardwareIoPortConfigFunction = error "Not implemented yet."

hardwareIoPortConfigPull :: a
hardwareIoPortConfigPull = error "Not implemented yet."

hardwareIoPortIrqDirection :: a
hardwareIoPortIrqDirection = error "Not implemented yet."

hardwareIoPortIrqEnable :: a
hardwareIoPortIrqEnable = error "Not implemented yet."

hardwareIoPortRead :: a
hardwareIoPortRead = error "Not implemented yet."

hardwareIoPortWrite :: a
hardwareIoPortWrite = error "Not implemented yet."

hardwareSetRxgain :: a
hardwareSetRxgain = error "Not implemented yet."

hardwareSetSoftTimer :: a
hardwareSetSoftTimer = error "Not implemented yet."

hardwareSetTxpower :: a
hardwareSetTxpower = error "Not implemented yet."

hardwareSleepEnable :: a
hardwareSleepEnable = error "Not implemented yet."

hardwareSpiConfig :: a
hardwareSpiConfig = error "Not implemented yet."

hardwareSpiTransfer :: a
hardwareSpiTransfer = error "Not implemented yet."

hardwareTimerComparator :: a
hardwareTimerComparator = error "Not implemented yet."

hardwareUsbEnable :: a
hardwareUsbEnable = error "Not implemented yet."

evtHardwareAdcResult :: a
evtHardwareAdcResult = error "Not implemented yet."

evtHardwareAnalogComparatorStatus :: a
evtHardwareAnalogComparatorStatus = error "Not implemented yet."

evtHardwareIoPortStatus :: a
evtHardwareIoPortStatus = error "Not implemented yet."

evtHardwareSoftTimer :: a
evtHardwareSoftTimer = error "Not implemented yet."

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flashErasePage :: a
flashErasePage = error "Not implemented yet."

flashPsDefrag :: a
flashPsDefrag = error "Not implemented yet."

flashPsDump :: a
flashPsDump = error "Not implemented yet."

flashPsEraseAll :: a
flashPsEraseAll = error "Not implemented yet."

flashPsErase :: a
flashPsErase = error "Not implemented yet."

flashPsLoad :: a
flashPsLoad = error "Not implemented yet."

flashPsSave :: a
flashPsSave = error "Not implemented yet."

flashReadData :: a
flashReadData = error "Not implemented yet."

flashWriteData :: a
flashWriteData = error "Not implemented yet."

evtFlashPsKey :: a
evtFlashPsKey = error "Not implemented yet."

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding :: a
smDeleteBonding = error "Not implemented yet."

smEncryptStart :: a
smEncryptStart = error "Not implemented yet."

smGetBonds :: a
smGetBonds = error "Not implemented yet."

smPasskeyEntry :: a
smPasskeyEntry = error "Not implemented yet."

setBondableMode :: a
setBondableMode = error "Not implemented yet."

smSetOobData :: a
smSetOobData = error "Not implemented yet."

smSetPairingDistributionKeys :: a
smSetPairingDistributionKeys = error "Not implemented yet."

smSetParameters :: a
smSetParameters = error "Not implemented yet."

smWhitelistBonds :: a
smWhitelistBonds = error "Not implemented yet."

evtSmBondingFail :: a
evtSmBondingFail = error "Not implemented yet."

evtSmBondStatus :: a
evtSmBondStatus = error "Not implemented yet."

evtSmPasskeyDisplay :: a
evtSmPasskeyDisplay = error "Not implemented yet."

evtSmPasskeyRequest :: a
evtSmPasskeyRequest = error "Not implemented yet."

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

systemDelayReset :: a
systemDelayReset = error "Not implemented yet."

systemEndpointRx :: a
systemEndpointRx = error "Not implemented yet."

systemEndpointSetWatermarks :: a
systemEndpointSetWatermarks = error "Not implemented yet."

systemEndpointTx :: a
systemEndpointTx = error "Not implemented yet."

systemGetBootloaderCrc :: a
systemGetBootloaderCrc = error "Not implemented yet."

systemGetConnections :: a
systemGetConnections = error "Not implemented yet."

systemGetCounters :: a
systemGetCounters = error "Not implemented yet."

systemGetInfo :: a
systemGetInfo = error "Not implemented yet."


-- This command can be used to test if the local device is functional. Similar to a typical "AT" -> "OK" test.
systemHello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

-- This command resets the local device immediately. The command does not have a response.
systemReset :: (MonadIO m, MonadReader env m, HasSerialPort env) => RebootMode -> m ()
systemReset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

systemUsbEnumerationStatusGet :: a
systemUsbEnumerationStatusGet = error "Not implemented yet."

systemWhitelistAppend :: a
systemWhitelistAppend = error "Not implemented yet."

systemWhitelistClear :: a
systemWhitelistClear = error "Not implemented yet."

systemWhitelistRemove :: a
systemWhitelistRemove = error "Not implemented yet."

evtSystemBoot :: a
evtSystemBoot = error "Not implemented yet."

evtSystemEndpointWatermarkRx :: a
evtSystemEndpointWatermarkRx = error "Not implemented yet."

evtSystemEndpointWatermarkTx :: a
evtSystemEndpointWatermarkTx = error "Not implemented yet."

evtSystemNoLicenseKey :: a
evtSystemNoLicenseKey = error "Not implemented yet."

-- Event handler for protocol errors
evtSystemProtocolError :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => (BGAPIError -> IO Bool) -> m ThreadId
evtSystemProtocolError handler = registerEventHandler BgMsgEvent BgBlue BgClsSystem 0x06 handler

evtSystemScriptFailure :: a
evtSystemScriptFailure = error "Not implemented yet."

evtSystemUsbEnumerated :: a
evtSystemUsbEnumerated = error "Not implemented yet."

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode :: a
testChannelMode = error "Not implemented yet."

testGetChannelMap :: a
testGetChannelMap = error "Not implemented yet."

testPhyEnd :: a
testPhyEnd = error "Not implemented yet."

testPhyRx :: a
testPhyRx = error "Not implemented yet."

testPhyTx :: a
testPhyTx = error "Not implemented yet."

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress :: a
dfuFlashSetAddress = error "Not implemented yet."

dfuFlashUpload :: a
dfuFlashUpload = error "Not implemented yet."

dfuFlashUploadFinish :: a
dfuFlashUploadFinish = error "Not implemented yet."

dfuReset :: a
dfuReset = error "Not implemented yet."

evtDfuBoot :: a
evtDfuBoot = error "Not implemented yet."
