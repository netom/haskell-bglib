{-# LANGUAGE RecordWildCards #-}

module Commands
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

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import           Control.Monad.STM
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
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
attclientAttributeWrite = undefined

attclientExecuteWrite :: a
attclientExecuteWrite = undefined

attclientFindByTypeValue :: a
attclientFindByTypeValue = undefined

attclientFindInformation :: a
attclientFindInformation = undefined

attclientIndicateConfirm :: a
attclientIndicateConfirm = undefined

attclientPrepareWrite :: a
attclientPrepareWrite = undefined

attclientReadByGroupType :: a
attclientReadByGroupType = undefined

attclientReadByHandle :: a
attclientReadByHandle = undefined

attclientReadByType :: a
attclientReadByType = undefined

attclientReadLong :: a
attclientReadLong = undefined

attclientReadMultiple :: a
attclientReadMultiple = undefined

attclientWriteCommand :: a
attclientWriteCommand = undefined

evtAttclientAttributeValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((UInt8, UInt16, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtAttclientAttributeValue handler = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x05 handler

evtAttclientFindInformationFound :: a
evtAttclientFindInformationFound = undefined

evtAttclientGroupFound :: a
evtAttclientGroupFound = undefined

evtAttclientIndicated :: a
evtAttclientIndicated = undefined

evtAttclientProcedureCompleted :: a
evtAttclientProcedureCompleted = undefined

evtAttclientReadMultipleResponse :: a
evtAttclientReadMultipleResponse = undefined

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributesRead :: a
attributesRead = undefined

attributesReadType :: a
attributesReadType = undefined

attributesSend :: a
attributesSend = undefined

attributesUserReadResponse :: a
attributesUserReadResponse = undefined

attributesUserWriteResponse :: a
attributesUserWriteResponse = undefined

attributesWrite :: a
attributesWrite = undefined

evtAttributesStatus :: a
evtAttributesStatus = undefined

evtAttributesUserReadRequest :: a
evtAttributesUserReadRequest = undefined

evtAttributesValue :: a
evtAttributesValue = undefined

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connectionChannelMapGet :: a
connectionChannelMapGet = undefined

connectionChannelMapSet :: a
connectionChannelMapSet = undefined

connectionDisconnect :: a
connectionDisconnect = undefined

connectionGetRssi :: a
connectionGetRssi = undefined

connectionGetStatus :: a
connectionGetStatus = undefined

connectionSlaveLatencyDisable :: a
connectionSlaveLatencyDisable = undefined

connectionUpdate :: a
connectionUpdate = undefined

connectionVersionUpdate :: a
connectionVersionUpdate = undefined

evtConnectionDisconnected :: a
evtConnectionDisconnected = undefined

evtConnectionFeatureInd :: a
evtConnectionFeatureInd = undefined

evtConnectionStatus :: a
evtConnectionStatus = undefined

evtConnectionVersionInd :: a
evtConnectionVersionInd = undefined

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gapConnectDirect :: a
gapConnectDirect = undefined

gapConnectSelective :: a
gapConnectSelective = undefined

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
gapSetAdvData = undefined

gapSetAdvParameters :: a
gapSetAdvParameters = undefined

gapSetDirectedConnectableMode :: a
gapSetDirectedConnectableMode = undefined

gapSetFiltering :: a
gapSetFiltering = undefined

gapSetInitiatingConParameters :: a
gapSetInitiatingConParameters = undefined

gapSetMode :: a
gapSetMode = undefined

gapSetNonresolvableAddress :: a
gapSetNonresolvableAddress = undefined

gapSetPrivacyFlags :: a
gapSetPrivacyFlags = undefined

gapSetScanParameters :: a
gapSetScanParameters = undefined

-- Register an event handler for GAP scan responses
evtGapScanResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((Int8, UInt8, BdAddr, UInt8, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtGapScanResponse handler = registerEventHandler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 handler

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardwareAdcRead :: a
hardwareAdcRead = undefined

hardwareAnalogComparatorConfigIrq :: a
hardwareAnalogComparatorConfigIrq = undefined

hardwareAnalogComparatorEnable :: a
hardwareAnalogComparatorEnable = undefined

hardwareAnalogComparatorRead :: a
hardwareAnalogComparatorRead = undefined

hardwareGetTimestamp :: a
hardwareGetTimestamp = undefined

hardwareI2cRead :: a
hardwareI2cRead = undefined

hardwareI2cWrite :: a
hardwareI2cWrite = undefined

hardwareIoPortConfigDirection :: a
hardwareIoPortConfigDirection = undefined

hardwareIoPortConfigFunction :: a
hardwareIoPortConfigFunction = undefined

hardwareIoPortConfigPull :: a
hardwareIoPortConfigPull = undefined

hardwareIoPortIrqDirection :: a
hardwareIoPortIrqDirection = undefined

hardwareIoPortIrqEnable :: a
hardwareIoPortIrqEnable = undefined

hardwareIoPortRead :: a
hardwareIoPortRead = undefined

hardwareIoPortWrite :: a
hardwareIoPortWrite = undefined

hardwareSetRxgain :: a
hardwareSetRxgain = undefined

hardwareSetSoftTimer :: a
hardwareSetSoftTimer = undefined

hardwareSetTxpower :: a
hardwareSetTxpower = undefined

hardwareSleepEnable :: a
hardwareSleepEnable = undefined

hardwareSpiConfig :: a
hardwareSpiConfig = undefined

hardwareSpiTransfer :: a
hardwareSpiTransfer = undefined

hardwareTimerComparator :: a
hardwareTimerComparator = undefined

hardwareUsbEnable :: a
hardwareUsbEnable = undefined

evtHardwareAdcResult :: a
evtHardwareAdcResult = undefined

evtHardwareAnalogComparatorStatus :: a
evtHardwareAnalogComparatorStatus = undefined

evtHardwareIoPortStatus :: a
evtHardwareIoPortStatus = undefined

evtHardwareSoftTimer :: a
evtHardwareSoftTimer = undefined

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flashErasePage :: a
flashErasePage = undefined

flashPsDefrag :: a
flashPsDefrag = undefined

flashPsDump :: a
flashPsDump = undefined

flashPsEraseAll :: a
flashPsEraseAll = undefined

flashPsErase :: a
flashPsErase = undefined

flashPsLoad :: a
flashPsLoad = undefined

flashPsSave :: a
flashPsSave = undefined

flashReadData :: a
flashReadData = undefined

flashWriteData :: a
flashWriteData = undefined

evtFlashPsKey :: a
evtFlashPsKey = undefined

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding :: a
smDeleteBonding = undefined

smEncryptStart :: a
smEncryptStart = undefined

smGetBonds :: a
smGetBonds = undefined

smPasskeyEntry :: a
smPasskeyEntry = undefined

setBondableMode :: a
setBondableMode = undefined

smSetOobData :: a
smSetOobData = undefined

smSetPairingDistributionKeys :: a
smSetPairingDistributionKeys = undefined

smSetParameters :: a
smSetParameters = undefined

smWhitelistBonds :: a
smWhitelistBonds = undefined

evtSmBondingFail :: a
evtSmBondingFail = undefined

evtSmBondStatus :: a
evtSmBondStatus = undefined

evtSmPasskeyDisplay :: a
evtSmPasskeyDisplay = undefined

evtSmPasskeyRequest :: a
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

systemDelayReset :: a
systemDelayReset = undefined

systemEndpointRx :: a
systemEndpointRx = undefined

systemEndpointSetWatermarks :: a
systemEndpointSetWatermarks = undefined

systemEndpointTx :: a
systemEndpointTx = undefined

systemGetBootloaderCrc :: a
systemGetBootloaderCrc = undefined

systemGetConnections :: a
systemGetConnections = undefined

systemGetCounters :: a
systemGetCounters = undefined

systemGetInfo :: a
systemGetInfo = undefined


-- This command can be used to test if the local device is functional. Similar to a typical "AT" -> "OK" test.
systemHello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

-- This command resets the local device immediately. The command does not have a response.
systemReset :: (MonadIO m, MonadReader env m, HasSerialPort env) => RebootMode -> m ()
systemReset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

systemUsbEnumerationStatusGet :: a
systemUsbEnumerationStatusGet = undefined

systemWhitelistAppend :: a
systemWhitelistAppend = undefined

systemWhitelistClear :: a
systemWhitelistClear = undefined

systemWhitelistRemove :: a
systemWhitelistRemove = undefined

evtSystemBoot :: a
evtSystemBoot = undefined

evtSystemEndpointWatermarkRx :: a
evtSystemEndpointWatermarkRx = undefined

evtSystemEndpointWatermarkTx :: a
evtSystemEndpointWatermarkTx = undefined

evtSystemNoLicenseKey :: a
evtSystemNoLicenseKey = undefined

-- Event handler for protocol errors
evtSystemProtocolError :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => (BGAPIError -> IO Bool) -> m ThreadId
evtSystemProtocolError handler = registerEventHandler BgMsgEvent BgBlue BgClsSystem 0x06 handler

evtSystemScriptFailure :: a
evtSystemScriptFailure = undefined

evtSystemUsbEnumerated :: a
evtSystemUsbEnumerated = undefined

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode :: a
testChannelMode = undefined

testGetChannelMap :: a
testGetChannelMap = undefined

testPhyEnd :: a
testPhyEnd = undefined

testPhyRx :: a
testPhyRx = undefined

testPhyTx :: a
testPhyTx = undefined

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress :: a
dfuFlashSetAddress = undefined

dfuFlashUpload :: a
dfuFlashUpload = undefined

dfuFlashUploadFinish :: a
dfuFlashUploadFinish = undefined

dfuReset :: a
dfuReset = undefined

evtDfuBoot :: a
evtDfuBoot = undefined
