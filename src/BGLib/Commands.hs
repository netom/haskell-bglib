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
writeBGPacket' :: Bool -> SerialPort -> BgPacket -> IO ()
writeBGPacket' dbg s p = do
    let packetBS = BSL.toStrict $ encode p
    when dbg $ do
        putStr "[DEBUG] WRITE: "
        putStrLn $ show p
    _ <- send s packetBS
    return ()

-- Write the BgPacket to the SerialPort in env asked from the MonadReader
writeBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env, HasDebug env) => BgPacket -> m ()
writeBGPacket p = do
    s <- askSerialPort
    dbg <- askDebug
    liftIO $ writeBGPacket' dbg s p

-- Read one BgPacket from a SerialPort
readBGPacket' :: Bool -> SerialPort -> IO BgPacket
readBGPacket' dbg s = do
    bgpHeader@BgPacketHeader{..} <- decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- toBgPayload <$> recv s (fromIntegral bghLength)
    let p = BgPacket {..}
    when dbg $ do
        putStr "[DEBUG]  READ: "
        putStrLn $ show p
    return p

-- Read one BgPacket from the SerialPort in env asked from the MonadReader
--readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
--readBGPacket = do
--    s <- askSerialPort
--    liftIO $ readBGPacket' s

-- Launch a thread that reads packets and sends them down a TChan BgPacket
startPacketReader :: (MonadIO m, MonadReader env m, HasBGChan env, HasSerialPort env, HasDebug env) => m () 
startPacketReader = do
    c <- askBGChan
    s <- askSerialPort
    dbg <- askDebug
    _ <- liftIO $ forkIO $ forever $ do
        --putStrLn "* READ *"
        readBGPacket' dbg s >>= atomically . writeTChan c
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
xCmd' :: (MonadIO m, MonadReader env m, HasSerialPort env, HasDebug env, Binary a ) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m ()
xCmd' mt tt cc cid inp = do
    let inpBS = BSL.toStrict $ encode inp
    writeBGPacket $ BgPacket (BgPacketHeader mt tt (fromIntegral $ BSS.length inpBS) cc cid) (toBgPayload inpBS)

-- Execute a command, wait for the appropriate answer
xCmd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env, Binary a, Binary b) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m b
xCmd mt tt cc cid inp = do
    -- We need to duplicate the channel BEFORE sending the command, so we don't miss the answer by accident
    chan <- askDupBGChan
    xCmd' mt tt cc cid inp
    decode . BSL.fromStrict . fromBgPayload . bgpPayload <$> ( liftIO $ waitForPacket chan mt tt cc cid )

registerEventHandler :: Binary a => (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> (a -> IO Bool) -> m ThreadId
registerEventHandler mt tt cc cid handler = do
    chan <- askDupBGChan
    liftIO $ forkIO $ go chan
    where
        go chan = do
            BgPacket{..} <- waitForPacket chan mt tt cc cid
            continue <- handler $ decode $ BSL.fromStrict $ fromBgPayload $ bgpPayload
            if continue then go chan else return ()

-----------------------------------------------------------------------
-- Attribute Client
-----------------------------------------------------------------------
--gapDiscover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode


attclientAttributeWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Connection -> Attribute -> UInt8Array -> m (Connection, BGResult)
attclientAttributeWrite conn attr dta = xCmd BgMsgCR BgBlue BgClsAttributeClient 0x05 (conn, attr, dta)

attclientExecuteWrite :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientExecuteWrite = error "Not implemented yet."

attclientFindByTypeValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientFindByTypeValue = error "Not implemented yet."

attclientFindInformation :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientFindInformation = error "Not implemented yet."

attclientIndicateConfirm :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientIndicateConfirm = error "Not implemented yet."

attclientPrepareWrite :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientPrepareWrite = error "Not implemented yet."

attclientReadByGroupType :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientReadByGroupType = error "Not implemented yet."

attclientReadByHandle :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientReadByHandle = error "Not implemented yet."

attclientReadByType :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientReadByType = error "Not implemented yet."

attclientReadLong :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientReadLong = error "Not implemented yet."

attclientReadMultiple :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientReadMultiple = error "Not implemented yet."

attclientWriteCommand :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attclientWriteCommand = error "Not implemented yet."

evtAttclientAttributeValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => ((UInt8, UInt16, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtAttclientAttributeValue handler = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x05 handler

evtAttclientFindInformationFound :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttclientFindInformationFound = error "Not implemented yet."

evtAttclientGroupFound :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttclientGroupFound = error "Not implemented yet."

evtAttclientIndicated :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttclientIndicated = error "Not implemented yet."

evtAttclientProcedureCompleted :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttclientProcedureCompleted = error "Not implemented yet."

evtAttclientReadMultipleResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttclientReadMultipleResponse = error "Not implemented yet."

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributesRead :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesRead = error "Not implemented yet."

attributesReadType :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesReadType = error "Not implemented yet."

attributesSend :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesSend = error "Not implemented yet."

attributesUserReadResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesUserReadResponse = error "Not implemented yet."

attributesUserWriteResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesUserWriteResponse = error "Not implemented yet."

attributesWrite :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
attributesWrite = error "Not implemented yet."

evtAttributesStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttributesStatus = error "Not implemented yet."

evtAttributesUserReadRequest :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttributesUserReadRequest = error "Not implemented yet."

evtAttributesValue :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtAttributesValue = error "Not implemented yet."

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connectionChannelMapGet :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionChannelMapGet = error "Not implemented yet."

connectionChannelMapSet :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionChannelMapSet = error "Not implemented yet."

connectionDisconnect :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionDisconnect = error "Not implemented yet."

connectionGetRssi :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionGetRssi = error "Not implemented yet."

connectionGetStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionGetStatus = error "Not implemented yet."

connectionSlaveLatencyDisable :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionSlaveLatencyDisable = error "Not implemented yet."

connectionUpdate :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionUpdate = error "Not implemented yet."

connectionVersionUpdate :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
connectionVersionUpdate = error "Not implemented yet."

evtConnectionDisconnected :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtConnectionDisconnected = error "Not implemented yet."

evtConnectionFeatureInd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtConnectionFeatureInd = error "Not implemented yet."

evtConnectionStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtConnectionStatus = error "Not implemented yet."

evtConnectionVersionInd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtConnectionVersionInd = error "Not implemented yet."

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gapConnectDirect :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapConnectDirect = error "Not implemented yet."

gapConnectSelective :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapConnectSelective = error "Not implemented yet."

-- This command starts the GAP discovery procedure to scan for advertising devices i.e. to perform a device
-- discovery.
-- Scanning parameters can be configured with the Set Scan Parameters command before issuing this command.
-- To cancel on an ongoing discovery process use the End Procedure command.
gapDiscover :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => GapDiscoverMode -> m UInt16
gapDiscover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode

-- This command ends the current GAP discovery procedure and stop the scanning of advertising devices.
gapEndProcedure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m UInt16
gapEndProcedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

gapSetAdvData :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetAdvData = error "Not implemented yet."

gapSetAdvParameters :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetAdvParameters = error "Not implemented yet."

gapSetDirectedConnectableMode :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetDirectedConnectableMode = error "Not implemented yet."

gapSetFiltering :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetFiltering = error "Not implemented yet."

gapSetInitiatingConParameters :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetInitiatingConParameters = error "Not implemented yet."

gapSetMode :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetMode = error "Not implemented yet."

gapSetNonresolvableAddress :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetNonresolvableAddress = error "Not implemented yet."

gapSetPrivacyFlags :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetPrivacyFlags = error "Not implemented yet."

gapSetScanParameters :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
gapSetScanParameters = error "Not implemented yet."

-- Register an event handler for GAP scan responses
evtGapScanResponse :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => ((Int8, UInt8, BdAddr, UInt8, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evtGapScanResponse handler = registerEventHandler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 handler

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardwareAdcRead :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareAdcRead = error "Not implemented yet."

hardwareAnalogComparatorConfigIrq :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareAnalogComparatorConfigIrq = error "Not implemented yet."

hardwareAnalogComparatorEnable :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareAnalogComparatorEnable = error "Not implemented yet."

hardwareAnalogComparatorRead :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareAnalogComparatorRead = error "Not implemented yet."

hardwareGetTimestamp :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareGetTimestamp = error "Not implemented yet."

hardwareI2cRead :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareI2cRead = error "Not implemented yet."

hardwareI2cWrite :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareI2cWrite = error "Not implemented yet."

hardwareIoPortConfigDirection :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortConfigDirection = error "Not implemented yet."

hardwareIoPortConfigFunction :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortConfigFunction = error "Not implemented yet."

hardwareIoPortConfigPull :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortConfigPull = error "Not implemented yet."

hardwareIoPortIrqDirection :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortIrqDirection = error "Not implemented yet."

hardwareIoPortIrqEnable :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortIrqEnable = error "Not implemented yet."

hardwareIoPortRead :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortRead = error "Not implemented yet."

hardwareIoPortWrite :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareIoPortWrite = error "Not implemented yet."

hardwareSetRxgain :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSetRxgain = error "Not implemented yet."

hardwareSetSoftTimer :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSetSoftTimer = error "Not implemented yet."

hardwareSetTxpower :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSetTxpower = error "Not implemented yet."

hardwareSleepEnable :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSleepEnable = error "Not implemented yet."

hardwareSpiConfig :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSpiConfig = error "Not implemented yet."

hardwareSpiTransfer :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareSpiTransfer = error "Not implemented yet."

hardwareTimerComparator :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareTimerComparator = error "Not implemented yet."

hardwareUsbEnable :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
hardwareUsbEnable = error "Not implemented yet."

evtHardwareAdcResult :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtHardwareAdcResult = error "Not implemented yet."

evtHardwareAnalogComparatorStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtHardwareAnalogComparatorStatus = error "Not implemented yet."

evtHardwareIoPortStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtHardwareIoPortStatus = error "Not implemented yet."

evtHardwareSoftTimer :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtHardwareSoftTimer = error "Not implemented yet."

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flashErasePage :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashErasePage = error "Not implemented yet."

flashPsDefrag :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsDefrag = error "Not implemented yet."

flashPsDump :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsDump = error "Not implemented yet."

flashPsEraseAll :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsEraseAll = error "Not implemented yet."

flashPsErase :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsErase = error "Not implemented yet."

flashPsLoad :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsLoad = error "Not implemented yet."

flashPsSave :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashPsSave = error "Not implemented yet."

flashReadData :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashReadData = error "Not implemented yet."

flashWriteData :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
flashWriteData = error "Not implemented yet."

evtFlashPsKey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtFlashPsKey = error "Not implemented yet."

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smDeleteBonding = error "Not implemented yet."

smEncryptStart :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smEncryptStart = error "Not implemented yet."

smGetBonds :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smGetBonds = error "Not implemented yet."

smPasskeyEntry :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smPasskeyEntry = error "Not implemented yet."

setBondableMode :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
setBondableMode = error "Not implemented yet."

smSetOobData :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smSetOobData = error "Not implemented yet."

smSetPairingDistributionKeys :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smSetPairingDistributionKeys = error "Not implemented yet."

smSetParameters :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smSetParameters = error "Not implemented yet."

smWhitelistBonds :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
smWhitelistBonds = error "Not implemented yet."

evtSmBondingFail :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSmBondingFail = error "Not implemented yet."

evtSmBondStatus :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSmBondStatus = error "Not implemented yet."

evtSmPasskeyDisplay :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSmPasskeyDisplay = error "Not implemented yet."

evtSmPasskeyRequest :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSmPasskeyRequest = error "Not implemented yet."

-----------------------------------------------------------------------
-- System
-----------------------------------------------------------------------

-- This command reads the local devices public Bluetooth address.
systemAddressGet :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m BdAddr
systemAddressGet = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()

-- This command decrypts the given data using the AES algorithm with the predefined key set with command Aes
-- Setkey .
systemAesDecrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => UInt8Array -> m UInt8Array
systemAesDecrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x11 dta

-- This command encrypts the given data using the AES algorithm with the predefined with command Aes Setkey .
systemAesEncrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => UInt8Array -> m UInt8Array
systemAesEncrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x10 dta

-- This command defines the encryption key that will be used with the AES encrypt and decrypt commands.
systemAesSetkey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => UInt8Array -> m ()
systemAesSetkey key = xCmd BgMsgCR BgBlue BgClsSystem 0x0f key

systemDelayReset :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemDelayReset = error "Not implemented yet."

systemEndpointRx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemEndpointRx = error "Not implemented yet."

systemEndpointSetWatermarks :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemEndpointSetWatermarks = error "Not implemented yet."

systemEndpointTx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemEndpointTx = error "Not implemented yet."

systemGetBootloaderCrc :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemGetBootloaderCrc = error "Not implemented yet."

systemGetConnections :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemGetConnections = error "Not implemented yet."

systemGetCounters :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemGetCounters = error "Not implemented yet."

systemGetInfo :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemGetInfo = error "Not implemented yet."


-- This command can be used to test if the local device is functional. Similar to a typical "AT" -> "OK" test.
systemHello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

-- This command resets the local device immediately. The command does not have a response.
systemReset :: (MonadIO m, MonadReader env m, HasSerialPort env, HasDebug env) => RebootMode -> m ()
systemReset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

systemUsbEnumerationStatusGet :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemUsbEnumerationStatusGet = error "Not implemented yet."

systemWhitelistAppend :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemWhitelistAppend = error "Not implemented yet."

systemWhitelistClear :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemWhitelistClear = error "Not implemented yet."

systemWhitelistRemove :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
systemWhitelistRemove = error "Not implemented yet."

evtSystemBoot :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemBoot = error "Not implemented yet."

evtSystemEndpointWatermarkRx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemEndpointWatermarkRx = error "Not implemented yet."

evtSystemEndpointWatermarkTx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemEndpointWatermarkTx = error "Not implemented yet."

evtSystemNoLicenseKey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemNoLicenseKey = error "Not implemented yet."

-- Event handler for protocol errors
evtSystemProtocolError :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (BGResult -> IO Bool) -> m ThreadId
evtSystemProtocolError handler = registerEventHandler BgMsgEvent BgBlue BgClsSystem 0x06 handler

evtSystemScriptFailure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemScriptFailure = error "Not implemented yet."

evtSystemUsbEnumerated :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtSystemUsbEnumerated = error "Not implemented yet."

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
testChannelMode = error "Not implemented yet."

testGetChannelMap :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
testGetChannelMap = error "Not implemented yet."

testPhyEnd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
testPhyEnd = error "Not implemented yet."

testPhyRx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
testPhyRx = error "Not implemented yet."

testPhyTx :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
testPhyTx = error "Not implemented yet."

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
dfuFlashSetAddress = error "Not implemented yet."

dfuFlashUpload :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
dfuFlashUpload = error "Not implemented yet."

dfuFlashUploadFinish :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
dfuFlashUploadFinish = error "Not implemented yet."

dfuReset :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => m ()
dfuReset = error "Not implemented yet."

evtDfuBoot :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env) => (a -> IO Bool) -> m ThreadId
evtDfuBoot = error "Not implemented yet."
