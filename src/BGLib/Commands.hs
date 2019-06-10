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
    , hardwareIoPortConfigIrq
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

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 func a b c = func (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 func a b c d = func (a, b, c, d)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 func (a, b, c, d) = func a b c d

curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f
curry5 func a b c d e = func (a, b, c, d, e)

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 func (a, b, c, d, e) = func a b c d e

curry6 :: ((a, b, c, d, e, f) -> g) -> a -> b -> c -> d -> e -> f -> g
curry6 func a b c d e f = func (a, b, c, d, e, f)

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 func (a, b, c, d, e, f) = func a b c d e f

uncurry8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a, b, c, d, e, f, g, h) -> i
uncurry8 func (a, b, c, d, e, f, g, h) = func a b c d e f g h

-----------------------------------------------------------------------
-- Attribute Client
-----------------------------------------------------------------------
--gapDiscover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode


attclientAttributeWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientAttributeWrite = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x05

attclientExecuteWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> Bool -> m (UInt8, BGResult)
attclientExecuteWrite = curry $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x0a

attclientFindByTypeValue
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientFindByTypeValue = curry5 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x00

attclientFindInformation
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> m (UInt8, BGResult)
attclientFindInformation = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x03

attclientIndicateConfirm
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m BGResult
attclientIndicateConfirm = xCmd BgMsgCR BgBlue BgClsAttributeClient 0x07

attclientPrepareWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientPrepareWrite = curry4 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x09

attclientReadByGroupType
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientReadByGroupType = curry4 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x01

attclientReadByHandle
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> m (UInt8, BGResult)
attclientReadByHandle = curry $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x04

attclientReadByType
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientReadByType = curry4 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x02

attclientReadLong
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> m (UInt8, BGResult)
attclientReadLong = curry $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x08

attclientReadMultiple
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8Array -> m (UInt8, BGResult)
attclientReadMultiple = curry $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x0b

attclientWriteCommand
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt8Array -> m (UInt8, BGResult)
attclientWriteCommand = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeClient 0x06

evtAttclientAttributeValue
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt8 -> UInt8Array -> IO Bool) -> m ThreadId
evtAttclientAttributeValue
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x05 . uncurry4

evtAttclientFindInformationFound
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt8Array -> IO Bool) -> m ThreadId
evtAttclientFindInformationFound
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x04 . uncurry3

evtAttclientGroupFound
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt16 -> UInt8Array -> IO Bool) -> m ThreadId
evtAttclientGroupFound
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x02 . uncurry4

evtAttclientIndicated
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> IO Bool) -> m ThreadId
evtAttclientIndicated
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x00 . uncurry

evtAttclientProcedureCompleted
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> BGResult -> UInt16 -> IO Bool) -> m ThreadId
evtAttclientProcedureCompleted
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x01 . uncurry3

evtAttclientReadMultipleResponse
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8Array -> IO Bool) -> m ThreadId
evtAttclientReadMultipleResponse
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeClient 0x06 . uncurry

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributesRead
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt16 -> m (UInt16, UInt16, BGResult, UInt8Array)
attributesRead = curry $ xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x01

attributesReadType
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> m (UInt16, BGResult, UInt8Array)
attributesReadType = xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x02

attributesSend
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt8Array -> m BGResult
attributesSend = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x05

attributesUserReadResponse
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8Array -> m ()
attributesUserReadResponse = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x03

attributesUserWriteResponse
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m ()
attributesUserWriteResponse = curry $ xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x04

attributesWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt8 -> UInt8Array -> m BGResult
attributesWrite = curry3 $ xCmd BgMsgCR BgBlue BgClsAttributeDatabase 0x00

evtAttributesStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt16 -> UInt8 -> IO Bool) -> m ThreadId
evtAttributesStatus
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeDatabase 0x02 . uncurry

evtAttributesUserReadRequest
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt16 -> UInt8 -> IO Bool) -> m ThreadId
evtAttributesUserReadRequest
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeDatabase 0x01 . uncurry4

evtAttributesValue
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> UInt16 -> UInt16 -> UInt8Array -> IO Bool) -> m ThreadId
evtAttributesValue
    = registerEventHandler BgMsgEvent BgBlue BgClsAttributeDatabase 0x00 . uncurry5

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connectionChannelMapGet
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m (UInt8, UInt8Array)
connectionChannelMapGet = xCmd BgMsgCR BgBlue BgClsConnection 0x04

connectionChannelMapSet
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8Array -> m (UInt8, BGResult)
connectionChannelMapSet = curry $ xCmd BgMsgCR BgBlue BgClsConnection 0x05

connectionDisconnect
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m (UInt8, BGResult)
connectionDisconnect = xCmd BgMsgCR BgBlue BgClsConnection 0x00

connectionGetRssi
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m (UInt8, Int8)
connectionGetRssi = xCmd BgMsgCR BgBlue BgClsConnection 0x01

connectionGetStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m UInt8
connectionGetStatus = xCmd BgMsgCR BgBlue BgClsConnection 0x07

connectionSlaveLatencyDisable
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m BGResult
connectionSlaveLatencyDisable = xCmd BgMsgCR BgBlue BgClsConnection 0x09

connectionUpdate
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt16 -> UInt16 -> UInt16 -> UInt16 -> m (UInt8, BGResult)
connectionUpdate = curry5 $ xCmd BgMsgCR BgBlue BgClsConnection 0x02

connectionVersionUpdate
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m (UInt8, BGResult)
connectionVersionUpdate = xCmd BgMsgCR BgBlue BgClsConnection 0x03

evtConnectionDisconnected
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> BGResult -> IO Bool) -> m ThreadId
evtConnectionDisconnected
    = registerEventHandler BgMsgEvent BgBlue BgClsConnection 0x04 . uncurry

evtConnectionFeatureInd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8Array -> IO Bool) -> m ThreadId
evtConnectionFeatureInd = registerEventHandler BgMsgEvent BgBlue BgClsConnection 0x02 . uncurry

evtConnectionStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> BdAddr -> UInt8 -> UInt16 -> UInt16 -> UInt16 -> UInt8 -> IO Bool)
    -> m ThreadId
evtConnectionStatus = registerEventHandler BgMsgEvent BgBlue BgClsConnection 0x00 . uncurry8

evtConnectionVersionInd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> UInt16 -> UInt16 -> IO Bool) -> m ThreadId
evtConnectionVersionInd = registerEventHandler BgMsgEvent BgBlue BgClsConnection 0x01 . uncurry4

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gapConnectDirect
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => BdAddr -> GapAddressType -> UInt16 -> UInt16 -> UInt16 -> UInt16 -> m (BGResult, UInt8)
gapConnectDirect = curry6 $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x03

gapConnectSelective
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt16 -> UInt16 -> UInt16 -> m (BGResult, UInt8)
gapConnectSelective = curry4 $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x05

gapDiscover
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => GapDiscoverMode -> m UInt16
gapDiscover = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02

gapEndProcedure
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt16
gapEndProcedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

gapSetAdvData
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8Array -> m BGResult
gapSetAdvData = curry $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x09

gapSetAdvParameters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt16 -> UInt8 -> m BGResult
gapSetAdvParameters = curry3 $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x08

gapSetDirectedConnectableMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => BdAddr -> GapAddressType -> m BGResult
gapSetDirectedConnectableMode = curry $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x0a

gapSetFiltering
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => GapScanPolicy -> GapAdvPolicy -> UInt8 -> m BGResult
gapSetFiltering = curry3 $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x06

gapSetInitiatingConParameters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt16 -> m BGResult
gapSetInitiatingConParameters = curry $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x0b

gapSetMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => GapDiscoverableMode -> GapConnectableMode -> m BGResult
gapSetMode = curry $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x01

gapSetNonresolvableAddress
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => BdAddr -> m BGResult
gapSetNonresolvableAddress = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x0c

gapSetPrivacyFlags
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m ()
gapSetPrivacyFlags = curry $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x00

gapSetScanParameters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt16 -> UInt8 -> m BGResult
gapSetScanParameters = curry3 $ xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x07

-- Register an event handler for GAP scan responses
evtGapScanResponse
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (Int8 -> UInt8 -> BdAddr -> UInt8 -> UInt8 -> UInt8Array -> IO Bool) -> m ThreadId
evtGapScanResponse
    = registerEventHandler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 . uncurry6

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardwareAdcRead
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8 -> m BGResult
hardwareAdcRead = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x02

hardwareAnalogComparatorConfigIrq
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m BGResult
hardwareAnalogComparatorConfigIrq = xCmd BgMsgCR BgBlue BgClsHardware 0x12

hardwareAnalogComparatorEnable
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m ()
hardwareAnalogComparatorEnable = xCmd BgMsgCR BgBlue BgClsHardware 0x10

hardwareAnalogComparatorRead
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m (BGResult, UInt8)
hardwareAnalogComparatorRead = xCmd BgMsgCR BgBlue BgClsHardware 0x11 ()

hardwareGetTimestamp
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt32
hardwareGetTimestamp = xCmd BgMsgCR BgBlue BgClsHardware 0x16 ()

hardwareI2cRead
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> Bool -> UInt8 -> m (UInt16, UInt8Array)
hardwareI2cRead = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x0a

hardwareI2cWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> Bool -> UInt8Array -> m UInt8
hardwareI2cWrite = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x0b

hardwareIoPortConfigDirection
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m BGResult
hardwareIoPortConfigDirection = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x03

hardwareIoPortConfigFunction
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m BGResult
hardwareIoPortConfigFunction = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x04

hardwareIoPortConfigIrq
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> Bool -> m BGResult
hardwareIoPortConfigIrq = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x00

hardwareIoPortConfigPull
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> Bool -> m BGResult
hardwareIoPortConfigPull = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x05

hardwareIoPortIrqDirection
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> Bool -> m BGResult
hardwareIoPortIrqDirection = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x0f

hardwareIoPortIrqEnable
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m BGResult
hardwareIoPortIrqEnable = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x0e

hardwareIoPortRead
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m (BGResult, UInt8, UInt8)
hardwareIoPortRead = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x07

hardwareIoPortWrite
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8 -> m BGResult
hardwareIoPortWrite = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x06

hardwareSetRxgain
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m ()
hardwareSetRxgain = xCmd BgMsgCR BgBlue BgClsHardware 0x13

hardwareSetSoftTimer
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt32 -> UInt8 -> Bool -> m BGResult
hardwareSetSoftTimer = curry3 $ xCmd BgMsgCR BgBlue BgClsHardware 0x01

hardwareSetTxpower
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m ()
hardwareSetTxpower = xCmd BgMsgCR BgBlue BgClsHardware 0x0c

hardwareSleepEnable
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m BGResult
hardwareSleepEnable = xCmd BgMsgCR BgBlue BgClsHardware 0x15

hardwareSpiConfig
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> Bool -> Bool -> Bool -> UInt8 -> UInt8 -> m BGResult
hardwareSpiConfig = curry6 $ xCmd BgMsgCR BgBlue BgClsHardware 0x08

hardwareSpiTransfer
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8Array -> m (BGResult, UInt8, UInt8Array)
hardwareSpiTransfer = curry $ xCmd BgMsgCR BgBlue BgClsHardware 0x09

hardwareTimerComparator
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8 -> UInt16 -> m BGResult
hardwareTimerComparator = curry4 $ xCmd BgMsgCR BgBlue BgClsHardware 0x0d

hardwareUsbEnable
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m BGResult
hardwareUsbEnable = xCmd BgMsgCR BgBlue BgClsHardware 0x14

evtHardwareAdcResult
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> IO Bool) -> m ThreadId
evtHardwareAdcResult
    = registerEventHandler BgMsgEvent BgBlue BgClsHardware 0x02 . uncurry

evtHardwareAnalogComparatorStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt32 -> UInt8 -> IO Bool) -> m ThreadId
evtHardwareAnalogComparatorStatus
    = registerEventHandler BgMsgEvent BgBlue BgClsHardware 0x03 . uncurry

evtHardwareIoPortStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt32 -> UInt8 -> UInt8 -> UInt8 -> IO Bool) -> m ThreadId
evtHardwareIoPortStatus
    = registerEventHandler BgMsgEvent BgBlue BgClsHardware 0x00 . uncurry4

evtHardwareSoftTimer
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> IO Bool) -> m ThreadId
evtHardwareSoftTimer
    = registerEventHandler BgMsgEvent BgBlue BgClsHardware 0x01

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flashErasePage
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m BGResult
flashErasePage = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x06

flashPsDefrag
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
flashPsDefrag = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x00 ()

flashPsDump
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
flashPsDump = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x01 ()

flashPsEraseAll
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
flashPsEraseAll = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x02 ()

flashPsErase
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> m ()
flashPsErase = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x05

flashPsLoad
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> m (BGResult, UInt8Array)
flashPsLoad = xCmd BgMsgCR BgBlue BgClsPersistentStore 0x04

flashPsSave
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt16 -> UInt8Array -> m BGResult
flashPsSave = curry $ xCmd BgMsgCR BgBlue BgClsPersistentStore 0x03

flashReadData
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt32 -> UInt8 -> m UInt8Array
flashReadData = curry $ xCmd BgMsgCR BgBlue BgClsPersistentStore 0x08

flashWriteData
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt32 -> UInt8Array -> m BGResult
flashWriteData = curry $ xCmd BgMsgCR BgBlue BgClsPersistentStore 0x07

evtFlashPsKey
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt16 -> UInt8Array -> IO Bool) -> m ThreadId
evtFlashPsKey
    = registerEventHandler BgMsgEvent BgBlue BgClsPersistentStore 0x00 . uncurry

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smDeleteBonding = error "Not implemented yet."

smEncryptStart
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smEncryptStart = error "Not implemented yet."

smGetBonds
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smGetBonds = error "Not implemented yet."

smPasskeyEntry
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smPasskeyEntry = error "Not implemented yet."

setBondableMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
setBondableMode = error "Not implemented yet."

smSetOobData
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smSetOobData = error "Not implemented yet."

smSetPairingDistributionKeys
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smSetPairingDistributionKeys = error "Not implemented yet."

smSetParameters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smSetParameters = error "Not implemented yet."

smWhitelistBonds
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
smWhitelistBonds = error "Not implemented yet."

evtSmBondingFail
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSmBondingFail = error "Not implemented yet."

evtSmBondStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSmBondStatus = error "Not implemented yet."

evtSmPasskeyDisplay
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSmPasskeyDisplay = error "Not implemented yet."

evtSmPasskeyRequest
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSmPasskeyRequest = error "Not implemented yet."

-----------------------------------------------------------------------
-- System
-----------------------------------------------------------------------

systemAddressGet
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m BdAddr
systemAddressGet = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()

systemAesDecrypt
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m UInt8Array
systemAesDecrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x11 dta

systemAesEncrypt
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m UInt8Array
systemAesEncrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x10 dta

systemAesSetkey
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m ()
systemAesSetkey key = xCmd BgMsgCR BgBlue BgClsSystem 0x0f key

systemDelayReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemDelayReset = error "Not implemented yet."

systemEndpointRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemEndpointRx = error "Not implemented yet."

systemEndpointSetWatermarks
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemEndpointSetWatermarks = error "Not implemented yet."

systemEndpointTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemEndpointTx = error "Not implemented yet."

systemGetBootloaderCrc
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemGetBootloaderCrc = error "Not implemented yet."

systemGetConnections
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemGetConnections = error "Not implemented yet."

systemGetCounters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemGetCounters = error "Not implemented yet."

systemGetInfo
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemGetInfo = error "Not implemented yet."

systemHello
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

systemReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasDebug env)
    => RebootMode -> m ()
systemReset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

systemUsbEnumerationStatusGet
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemUsbEnumerationStatusGet = error "Not implemented yet."

systemWhitelistAppend
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemWhitelistAppend = error "Not implemented yet."

systemWhitelistClear
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemWhitelistClear = error "Not implemented yet."

systemWhitelistRemove
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemWhitelistRemove = error "Not implemented yet."

evtSystemBoot
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemBoot = error "Not implemented yet."

evtSystemEndpointWatermarkRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemEndpointWatermarkRx = error "Not implemented yet."

evtSystemEndpointWatermarkTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemEndpointWatermarkTx = error "Not implemented yet."

evtSystemNoLicenseKey
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemNoLicenseKey = error "Not implemented yet."

evtSystemProtocolError
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (BGResult -> IO Bool) -> m ThreadId
evtSystemProtocolError
    = registerEventHandler BgMsgEvent BgBlue BgClsSystem 0x06

evtSystemScriptFailure
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemScriptFailure = error "Not implemented yet."

evtSystemUsbEnumerated
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtSystemUsbEnumerated = error "Not implemented yet."

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
testChannelMode = error "Not implemented yet."

testGetChannelMap
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
testGetChannelMap = error "Not implemented yet."

testPhyEnd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
testPhyEnd = error "Not implemented yet."

testPhyRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
testPhyRx = error "Not implemented yet."

testPhyTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
testPhyTx = error "Not implemented yet."

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
dfuFlashSetAddress = error "Not implemented yet."

dfuFlashUpload
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
dfuFlashUpload = error "Not implemented yet."

dfuFlashUploadFinish
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
dfuFlashUploadFinish = error "Not implemented yet."

dfuReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
dfuReset = error "Not implemented yet."

evtDfuBoot
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> IO Bool) -> m ThreadId
evtDfuBoot = error "Not implemented yet."
