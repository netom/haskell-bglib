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

recv' :: SerialPort -> Int -> IO BSS.ByteString
recv' s n = do
    bs <- recv s n
    let len = BSS.length bs
    if  len >= n
        then return bs
        else do
            putStrLn "* SHORT!"
            BSS.append bs <$> recv' s (n - len)

-- Read one BgPacket from a SerialPort
readBGPacket' :: Bool -> SerialPort -> IO (Maybe BgPacket)
readBGPacket' dbg s = do
    bsHeader <- recv' s 4
    let eHeader = decodeOrFail $ BSL.fromStrict bsHeader

    case eHeader of
        Left _ -> do
            when dbg $ do
                putStr $ "[DEBUG] ERROR: decoding header: " ++ bsShowHex bsHeader
            return Nothing
        Right (_, _, bgpHeader@BgPacketHeader{..}) -> do
            bsPayload <- recv' s (fromIntegral bghLength)
            let bgpPayload = toBgPayload bsPayload
            let p = BgPacket {..}
            when dbg $ do
                putStr "[DEBUG]  READ: "
                putStrLn $ show p
            return $ Just p

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
        Just p <- readBGPacket' dbg s
        atomically $ writeTChan c p
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

-- eXecute a Command, don't wait for answer
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
    p <- liftIO $ waitForPacket chan mt tt cc cid
    let eRes = decodeOrFail $ BSL.fromStrict $ fromBgPayload $ bgpPayload p
    case eRes of
        Left _ -> do
            error "ERROR decoding packet."
        Right (_, _, res) -> 
            return res

-- Handle a specific type of packet
--
-- Event handler block by waiting on the TChan provided by the
-- environment. One can use forkIO to make event handler run in
-- independent threads, or use race with threadDelay to wait for an
-- event with a timeout.
handlePacket
    :: (Binary a, MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env)
    => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> (a -> m (Maybe b)) -> m b
handlePacket mt tt cc cid handler = do
    chan <- askBGChan
    go chan
    where
        go chan = do
            BgPacket{..} <- liftIO $ waitForPacket chan mt tt cc cid
            mbResult <- handler $ decode $ BSL.fromStrict $ fromBgPayload $ bgpPayload
            case mbResult of
                Nothing -> go chan
                Just r  -> return r

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

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 func (a, b, c, d, e, f, g) = func a b c d e f g

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
    => (UInt8 -> UInt16 -> UInt8 -> UInt8Array -> m (Maybe a)) -> m a
evtAttclientAttributeValue
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x05 . uncurry4

evtAttclientFindInformationFound
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt8Array -> m (Maybe a)) -> m a
evtAttclientFindInformationFound
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x04 . uncurry3

evtAttclientGroupFound
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt16 -> UInt8Array -> m (Maybe a)) -> m a
evtAttclientGroupFound
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x02 . uncurry4

evtAttclientIndicated
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> m (Maybe a)) -> m a
evtAttclientIndicated
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x00 . uncurry

evtAttclientProcedureCompleted
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> BGResult -> UInt16 -> m (Maybe a)) -> m a
evtAttclientProcedureCompleted
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x01 . uncurry3

evtAttclientReadMultipleResponse
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8Array -> m (Maybe a)) -> m a
evtAttclientReadMultipleResponse
    = handlePacket BgMsgEvent BgBlue BgClsAttributeClient 0x06 . uncurry

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
    => (UInt16 -> UInt8 -> m (Maybe a)) -> m a
evtAttributesStatus
    = handlePacket BgMsgEvent BgBlue BgClsAttributeDatabase 0x02 . uncurry

evtAttributesUserReadRequest
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt16 -> UInt16 -> UInt8 -> m (Maybe a)) -> m a
evtAttributesUserReadRequest
    = handlePacket BgMsgEvent BgBlue BgClsAttributeDatabase 0x01 . uncurry4

evtAttributesValue
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> UInt16 -> UInt16 -> UInt8Array -> m (Maybe a)) -> m a
evtAttributesValue
    = handlePacket BgMsgEvent BgBlue BgClsAttributeDatabase 0x00 . uncurry5

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
    => (UInt8 -> BGResult -> m (Maybe a)) -> m a
evtConnectionDisconnected
    = handlePacket BgMsgEvent BgBlue BgClsConnection 0x04 . uncurry

evtConnectionFeatureInd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8Array -> m (Maybe a)) -> m a
evtConnectionFeatureInd = handlePacket BgMsgEvent BgBlue BgClsConnection 0x02 . uncurry

evtConnectionStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> BdAddr -> UInt8 -> UInt16 -> UInt16 -> UInt16 -> UInt8 -> m (Maybe a))
    -> m a
evtConnectionStatus = handlePacket BgMsgEvent BgBlue BgClsConnection 0x00 . uncurry8

evtConnectionVersionInd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> UInt16 -> UInt16 -> m (Maybe a)) -> m a
evtConnectionVersionInd = handlePacket BgMsgEvent BgBlue BgClsConnection 0x01 . uncurry4

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
    => (Int8 -> UInt8 -> BdAddr -> GapAddressType -> UInt8 -> UInt8Array -> m (Maybe a)) -> m a
evtGapScanResponse
    = handlePacket BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 . uncurry6

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
    => (UInt8 -> UInt16 -> m (Maybe a)) -> m a
evtHardwareAdcResult
    = handlePacket BgMsgEvent BgBlue BgClsHardware 0x02 . uncurry

evtHardwareAnalogComparatorStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt32 -> UInt8 -> m (Maybe a)) -> m a
evtHardwareAnalogComparatorStatus
    = handlePacket BgMsgEvent BgBlue BgClsHardware 0x03 . uncurry

evtHardwareIoPortStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt32 -> UInt8 -> UInt8 -> UInt8 -> m (Maybe a)) -> m a
evtHardwareIoPortStatus
    = handlePacket BgMsgEvent BgBlue BgClsHardware 0x00 . uncurry4

evtHardwareSoftTimer
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> m (Maybe a)) -> m a
evtHardwareSoftTimer
    = handlePacket BgMsgEvent BgBlue BgClsHardware 0x01

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
    => (UInt16 -> UInt8Array -> m (Maybe a)) -> m a
evtFlashPsKey
    = handlePacket BgMsgEvent BgBlue BgClsPersistentStore 0x00 . uncurry

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

smDeleteBonding
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m BGResult
smDeleteBonding = xCmd BgMsgCR BgBlue BgClsSecurityManager 0x02

smEncryptStart
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> Bool -> m (UInt8, BGResult)
smEncryptStart = curry $ xCmd BgMsgCR BgBlue BgClsSecurityManager 0x00

smGetBonds
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt8
smGetBonds = xCmd BgMsgCR BgBlue BgClsSecurityManager 0x05 ()

smPasskeyEntry
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt32 -> m BGResult
smPasskeyEntry = curry $ xCmd BgMsgCR BgBlue BgClsSecurityManager 0x04

setBondableMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m ()
setBondableMode = xCmd BgMsgCR BgBlue BgClsSecurityManager 0x01

smSetOobData
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m ()
smSetOobData = xCmd BgMsgCR BgBlue BgClsSecurityManager 0x06

smSetPairingDistributionKeys
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m BGResult
smSetPairingDistributionKeys = curry $ xCmd BgMsgCR BgBlue BgClsSecurityManager 0x08

smSetParameters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> UInt8 -> SMIOCapabilities -> m ()
smSetParameters = curry3 $ xCmd BgMsgCR BgBlue BgClsSecurityManager 0x03

smWhitelistBonds
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m (BGResult, UInt8)
smWhitelistBonds = xCmd BgMsgCR BgBlue BgClsSecurityManager 0x07 ()

evtSmBondingFail
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> BGResult -> m (Maybe a)) -> m a
evtSmBondingFail
    = handlePacket BgMsgEvent BgBlue BgClsSecurityManager 0x01 . uncurry

evtSmBondStatus
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> Bool -> UInt8 -> m (Maybe a)) -> m a
evtSmBondStatus
    = handlePacket BgMsgEvent BgBlue BgClsSecurityManager 0x04 . uncurry4

evtSmPasskeyDisplay
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt32 -> m (Maybe a)) -> m a
evtSmPasskeyDisplay
    = handlePacket BgMsgEvent BgBlue BgClsSecurityManager 0x02 . uncurry

evtSmPasskeyRequest
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> m (Maybe a)) -> m a
evtSmPasskeyRequest
    = handlePacket BgMsgEvent BgBlue BgClsSecurityManager 0x03

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
systemAesDecrypt = xCmd BgMsgCR BgBlue BgClsSystem 0x11

systemAesEncrypt
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m UInt8Array
systemAesEncrypt = xCmd BgMsgCR BgBlue BgClsSystem 0x10

systemAesSetkey
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m ()
systemAesSetkey = xCmd BgMsgCR BgBlue BgClsSystem 0x0f

systemDelayReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => RebootMode -> UInt16 -> m ()
systemDelayReset = curry $ xCmd' BgMsgCR BgBlue BgClsSystem 0x14

systemEndpointRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> m (BGResult, UInt8Array)
systemEndpointRx = curry $ xCmd BgMsgCR BgBlue BgClsSystem 0x0d

systemEndpointSetWatermarks
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8 -> m BGResult
systemEndpointSetWatermarks = curry3 $ xCmd BgMsgCR BgBlue BgClsSystem 0x0e

systemEndpointTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8Array -> m BGResult
systemEndpointTx = curry $ xCmd BgMsgCR BgBlue BgClsSystem 0x09

systemGetBootloaderCrc
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt16
systemGetBootloaderCrc = xCmd BgMsgCR BgBlue BgClsSystem 0x13 ()

systemGetConnections
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt8
systemGetConnections = xCmd BgMsgCR BgBlue BgClsSystem 0x06 ()

systemGetCounters
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m (UInt8, UInt8, UInt8, UInt8, UInt8)
systemGetCounters = xCmd BgMsgCR BgBlue BgClsSystem 0x05 ()

systemGetInfo
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m (UInt16, UInt16, UInt16, UInt16, UInt16, UInt8, UInt8)
systemGetInfo = xCmd BgMsgCR BgBlue BgClsSystem 0x08 ()

systemHello
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemHello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

systemReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasDebug env)
    => RebootMode -> m ()
systemReset = xCmd' BgMsgCR BgBlue BgClsSystem 0x01

systemUsbEnumerationStatusGet
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m (BGResult, Bool)
systemUsbEnumerationStatusGet = xCmd BgMsgCR BgBlue BgClsSystem 0x12 ()

systemWhitelistAppend
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => BdAddr -> GapAddressType -> m BGResult
systemWhitelistAppend = curry $ xCmd BgMsgCR BgBlue BgClsSystem 0x0a

systemWhitelistClear
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m ()
systemWhitelistClear = xCmd BgMsgCR BgBlue BgClsSystem 0x0c ()

systemWhitelistRemove
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => BdAddr -> GapAddressType -> m BGResult
systemWhitelistRemove = curry $ xCmd BgMsgCR BgBlue BgClsSystem 0x0b

evtSystemBoot
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt16 -> UInt16 -> UInt16 -> UInt16 -> UInt16 -> UInt8 -> UInt8 -> m (Maybe a)) -> m a
evtSystemBoot
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x00 . uncurry7

evtSystemEndpointWatermarkRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> m (Maybe a)) -> m a
evtSystemEndpointWatermarkRx
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x02 . uncurry

evtSystemEndpointWatermarkTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt8 -> UInt8 -> m (Maybe a)) -> m a
evtSystemEndpointWatermarkTx
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x03 . uncurry

evtSystemNoLicenseKey
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (() -> m (Maybe a)) -> m a
evtSystemNoLicenseKey
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x05

evtSystemProtocolError
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (BGResult -> m (Maybe a)) -> m a
evtSystemProtocolError
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x06

evtSystemScriptFailure
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt16 -> BGResult -> m (Maybe a)) -> m a
evtSystemScriptFailure
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x04 . uncurry

evtSystemUsbEnumerated
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (Bool -> m (Maybe a)) -> m a
evtSystemUsbEnumerated
    = handlePacket BgMsgEvent BgBlue BgClsSystem 0x07

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

testChannelMode
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m ()
testChannelMode = xCmd BgMsgCR BgBlue BgClsTest 0x06

testGetChannelMap
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt8Array
testGetChannelMap = xCmd BgMsgCR BgBlue BgClsTest 0x04 ()

testPhyEnd
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m UInt16
testPhyEnd = xCmd BgMsgCR BgBlue BgClsTest 0x02 ()

testPhyRx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> m ()
testPhyRx = xCmd BgMsgCR BgBlue BgClsTest 0x01

testPhyTx
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8 -> UInt8 -> UInt8 -> m ()
testPhyTx = curry3 $ xCmd BgMsgCR BgBlue BgClsTest 0x00

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfuFlashSetAddress
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt32 -> m BGResult
dfuFlashSetAddress = xCmd BgMsgCR BgBlue BgClsDfu 0x01

dfuFlashUpload
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => UInt8Array -> m BGResult
dfuFlashUpload = xCmd BgMsgCR BgBlue BgClsDfu 0x02

dfuFlashUploadFinish
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => m BGResult
dfuFlashUploadFinish = xCmd BgMsgCR BgBlue BgClsDfu 0x03 ()

dfuReset
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => Bool -> m ()
dfuReset = xCmd' BgMsgCR BgBlue BgClsDfu 0x00

evtDfuBoot
    :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, HasDebug env)
    => (UInt32 -> m (Maybe a)) -> m a
evtDfuBoot = handlePacket BgMsgEvent BgBlue BgClsDfu 0x00
