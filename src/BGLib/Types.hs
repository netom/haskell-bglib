{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BGLib.Types
    ( Int8
    , UInt8
    , UInt16
    , UInt32
    , UInt8Array(..)
    , toUInt8Array
    , BdAddr(..)
    , BgMessageType(..)
    , BgTecnologyType(..)
    , BgCommandClass(..)
    , BgPacketHeader(..)
    , bgHeaderMatches
    , BgPayload
    , fromBgPayload
    , toBgPayload
    , BgPacket(..)
    , HasSerialPort(..)
    , askSerialPort
    , HasBGChan(..)
    , askBGChan
    , askDupBGChan
    , askCloneBGChan
    , packetBlock
    , packetBlock_
    , packetBlock'
    , packetBlock'_
    , HasDebug(..)
    , askDebug
    , bsShowHex
    , RebootMode(..)
    , AttributeValueType(..)
    , AttributeChangeReason(..)
    , fASNotify
    , fASIndicate
    , fCConnected
    , fCEncrypted
    , fCCompleted
    , fCParametersChanged
    , fADLimitedDiscoverable
    , fADGeneralDiscoverable
    , fADBREDRNotSupported
    , fADSimultaneousLEBREDRCtrl
    , fADSimultaneousLEBREDRHost
    , fADMask
    , GapAdvType(..)
    , GapAdvPolicy(..)
    , GapAddressType(..)
    , GapConnectableMode(..)
    , GapDiscoverableMode(..)
    , GapDiscoverMode(..)
    , GSPScanHeaderFlag(..)
    , GapScanPolicy(..)
    , fBKLTK
    , fBKAddrPublic
    , fBKAddrStatic
    , fBKIRK
    , fBKEDIVRAND
    , fBKCSRK
    , fBKMasterId
    , SMIOCapabilities(..)
    , SystemEndpoint(..)
    , BGResult(..)
    ) where

import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString as BSS
import           Data.Data
import qualified Data.Int as I
import           Data.Ix
import           Data.String
import qualified Data.Word as W
import           Foreign.Storable
import           Numeric
import           System.Hardware.Serialport
import           Text.Printf 

-- int8           1 byte Signed 8-bit integer
type Int8 = I.Int8

-- uint8          1 byte Unsigned 8-bit integer
type UInt8 = W.Word8

-- uint16         2 bytes Unsigned 16-bit integer
newtype UInt16 = UInt16 { fromUInt16 :: W.Word16 }
    deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Ix, FiniteBits, Bits, Storable, PrintfArg)

instance Binary UInt16 where
    get = UInt16 <$> getWord16le
    put = putWord16le . fromUInt16

-- uint32         4 bytes Unsigned 32-bit integer
newtype UInt32 = UInt32 { fromUInt32 :: W.Word32 }
    deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show, Ix, FiniteBits, Bits, Storable, PrintfArg)

instance Binary UInt32 where
    get = UInt32 <$> getWord32le
    put = putWord32le . fromUInt32

-- uint8array     byte array, first byte is array size
newtype UInt8Array = UInt8Array { fromUInt8Array :: BSS.ByteString } deriving (Eq, Ord, IsString)

toUInt8Array :: BSS.ByteString -> UInt8Array
toUInt8Array s = UInt8Array s

instance Show UInt8Array where
    show = bsShowHex . fromUInt8Array

instance Binary UInt8Array where
    put UInt8Array{..} = do
        putWord8 $ fromIntegral $ BSS.length fromUInt8Array
        putByteString fromUInt8Array

    get = do
        l <- getWord8
        bs <- getByteString (fromIntegral l)
        return $ UInt8Array bs

-- bd_addr        Bluetooth address in little endian format
newtype BdAddr = BdAddr { fromBdAddr :: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8) }
    deriving Eq

instance Show BdAddr where
    show (BdAddr (_5, _4, _3, _2, _1, _0)) = printf "%02x:%02x:%02x:%02x:%02x:%02x" _0 _1 _2 _3 _4 _5

instance Binary BdAddr where
    put BdAddr{..} = put fromBdAddr
    get = get >>= return . BdAddr

data BgMessageType = BgMsgCR | BgMsgEvent deriving (Eq, Show, Bounded, Enum)

data BgTecnologyType = BgBlue | BgWifi deriving (Eq, Show, Bounded, Enum)

data BgCommandClass
    = BgClsSystem
    | BgClsPersistentStore
    | BgClsAttributeDatabase
    | BgClsConnection
    | BgClsAttributeClient
    | BgClsSecurityManager
    | BgClsGenericAccessProfile
    | BgClsHardware
    | BgClsTest
    | BgClsDfu
    deriving (Eq, Show, Bounded, Enum)

data BgPacketHeader = BgPacketHeader
    { bghMessageType    :: BgMessageType
    , bghTechnologyType :: BgTecnologyType
    , bghLength         :: UInt16 -- Only 11 bits actually
    , bghCommandClass   :: BgCommandClass
    , bghCommandId      :: UInt8
    } deriving (Eq, Show)

enumFromIntegral :: forall a b. (Integral a, Bounded b, Enum b) => a -> Get b
enumFromIntegral i = do
    let mi = fromEnum (minBound :: b)
    let ma = fromEnum (maxBound :: b)
    let ii = fromIntegral i
    if ii >= mi && ii <= ma
        then return $ toEnum ii
        else fail $ "Value out of bounds: " ++ show mi ++ " <= " ++ show ii ++ " <= " ++ show ma

instance Binary BgPacketHeader where
    put BgPacketHeader{..} = do
        putWord8
            $   fromIntegral (fromEnum bghMessageType `shift` 7)
            .|. fromIntegral (fromEnum bghTechnologyType `shift` 3)
            .|. fromIntegral ((bghLength .&. 0x0700) `shift` (-8))
        putWord8 $ fromIntegral $ bghLength .&. 0x00ff
        putWord8 $ fromIntegral $ fromEnum bghCommandClass
        putWord8 $ bghCommandId

    get = do
        oct0  <- getWord8
        lLow  <- getWord8
        clsId <- getWord8
        cmdId <- getWord8
    
        let lHigh = oct0 .&. 0x07

        bghMessageType    <- enumFromIntegral $ oct0 `shift` (-7)
        bghTechnologyType <- enumFromIntegral $ (oct0 `shift` (-3)) .&. 0x0f
        let bghLength     =  (fromIntegral lHigh `shift` 8) + (fromIntegral lLow) :: UInt16
        bghCommandClass   <- enumFromIntegral clsId
        let bghCommandId  =  cmdId

        return $ BgPacketHeader{..}

bgHeaderMatches :: BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> BgPacketHeader -> Bool
bgHeaderMatches mt tt cc cid BgPacketHeader{..}
    =  mt  == bghMessageType
    && tt  == bghTechnologyType
    && cc  == bghCommandClass
    && cid == bghCommandId

newtype BgPayload = BgPayload { fromBgPayload :: BSS.ByteString } deriving Eq

toBgPayload :: BSS.ByteString -> BgPayload
toBgPayload = BgPayload

instance Show BgPayload where
    show = bsShowHex . fromBgPayload

data BgPacket = BgPacket
    { bgpHeader  :: BgPacketHeader
    , bgpPayload :: BgPayload
    } deriving (Eq, Show)

instance Binary BgPacket where
    put BgPacket{..} = do
        put bgpHeader
        putByteString $ fromBgPayload bgpPayload

    get = do
        bgpHeader@BgPacketHeader{..} <- get
        bgpPayload <- toBgPayload <$> getByteString (fromIntegral bghLength)
        return BgPacket{..}

class HasSerialPort env where
    getSerialPort :: env -> SerialPort

askSerialPort :: (MonadReader env m, HasSerialPort env) => m SerialPort
askSerialPort = getSerialPort <$> ask

class HasBGChan env where
    getBGChan :: env -> TChan BgPacket
    updateBGChan :: TChan BgPacket -> env -> env

askBGChan :: (MonadReader env m, HasBGChan env) => m (TChan BgPacket)
askBGChan = getBGChan <$> ask

askDupBGChan :: (MonadIO m, MonadReader env m, HasBGChan env) => m (TChan BgPacket)
askDupBGChan = do
    chan <- getBGChan <$> ask
    liftIO $ atomically $ dupTChan chan

askCloneBGChan :: (MonadIO m, MonadReader env m, HasBGChan env) => m (TChan BgPacket)
askCloneBGChan = do
    chan <- getBGChan <$> ask
    liftIO $ atomically $ cloneTChan chan
    
packetBlock :: (MonadIO m, MonadReader env m, HasBGChan env) => m a -> m a
packetBlock act = do
    newChan <- askDupBGChan
    local (updateBGChan newChan) act

packetBlock_ :: (MonadIO m, MonadReader env m, HasBGChan env) => m a -> m ()
packetBlock_ act = packetBlock act >> return ()

packetBlock' :: (MonadIO m, MonadReader env m, HasBGChan env) => m a -> m a
packetBlock' act = do
    newChan <- askCloneBGChan
    local (updateBGChan newChan) act

packetBlock'_ :: (MonadIO m, MonadReader env m, HasBGChan env) => m a -> m ()
packetBlock'_ act = packetBlock' act >> return ()

class HasDebug env where
    getDebug :: env -> Bool

askDebug :: (MonadReader env m, HasDebug env) => m (Bool)
askDebug = getDebug <$> ask

bsShowHex :: BSS.ByteString -> String
bsShowHex = concatMap (\n -> ' ' : showHex n "") . BSS.unpack

data RebootMode
    -- Reboot into application
    = RebootNormal
    -- Reboot into DFU mode
    | RebootDfu
    deriving (Eq, Show, Enum)

instance Binary RebootMode where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data AttributeValueType
    -- 0: Value was read
    = AVTRead
    -- 1: Value was notified
    | AVTNotify
    -- 2: Value was indicated
    | AVTIndicate
    -- 3: Value was read
    | AVTReadByType
    -- 4: Value was part of a long attribute
    | AVTReadBlob
    -- 5: Value was indicated and the remote device is
    -- waiting for a confirmation
    | AVTIndicateRsqReq
    deriving (Eq, Show, Enum)

instance Binary AttributeValueType where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data AttributeChangeReason
    -- 0: Value was written by remote device using write request
    = ACRWriteRequest
    -- 1: Value was written by remote device using write command
    | ACRWriteCommand
    -- 2: Local attribute value was written by the
    -- remote device, but the Bluetooth Smart
    -- stack is waiting for the write to be
    -- confirmed by the application.
    -- User Write Response command should
    -- be used to send the confirmation.
    | ACRWriteRequestUser
    deriving (Eq, Show, Enum)

instance Binary AttributeChangeReason where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8
 
-- Attribute status flags

-- Notifications are enabled
fASNotify :: UInt8
fASNotify = 0x01

-- Indications are enabled
fASIndicate :: UInt8
fASIndicate = 0x02

-- Connection status flags

-- This status flag tells the connection exists to a remote device.
fCConnected :: UInt8
fCConnected = 0x01

-- This flag tells the connection is encrypted.
fCEncrypted :: UInt8
fCEncrypted = 0x02

-- Connection completed flag, which is used to tell a new connection
-- has been created.
fCCompleted :: UInt8
fCCompleted = 0x04

-- This flag tells that connection parameters have changed and. It is
-- set when connection parameters have changed due to a link layer
-- operation.
fCParametersChanged :: UInt8
fCParametersChanged = 0x08

-- 0x01 GAP_AD_FLAG_LIMITED_DISCOVERABLE Limited discoverability
fADLimitedDiscoverable :: UInt8
fADLimitedDiscoverable = 0x01

-- 0x02 GAP_AD_FLAG_GENERAL_DISCOVERABLE General discoverability
fADGeneralDiscoverable :: UInt8
fADGeneralDiscoverable = 0x02

-- 0x04 GAP_AD_FLAG_BREDR_NOT_SUPPORTED BR/EDR not supported
fADBREDRNotSupported :: UInt8
fADBREDRNotSupported = 0x04

-- 0x10 GAP_AD_FLAG_SIMULTANEOUS_LEBREDR_CTRL BR/EDR controller
fADSimultaneousLEBREDRCtrl :: UInt8
fADSimultaneousLEBREDRCtrl = 0x10

-- 0x20 GAP_AD_FLAG_SIMULTANEOUS_LEBREDR_HOST BE/EDR host
fADSimultaneousLEBREDRHost :: UInt8
fADSimultaneousLEBREDRHost = 0x20

-- 0x1f GAP_AD_FLAG_MASK -
fADMask :: UInt8
fADMask = 0x1f

data GapAdvType
    = GATNone
    | GATFlags
    | GATServices16bitMore
    | GATServices16bitAll
    | GATServices32bitMore
    | GATServices32bitAll
    | GATServices128bitMore
    | GATServices128bitAll
    | GATLocalnameShort
    | GATLocalnameComplete
    | GATTxPower
    deriving (Eq, Show, Enum)

instance Binary GapAdvType where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data GapAdvPolicy
    -- Respond to scan requests from any master, allow connection
    -- from any master (default)
    = GAPAll
    -- Respond to scan requests from whitelist only, allow connection
    -- from any
    | GAPWhitelistScan
    -- Respond to scan requests from any, allow connection from
    -- whitelist only
    | GAPWhitelistConnect
    -- Respond to scan requests from whitelist only, allow connection
    -- from whitelist only
    | GAPWhitelistAll
    deriving (Eq, Show, Enum)

instance Binary GapAdvPolicy where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8


data GapAddressType
    = GATPublic
    | GATRandom
    deriving (Eq, Show, Enum)

instance Binary GapAddressType where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data GapConnectableMode
    -- Not connectable
    = GCMNonConnectable
    -- Directed Connectable
    | GCMDirectedConnectable
    -- Undirected connectable
    | GCMUndirectedConnectable
    -- Same as non-connectable, but also supports ADV_SCAN_IND
    -- packets. Device accepts scan requests (active scanning) but is
    -- not connectable.
    | GCMScannableNonConnectable
    deriving (Eq, Show, Enum)

instance Binary GapConnectableMode where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8
    

data GapDiscoverableMode
    -- Non-discoverable mode: the LE Limited Discoverable Mode and the
    -- LE General Discoverable Mode bits are NOT set in the Flags AD
    -- type. A master can still connect to the advertising slave in this mode.
    = GDMNonDiscoverable
    -- 1 gap_limited_discoverable Discoverable using limited scanning mode: the advertisement
    -- packets will carry the LE Limited Discoverable Mode bit set in the
    -- Flags AD type.
    | GDMLimitedDiscoverable
    -- 2 gap_general_discoverable Discoverable using general scanning mode: the advertisement
    -- packets will carry the LE General Discoverable Mode bit set in the
    -- Flags AD type.
    | GDMGeneralDiscoverable
    -- 3 gap_broadcast Same as gap_non_discoverable above.
    | GDMBroadcast
    -- 4 gap_user_data In this advertisement the advertisement and scan response data
    -- defined by user will be used. The user is responsible of building the
    -- advertisement data so that it also contains the appropriate desired
    -- Flags AD type.
    | GDMUserData
    -- 0x80 gap_enhanced_broadcasting When turning the most highest bit on in GAP discoverable mode, the
    -- remote devices that send scan request packets to the advertiser are
    -- reported back to the application through Scan Response event.
    -- This is so called Enhanced Broadcasting mode.
    | GDMEnhancedBroadcasting
    deriving (Eq, Show, Enum)

instance Binary GapDiscoverableMode where
    put m = do
        putWord8$ case m of
            GDMEnhancedBroadcasting -> 0x80
            _ -> fromIntegral $ fromEnum m
    get = do
        x <- getWord8
        return $ case x of
            5 -> GDMEnhancedBroadcasting
            _ -> toEnum $ fromIntegral x

data GapDiscoverMode
    -- 0: Discover only limited discoverable devices, that is, Slaves which have the
    -- LE Limited Discoverable Mode bit set in the Flags AD type of their
    -- advertisement packets.
    = GapDiscoverLimited
    -- Discover limited and generic discoverable devices, that is, Slaves which
    -- have the LE Limited Discoverable Mode or the LE General Discoverable
    -- Mode bit set in the Flags AD type of their advertisement packets.
    | GapDiscoverGeneric
    -- Discover all devices regardless of the Flags AD typ
    | GapDiscoverOvservation
    deriving (Eq, Show, Enum)

instance Binary GapDiscoverMode where
    put m = do
        putWord16le $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord16le


-- GAP Scan header flags
data GSPScanHeaderFlag
    -- Connectable undirected advertising event
    = GSHFAdvInd
    -- Connectable directed advertising event
    | GSHFAdvDirectInd
    -- Non-connectable undirected advertising event
    | GSHFAdvNonConnInd
    -- Scanner wants information from Advertiser
    | GSHFScanReq
    -- Advertiser gives more information to Scanner
    | GSHFScanRsp
    -- Initiator wants to connect to Advertiser
    | GSHFConnectReq
    -- Non-connectable undirected advertising event
    | GSHFAdvDiscoverInd
    deriving (Eq, Show, Enum)

instance Binary GSPScanHeaderFlag where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data GapScanPolicy
    -- All advertisement Packets (default)
    = GSPAll
    -- Ignore advertisement packets from remote slaves not in the running
    -- whitelist
    | GSPWhitelist
    deriving (Eq, Show, Enum)

instance Binary GapScanPolicy where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

-- SM Bonding Key flags

-- LTK saved in master
fBKLTK :: UInt8
fBKLTK = 0x01

-- Public Address
fBKAddrPublic :: UInt8
fBKAddrPublic = 0x02

-- Static Address
fBKAddrStatic :: UInt8
fBKAddrStatic = 0x04

-- Identity resolving key for resolvable private addresses
fBKIRK :: UInt8
fBKIRK = 0x08

-- EDIV+RAND received from slave
fBKEDIVRAND :: UInt8
fBKEDIVRAND = 0x10

-- Connection signature resolving key
fBKCSRK :: UInt8
fBKCSRK = 0x20

-- EDIV+RAND sent to master
fBKMasterId :: UInt8
fBKMasterId = 0x40

data SMIOCapabilities
    -- Display Only
    = SICDisplayOnly
    -- Display with Yes/No-buttons
    | SICDisplayYesNo
    -- Keyboard Only
    | SICKeyboardOnly
    -- No Input and No Output
    | SICNoIO
    -- Display with Keyboard
    | SICKeyboardDisplay
    deriving (Eq, Enum, Show)

instance Binary SMIOCapabilities where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data SystemEndpoint
    -- Command Parser
     = SECommandParser
    -- Radio Test
    | SETest
    -- BGScript (not used)
    | SEScript
    -- USB Interface
    | SEUSB
    -- USART 0
    | SEUART0
    -- USART 1
    | SEUART1
    deriving (Eq, Show, Enum)

instance Binary SystemEndpoint where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

-- Operation result
data BGResult
    = BGRSuccess
    -- Invalid Parameter (0x0180)
    -- Command contained invalid parameter
    | BGRInvalidParameter

    -- Device in Wrong State (0x0181)
    -- Device is in wrong state to receive command
    | BGRWrongState

    -- Out Of Memory (0x0182)
    -- Device has run out of memory
    | BGROutOfMemory

    -- Feature Not Implemented (0x0183)
    -- Feature is not implemented
    | BGRNotImplemented

    -- Command Not Recognized (0x0184)
    -- Command was not recognized
    | BGRNotRecognized

    -- Timeout (0x0185)
    -- Command or Procedure failed due to timeout
    | BGRTimeout

    -- Not Connected (0x0186)
    -- Connection handle passed is to command is not a valid handle
    | BGRNotConnected

    -- Flow (0x0187)
    -- Command would cause either underflow or overflow error
    | BGRFlow

    -- User Attribute (0x0188)
    -- User attribute was accessed through API which is not supported
    | BGRUserAttribute

    -- Invalid License Key (0x0189)
    -- No valid license key found
    | BGRInvalidLicenseKey

    -- Command Too Long (0x018A)
    -- Command maximum length exceeded
    | BGRCommandTooLong

    -- Out of Bonds (0x018B)
    -- Bonding procedure can't be started because device has no space left for bond.
    | BGROutOfBonds

    -- Script Overflow (0x018C)
    -- Module was reset due to script stack overflow.
    -- In BLE BGScript there is a script stack overflow detection mechanism. This solution resets module
    -- when script stack overflow is detected. After next boot script failure event with specific error code is
    -- generated right after system boot event.
    -- This feature works only with BLE SDK version 1.7.0 or newer that support script stack overflow
    -- detection mechanism. For this feature to work correctly update of bootloader is needed.
    | BGRScriptOverflow

    -- Authentication Failure (0x0205)
    -- Pairing or authentication failed due to incorrect results in the pairing or authentication procedure. This could be
    -- due to an incorrect PIN or Link Key
    | BGRAuthenticationFailure

    -- Pin or Key Missing (0x0206)
    -- Pairing failed because of missing PIN, or authentication failed because of missing Key.
    -- Silicon Labs
    | BGRPinOrKeyMissing

    -- Memory Capacity Exceeded (0x0207)
    -- Controller is out of memory.
    | BGRMemoryCapacityExceeded

    -- Connection Timeout (0x0208)
    -- Link supervision timeout has expired.
    | BGRConnectionTimeout

    -- Connection Limit Exceeded (0x0209)
    -- Controller is at limit of connections it can support.
    | BGRConnectionLimitExceeded

    -- Command Disallowed (0x020C)
    -- Command requested cannot be executed because the Controller is in a state where it cannot process this
    -- command at this time.
    | BGRCommandDisallowed

    -- Invalid Command Parameters (0x0212)
    -- Command contained invalid parameters.
    | BGRInvalidCommandParameters

    -- Remote User Terminated Connection (0x0213)
    -- User on the remote device terminated the connection.
    | BGRRemoteUserTerminatedConnection

    -- Connection Terminated by Local Host (0x0216)
    -- Local device terminated the connection.
    | BGRConnectionTErminagedByLocalHost

    -- LL Response Timeout (0x0222)
    -- Connection terminated due to link-layer procedure timeout.
    | BGRLLResponseTimeout

    -- LL Instant Passed (0x0228)
    -- Received link-layer control packet where instant was in the past.
    | BGRLLInstantPassed

    -- Controller Busy (0x023A)
    -- Operation was rejected because the controller is busy and unable to process the request.
    | BGRControllerBusy

    -- Unacceptable Connection Interval (0x023B)
    -- The Unacceptable Connection Interval error code indicates that the remote device terminated the connection
    -- because of an unacceptable connection interval.
    | BGRUnacceptableConnectionInterval

    -- Directed Advertising Timeout (0x023C)
    -- Directed advertising completed without a connection being created.
    | BGRDirectedAdvertisingTimeout

    -- MIC Failure (0x023D)
    -- Connection was terminated because the Message Integrity Check (MIC) failed on a received packet.
    | BGRMICFailure

    -- Connection Failed to be Established (0x023E)
    -- LL initiated a connection but the connection has failed to be established. Controller did not receive any packets
    -- from remote end.
    -- More in detail, an attempt to open a connection is made by the master by sending only one CONNECT_REQ ,
    -- after which the master immediately transitions to connected state (BT4.1 Vol 6 Part B 4.4.4). If the advertiser for
    -- any reason (like interference) does not catch the packet it will just continue advertising, while the master
    -- remains in a fast termination mode, where it will only send 6 packets before failing, independent of supervision
    -- timeout (in fact, a master starts using normal supervision timeout only after it has received at least one packet
    -- from slave.) If the master does not receive anything by the time its 6 packets are sent, connection establishment
    -- will be considered failed and this error will be reported to the host or to the BGScript. In a busy environment it is
    -- normal to see roughly 1-2% error rate when opening connections.
    | BGRConnectionFailedToBeEstablised

    -- Passkey Entry Failed (0x0301)
    -- The user input of passkey failed, for example, the user cancelled the operation
    | BGRPasskeyEntryFailed

    -- OOB Data is not available (0x0302)
    -- Out of Band data is not available for authentication
    | BGROOBDataIsNotAvailable

    -- Authentication Requirements (0x0303)
    -- The pairing procedure cannot be performed as authentication requirements cannot be met due to IO capabilities
    -- of one or both devices
    | BGRAuthenticationRequirements

    -- Confirm Value Failed (0x0304)
    -- The confirm value does not match the calculated compare value
    | BGRConfirmValueFailed

    -- Pairing Not Supported (0x0305)
    -- Pairing is not supported by the device
    | BGRPairingNotSupported

    -- Encryption Key Size (0x0306)
    -- The resultant encryption key size is insufficient for the security requirements of this device
    | BGREncryptionKeySize

    -- Command Not Supported (0x0307)
    -- The SMP command received is not supported on this device
    | BGRCommandNotSupported

    -- Unspecified Reason (0x0308)
    -- Pairing failed due to an unspecified reason
    | BGRUnspecifiedReason

    -- Repeated Attempts (0x0309)
    -- Pairing or authentication procedure is disallowed because too little time has elapsed since last pairing request
    -- or security request
    | BGRRepeatedAttempts

    -- Invalid Parameters (0x030A)
    -- The Invalid Parameters error code indicates: the command length is invalid or a parameter is outside of the
    -- specified range.
    | BGRInvalidParameters

    -- Invalid Handle (0x0401)
    -- The attribute handle given was not valid on this server
    | BGRInvalidHandle

    -- Read Not Permitted (0x0402)
    -- The attribute cannot be read
    | BGRReadNotPermitted

    -- Write Not Permitted (0x0403)
    -- The attribute cannot be written
    | BGRWriteNotPermitted

    -- Invalid PDU (0x0404)
    -- The attribute PDU was invalid
    | BGRInvalidPDU

    -- Insufficient Authentication (0x0405)
    -- The attribute requires authentication before it can be read or written.
    | BGRInsufficientAuthentication

    -- Request Not Supported (0x0406)
    -- Attribute Server does not support the request received from the client.
    | BGRRequestNotSupported

    -- Invalid Offset (0x0407)
    -- Offset specified was past the end of the attribute
    | BGRInvalidOffset

    -- Insufficient Authorization (0x0408)
    -- The attribute requires authorization before it can be read or written.
    | BGRInsufficientAuthorization

    -- Prepare Queue Full (0x0409)
    -- Too many prepare writes have been queueud
    | BGRPrepareQueueFull

    -- Attribute Not Found (0x040A)
    -- No attribute found within the given attribute handle range.
    | BGRAttributeNotFound

    -- Attribute Not Long (0x040B)
    -- The attribute cannot be read or written using the Read Blob Request
    | BGRAttributeNotLong

    -- Insufficient Encryption Key Size (0x040C)
    -- The Encryption Key Size used for encrypting this link is insufficient.
    | BGRInsufficientEncryptionKeySize

    -- Invalid Attribute Value Length (0x040D)
    -- The attribute value length is invalid for the operation
    | BGRInvalidAttributeValueLength

    -- Unlikely Error (0x040E)
    -- The attribute request that was requested has encountered an error that was unlikely, and therefore could not be
    -- completed as requested.
    | BGRUnlikelyError

    -- Insufficient Encryption (0x040F)
    -- The attribute requires encryption before it can be read or written.
    | BGRInsufficientEncryption

    -- Unsupported Group Type (0x0410)
    -- The attribute type is not a supported grouping attribute as defined by a higher layer specification.
    | BGRUnsupportedGroupType

    -- Insufficient Resources (0x0411)
    -- Insufficient Resources to complete the request
    | BGRInsufficientResources

    -- Application Error Codes (0x0480)
    -- Application error code defined by a higher layer specification.
    -- The error code range 0x80-0x9F is reserved for application level errors.
    | BGRApplicationErrorCode UInt8

    -- And error code unknown by this library
    | BGRUnknown UInt16
    deriving (Eq, Show)

instance Binary BGResult where
    put m = do
        putWord16le $ case m of
            BGRSuccess                         -> 0x0000
            BGRInvalidParameter                -> 0x0180
            BGRWrongState                      -> 0x0181
            BGROutOfMemory                     -> 0x0182
            BGRNotImplemented                  -> 0x0183
            BGRNotRecognized                   -> 0x0184
            BGRTimeout                         -> 0x0185
            BGRNotConnected                    -> 0x0186
            BGRFlow                            -> 0x0187
            BGRUserAttribute                   -> 0x0188
            BGRInvalidLicenseKey               -> 0x0189
            BGRCommandTooLong                  -> 0x018A
            BGROutOfBonds                      -> 0x018B
            BGRScriptOverflow                  -> 0x018C
            BGRAuthenticationFailure           -> 0x0205
            BGRPinOrKeyMissing                 -> 0x0206
            BGRMemoryCapacityExceeded          -> 0x0207
            BGRConnectionTimeout               -> 0x0208
            BGRConnectionLimitExceeded         -> 0x0209
            BGRCommandDisallowed               -> 0x020C
            BGRInvalidCommandParameters        -> 0x0212
            BGRRemoteUserTerminatedConnection  -> 0x0213
            BGRConnectionTErminagedByLocalHost -> 0x0216
            BGRLLResponseTimeout               -> 0x0222
            BGRLLInstantPassed                 -> 0x0228
            BGRControllerBusy                  -> 0x023A
            BGRUnacceptableConnectionInterval  -> 0x023B
            BGRDirectedAdvertisingTimeout      -> 0x023C
            BGRMICFailure                      -> 0x023D
            BGRConnectionFailedToBeEstablised  -> 0x023E
            BGRPasskeyEntryFailed              -> 0x0301
            BGROOBDataIsNotAvailable           -> 0x0302
            BGRAuthenticationRequirements      -> 0x0303
            BGRConfirmValueFailed              -> 0x0304
            BGRPairingNotSupported             -> 0x0305
            BGREncryptionKeySize               -> 0x0306
            BGRCommandNotSupported             -> 0x0307
            BGRUnspecifiedReason               -> 0x0308
            BGRRepeatedAttempts                -> 0x0309
            BGRInvalidParameters               -> 0x030A
            BGRInvalidHandle                   -> 0x0401
            BGRReadNotPermitted                -> 0x0402
            BGRWriteNotPermitted               -> 0x0403
            BGRInvalidPDU                      -> 0x0404
            BGRInsufficientAuthentication      -> 0x0405
            BGRRequestNotSupported             -> 0x0406
            BGRInvalidOffset                   -> 0x0407
            BGRInsufficientAuthorization       -> 0x0408
            BGRPrepareQueueFull                -> 0x0409
            BGRAttributeNotFound               -> 0x040A
            BGRAttributeNotLong                -> 0x040B
            BGRInsufficientEncryptionKeySize   -> 0x040C
            BGRInvalidAttributeValueLength     -> 0x040D
            BGRUnlikelyError                   -> 0x040E
            BGRInsufficientEncryption          -> 0x040F
            BGRUnsupportedGroupType            -> 0x0410
            BGRInsufficientResources           -> 0x0411
            BGRApplicationErrorCode errC       -> (fromIntegral errC .&. 0x001f) .|. 0x0480
            BGRUnknown errC                    -> fromIntegral errC

    get = do
        errC <- getWord16le
        return $ case errC of
            0x0000 -> BGRSuccess
            0x0180 -> BGRInvalidParameter
            0x0181 -> BGRWrongState
            0x0182 -> BGROutOfMemory
            0x0183 -> BGRNotImplemented
            0x0184 -> BGRNotRecognized
            0x0185 -> BGRTimeout
            0x0186 -> BGRNotConnected
            0x0187 -> BGRFlow
            0x0188 -> BGRUserAttribute
            0x0189 -> BGRInvalidLicenseKey
            0x018A -> BGRCommandTooLong
            0x018B -> BGROutOfBonds
            0x018C -> BGRScriptOverflow
            0x0205 -> BGRAuthenticationFailure
            0x0206 -> BGRPinOrKeyMissing
            0x0207 -> BGRMemoryCapacityExceeded
            0x0208 -> BGRConnectionTimeout
            0x0209 -> BGRConnectionLimitExceeded
            0x020C -> BGRCommandDisallowed
            0x0212 -> BGRInvalidCommandParameters
            0x0213 -> BGRRemoteUserTerminatedConnection
            0x0216 -> BGRConnectionTErminagedByLocalHost
            0x0222 -> BGRLLResponseTimeout
            0x0228 -> BGRLLInstantPassed
            0x023A -> BGRControllerBusy
            0x023B -> BGRUnacceptableConnectionInterval
            0x023C -> BGRDirectedAdvertisingTimeout
            0x023D -> BGRMICFailure
            0x023E -> BGRConnectionFailedToBeEstablised
            0x0301 -> BGRPasskeyEntryFailed
            0x0302 -> BGROOBDataIsNotAvailable
            0x0303 -> BGRAuthenticationRequirements
            0x0304 -> BGRConfirmValueFailed
            0x0305 -> BGRPairingNotSupported
            0x0306 -> BGREncryptionKeySize
            0x0307 -> BGRCommandNotSupported
            0x0308 -> BGRUnspecifiedReason
            0x0309 -> BGRRepeatedAttempts
            0x030A -> BGRInvalidParameters
            0x0401 -> BGRInvalidHandle
            0x0402 -> BGRReadNotPermitted
            0x0403 -> BGRWriteNotPermitted
            0x0404 -> BGRInvalidPDU
            0x0405 -> BGRInsufficientAuthentication
            0x0406 -> BGRRequestNotSupported
            0x0407 -> BGRInvalidOffset
            0x0408 -> BGRInsufficientAuthorization
            0x0409 -> BGRPrepareQueueFull
            0x040A -> BGRAttributeNotFound
            0x040B -> BGRAttributeNotLong
            0x040C -> BGRInsufficientEncryptionKeySize
            0x040D -> BGRInvalidAttributeValueLength
            0x040E -> BGRUnlikelyError
            0x040F -> BGRInsufficientEncryption
            0x0410 -> BGRUnsupportedGroupType
            0x0411 -> BGRInsufficientResources
            _      ->
                if errC >= 0x0480 && errC <= 0x049f
                    then BGRApplicationErrorCode $ fromIntegral (errC .&. 0x1f)
                    else BGRUnknown $ fromIntegral errC
