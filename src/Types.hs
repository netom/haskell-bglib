{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
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
    , BgPacket(..)
    , HasSerialPort(..)
    , askSerialPort
    , HasBGChan(..)
    , askBGChan
    , askDupBGChan
    , prettyShowBS
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
    , ADType(..)
    , AdPolicy(..)
    , BluetoothAddressType(..)
    , GAPConnectableMode(..)
    , GAPDiscoverableMode(..)
    , GapDiscoverMode(..)
    , GSPScanHeaderFlag(..)
    , ScanPolicy(..)
    , fBKLTK
    , fBKAddrPublic
    , fBKAddrStatic
    , fBKIRK
    , fBKEDIVRAND
    , fBKCSRK
    , fBKMasterId
    , SMIOCapabilities(..)
    , SystemEndpoint(..)
    , BGAPIError(..)
    , BluetoothError(..)
    ) where

import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString as BSS
import qualified Data.Int as I
import qualified Data.Word as W
import           Numeric
import           System.Hardware.Serialport
import           Text.Printf

-- int8           1 byte Signed 8-bit integer
type Int8 = I.Int8

-- uint8          1 byte Unsigned 8-bit integer
type UInt8 = W.Word8

-- uint16         2 bytes Unsigned 16-bit integer
type UInt16 = W.Word16

-- uint32         4 bytes Unsigned 32-bit integer
type UInt32 = W.Word32

-- uint8array     byte array, first byte is array size
newtype UInt8Array = UInt8Array { fromUInt8Array :: BSS.ByteString }

toUInt8Array :: BSS.ByteString -> UInt8Array
toUInt8Array s = UInt8Array s

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

instance Show BdAddr where
    show (BdAddr (_5, _4, _3, _2, _1, _0)) = printf "%02x:%02x:%02x:%02x:%02x:%02x" _0 _1 _2 _3 _4 _5

instance Binary BdAddr where
    put BdAddr{..} = put fromBdAddr
    get = get >>= return . BdAddr

data BgMessageType = BgMsgCR | BgMsgEvent deriving (Eq, Show, Enum)

data BgTecnologyType = BgBlue | BgWifi deriving (Eq, Show, Enum)

data BgCommandClass
    = BgClsSystem
    | BgClsPersistentStore
    | BgClsAttributeDatabase
    | BgClsConnection
    | BgClsAttributeClient
    | BgClsSecurityManager
    | BgClsGenericAccessProfile
    | BgClsHardware deriving (Eq, Show, Enum)

data BgPacketHeader = BgPacketHeader
    { bghMessageType    :: BgMessageType
    , bghTechnologyType :: BgTecnologyType
    , bghLength         :: UInt16 -- Only 11 bits actually
    , bghCommandClass   :: BgCommandClass
    , bghCommandId      :: UInt8
    } deriving Show

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

        let bghMessageType    = toEnum $ fromIntegral $ oct0 `shift` (-7)
        let bghTechnologyType = toEnum $ fromIntegral $ (oct0 `shift` (-3)) .&. 0x0f
        let bghLength         = (fromIntegral lHigh `shift` 8) + (fromIntegral lLow) :: UInt16
        let bghCommandClass   = toEnum $ fromIntegral clsId
        let bghCommandId      = cmdId

        return $ BgPacketHeader{..}

bgHeaderMatches :: BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> BgPacketHeader -> Bool
bgHeaderMatches mt tt cc cid BgPacketHeader{..}
    =  mt  == bghMessageType
    && tt  == bghTechnologyType
    && cc  == bghCommandClass
    && cid == bghCommandId

data BgPacket = BgPacket
    { bgpHeader  :: BgPacketHeader
    , bgpPayload :: BSS.ByteString
    } deriving Show

instance Binary BgPacket where
    put BgPacket{..} = do
        put bgpHeader
        putByteString bgpPayload

    get = do
        bgpHeader@BgPacketHeader{..} <- get
        bgpPayload <- getByteString $ fromIntegral bghLength
        return BgPacket{..}

class HasSerialPort env where
    getSerialPort :: env -> SerialPort

askSerialPort :: (MonadReader env m, HasSerialPort env) => m SerialPort
askSerialPort = getSerialPort <$> ask

class HasBGChan env where
    getBGChan :: env -> TChan BgPacket

askBGChan :: (MonadReader env m, HasBGChan env) => m (TChan BgPacket)
askBGChan = getBGChan <$> ask
    
askDupBGChan :: (MonadIO m, MonadReader env m, HasBGChan env) => m (TChan BgPacket)
askDupBGChan = do
    chan <- getBGChan <$> ask
    liftIO $ atomically $ dupTChan chan

prettyShowBS :: BSS.ByteString -> String
prettyShowBS = concatMap (\n -> ' ' : showHex n "") . BSS.unpack

data RebootMode
    -- Reboot into application
    = RebootNormal
    -- Reboot into DFU mode
    | RebootDfu
    deriving (Show, Enum)

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
    deriving (Show, Enum)

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
    deriving (Show, Enum)

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

data ADType
    = ADTNone
    | ADTFlags
    | ADTServices16bitMore
    | ADTServices16bitAll
    | ADTServices32bitMore
    | ADTServices32bitAll
    | ADTServices128bitMore
    | ADTServices128bitAll
    | ADTLocalnameShort
    | ADTLocalnameComplete
    | ADTTxPower
    deriving (Show, Enum)

instance Binary ADType where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data AdPolicy
    -- Respond to scan requests from any master, allow connection
    -- from any master (default)
    = APAll
    -- Respond to scan requests from whitelist only, allow connection
    -- from any
    | APWhitelistScan
    -- Respond to scan requests from any, allow connection from
    -- whitelist only
    | APWhitelistConnect
    -- Respond to scan requests from whitelist only, allow connection
    -- from whitelist only
    | APWhitelistAll
    deriving (Show, Enum)

instance Binary AdPolicy where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8


data BluetoothAddressType
    = BATPublic
    | BATRandom
    deriving (Show, Enum)

instance Binary BluetoothAddressType where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data GAPConnectableMode
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
    deriving (Show, Enum)

instance Binary GAPConnectableMode where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8
    

data GAPDiscoverableMode
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
    deriving (Show, Enum)

instance Binary GAPDiscoverableMode where
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
    deriving (Show, Enum)

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
    deriving (Show, Enum)

instance Binary GSPScanHeaderFlag where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

data ScanPolicy
    -- All advertisement Packets (default)
    = SPAll
    -- Ignore advertisement packets from remote slaves not in the running
    -- whitelist
    | SPWhitelist
    deriving (Show, Enum)

instance Binary ScanPolicy where
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
    deriving (Enum, Show)

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
    deriving (Show, Enum)

instance Binary SystemEndpoint where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

-- TODO
-- BGAPI Errors
data BGAPIError
    = BGErrSuccess
    -- Invalid Parameter (0x0180)
    -- Command contained invalid parameter
    | BGErrInvalidParameter

    -- Device in Wrong State (0x0181)
    -- Device is in wrong state to receive command
    | BGErrWrongState

    -- Out Of Memory (0x0182)
    -- Device has run out of memory
    | BGErrOutOfMemory

    -- Feature Not Implemented (0x0183)
    -- Feature is not implemented
    | BGErrNotImplemented

    -- Command Not Recognized (0x0184)
    -- Command was not recognized
    | BGErrNotRecognized

    -- Timeout (0x0185)
    -- Command or Procedure failed due to timeout
    | BGErrTimeout

    -- Not Connected (0x0186)
    -- Connection handle passed is to command is not a valid handle
    | BGErrNotConnected

    -- Flow (0x0187)
    -- Command would cause either underflow or overflow error
    | BGErrFlow

    -- User Attribute (0x0188)
    -- User attribute was accessed through API which is not supported
    | BGErrUserAttribute

    -- Invalid License Key (0x0189)
    -- No valid license key found
    | BGErrInvalidLicenseKey

    -- Command Too Long (0x018A)
    -- Command maximum length exceeded
    | BGErrCommandTooLong

    -- Out of Bonds (0x018B)
    -- Bonding procedure can't be started because device has no space left for bond.
    | BGErrOutOfBonds

    -- Script Overflow (0x018C)
    -- Module was reset due to script stack overflow.
    -- In BLE BGScript there is a script stack overflow detection mechanism. This solution resets module
    -- when script stack overflow is detected. After next boot script failure event with specific error code is
    -- generated right after system boot event.
    -- This feature works only with BLE SDK version 1.7.0 or newer that support script stack overflow
    -- detection mechanism. For this feature to work correctly update of bootloader is needed.
    | BGErrScriptOverflow

    -- An error unknown by this library.
    -- Containts the received error code as an unsigned byte
    | BGErrUnknown UInt16
    deriving Show

instance Binary BGAPIError where
    put m = do
        putWord16le $ case m of
            BGErrSuccess           -> 0x0000
            BGErrInvalidParameter  -> 0x0180
            BGErrWrongState        -> 0x0181
            BGErrOutOfMemory       -> 0x0182
            BGErrNotImplemented    -> 0x0183
            BGErrNotRecognized     -> 0x0184
            BGErrTimeout           -> 0x0185
            BGErrNotConnected      -> 0x0186
            BGErrFlow              -> 0x0187
            BGErrUserAttribute     -> 0x0188
            BGErrInvalidLicenseKey -> 0x0189
            BGErrCommandTooLong    -> 0x018A
            BGErrOutOfBonds        -> 0x018B
            BGErrScriptOverflow    -> 0x018C
            BGErrUnknown errC      -> errC
    get = do
        errC <- getWord16le
        return $ case errC of
            0x0000 -> BGErrSuccess
            0x0180 -> BGErrInvalidParameter
            0x0181 -> BGErrWrongState
            0x0182 -> BGErrOutOfMemory
            0x0183 -> BGErrNotImplemented
            0x0184 -> BGErrNotRecognized
            0x0185 -> BGErrTimeout
            0x0186 -> BGErrNotConnected
            0x0187 -> BGErrFlow
            0x0188 -> BGErrUserAttribute
            0x0189 -> BGErrInvalidLicenseKey
            0x018A -> BGErrCommandTooLong
            0x018B -> BGErrOutOfBonds
            0x018C -> BGErrScriptOverflow
            _     -> BGErrUnknown errC

-- Bluetooth errors

data BluetoothError
    -- Success (0x0000)
    = BLErrSuccess

    -- Authentication Failure (0x0205)
    -- Pairing or authentication failed due to incorrect results in the pairing or authentication procedure. This could be
    -- due to an incorrect PIN or Link Key
    | BLErrAuthenticationFailure

    -- Pin or Key Missing (0x0206)
    -- Pairing failed because of missing PIN, or authentication failed because of missing Key.
    -- Silicon Labs
    | BLErrPinOrKeyMissing

    -- Memory Capacity Exceeded (0x0207)
    -- Controller is out of memory.
    | BLErrMemoryCapacityExceeded

    -- Connection Timeout (0x0208)
    -- Link supervision timeout has expired.
    | BLErrConnectionTimeout

    -- Connection Limit Exceeded (0x0209)
    -- Controller is at limit of connections it can support.
    | BLErrConnectionLimitExceeded

    -- Command Disallowed (0x020C)
    -- Command requested cannot be executed because the Controller is in a state where it cannot process this
    -- command at this time.
    | BLErrCommandDisallowed

    -- Invalid Command Parameters (0x0212)
    -- Command contained invalid parameters.
    | BLErrInvalidCommandParameters

    -- Remote User Terminated Connection (0x0213)
    -- User on the remote device terminated the connection.
    | BLErrRemoteUserTerminatedConnection

    -- Connection Terminated by Local Host (0x0216)
    -- Local device terminated the connection.
    | BLErrConnectionTErminagedByLocalHost

    -- LL Response Timeout (0x0222)
    -- Connection terminated due to link-layer procedure timeout.
    | BLErrLLResponseTimeout

    -- LL Instant Passed (0x0228)
    -- Received link-layer control packet where instant was in the past.
    | BLErrLLInstantPassed

    -- Controller Busy (0x023A)
    -- Operation was rejected because the controller is busy and unable to process the request.
    | BLErrControllerBusy

    -- Unacceptable Connection Interval (0x023B)
    -- The Unacceptable Connection Interval error code indicates that the remote device terminated the connection
    -- because of an unacceptable connection interval.
    | BLErrUnacceptableConnectionInterval

    -- Directed Advertising Timeout (0x023C)
    -- Directed advertising completed without a connection being created.
    | BLErrDirectedAdvertisingTimeout

    -- MIC Failure (0x023D)
    -- Connection was terminated because the Message Integrity Check (MIC) failed on a received packet.
    | BLErrMICFailure

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
    | BLErrConnectionFailedToBeEstablised

    -- Passkey Entry Failed (0x0301)
    -- The user input of passkey failed, for example, the user cancelled the operation
    | BLErrPasskeyEntryFailed

    -- OOB Data is not available (0x0302)
    -- Out of Band data is not available for authentication
    | BLErrOOBDataIsNotAvailable

    -- Authentication Requirements (0x0303)
    -- The pairing procedure cannot be performed as authentication requirements cannot be met due to IO capabilities
    -- of one or both devices
    | BLErrAuthenticationRequirements

    -- Confirm Value Failed (0x0304)
    -- The confirm value does not match the calculated compare value
    | BLErrConfirmValueFailed

    -- Pairing Not Supported (0x0305)
    -- Pairing is not supported by the device
    | BLErrPairingNotSupported

    -- Encryption Key Size (0x0306)
    -- The resultant encryption key size is insufficient for the security requirements of this device
    | BLErrEncryptionKeySize

    -- Command Not Supported (0x0307)
    -- The SMP command received is not supported on this device
    | BLErrCommandNotSupported

    -- Unspecified Reason (0x0308)
    -- Pairing failed due to an unspecified reason
    | BLErrUnspecifiedReason

    -- Repeated Attempts (0x0309)
    -- Pairing or authentication procedure is disallowed because too little time has elapsed since last pairing request
    -- or security request
    | BLErrRepeatedAttempts

    -- Invalid Parameters (0x030A)
    -- The Invalid Parameters error code indicates: the command length is invalid or a parameter is outside of the
    -- specified range.
    | BLErrInvalidParameters

    -- Invalid Handle (0x0401)
    -- The attribute handle given was not valid on this server
    | BLErrInvalidHandle

    -- Read Not Permitted (0x0402)
    -- The attribute cannot be read
    | BLErrReadNotPermitted

    -- Write Not Permitted (0x0403)
    -- The attribute cannot be written
    | BLErrWriteNotPermitted

    -- Invalid PDU (0x0404)
    -- The attribute PDU was invalid
    | BLErrInvalidPDU

    -- Insufficient Authentication (0x0405)
    -- The attribute requires authentication before it can be read or written.
    | BLErrInsufficientAuthentication

    -- Request Not Supported (0x0406)
    -- Attribute Server does not support the request received from the client.
    | BLErrRequestNotSupported

    -- Invalid Offset (0x0407)
    -- Offset specified was past the end of the attribute
    | BLErrInvalidOffset

    -- Insufficient Authorization (0x0408)
    -- The attribute requires authorization before it can be read or written.
    | BLErrInsufficientAuthorization

    -- Prepare Queue Full (0x0409)
    -- Too many prepare writes have been queueud
    | BLErrPrepareQueueFull

    -- Attribute Not Found (0x040A)
    -- No attribute found within the given attribute handle range.
    | BLErrAttributeNotFound

    -- Attribute Not Long (0x040B)
    -- The attribute cannot be read or written using the Read Blob Request
    | BLErrAttributeNotLong

    -- Insufficient Encryption Key Size (0x040C)
    -- The Encryption Key Size used for encrypting this link is insufficient.
    | BLErrInsufficientEncryptionKeySize

    -- Invalid Attribute Value Length (0x040D)
    -- The attribute value length is invalid for the operation
    | BLErrInvalidAttributeValueLength

    -- Unlikely Error (0x040E)
    -- The attribute request that was requested has encountered an error that was unlikely, and therefore could not be
    -- completed as requested.
    | BLErrUnlikelyError

    -- Insufficient Encryption (0x040F)
    -- The attribute requires encryption before it can be read or written.
    | BLErrInsufficientEncryption

    -- Unsupported Group Type (0x0410)
    -- The attribute type is not a supported grouping attribute as defined by a higher layer specification.
    | BLErrUnsupportedGroupType

    -- Insufficient Resources (0x0411)
    -- Insufficient Resources to complete the request
    | BLErrInsufficientResources

    -- Application Error Codes (0x0480)
    -- Application error code defined by a higher layer specification.
    -- The error code range 0x80-0x9F is reserved for application level errors.
    | BLErrApplicationErrorCode UInt8

    -- And error code unknown by this library
    | BLErrUnknown UInt16
    deriving Show

instance Binary BluetoothError where
    put m = do
        putWord16le $ case m of
            BLErrSuccess                         -> 0x0000
            BLErrAuthenticationFailure           -> 0x0205
            BLErrPinOrKeyMissing                 -> 0x0206
            BLErrMemoryCapacityExceeded          -> 0x0207
            BLErrConnectionTimeout               -> 0x0208
            BLErrConnectionLimitExceeded         -> 0x0209
            BLErrCommandDisallowed               -> 0x020C
            BLErrInvalidCommandParameters        -> 0x0212
            BLErrRemoteUserTerminatedConnection  -> 0x0213
            BLErrConnectionTErminagedByLocalHost -> 0x0216
            BLErrLLResponseTimeout               -> 0x0222
            BLErrLLInstantPassed                 -> 0x0228
            BLErrControllerBusy                  -> 0x023A
            BLErrUnacceptableConnectionInterval  -> 0x023B
            BLErrDirectedAdvertisingTimeout      -> 0x023C
            BLErrMICFailure                      -> 0x023D
            BLErrConnectionFailedToBeEstablised  -> 0x023E
            BLErrPasskeyEntryFailed              -> 0x0301
            BLErrOOBDataIsNotAvailable           -> 0x0302
            BLErrAuthenticationRequirements      -> 0x0303
            BLErrConfirmValueFailed              -> 0x0304
            BLErrPairingNotSupported             -> 0x0305
            BLErrEncryptionKeySize               -> 0x0306
            BLErrCommandNotSupported             -> 0x0307
            BLErrUnspecifiedReason               -> 0x0308
            BLErrRepeatedAttempts                -> 0x0309
            BLErrInvalidParameters               -> 0x030A
            BLErrInvalidHandle                   -> 0x0401
            BLErrReadNotPermitted                -> 0x0402
            BLErrWriteNotPermitted               -> 0x0403
            BLErrInvalidPDU                      -> 0x0404
            BLErrInsufficientAuthentication      -> 0x0405
            BLErrRequestNotSupported             -> 0x0406
            BLErrInvalidOffset                   -> 0x0407
            BLErrInsufficientAuthorization       -> 0x0408
            BLErrPrepareQueueFull                -> 0x0409
            BLErrAttributeNotFound               -> 0x040A
            BLErrAttributeNotLong                -> 0x040B
            BLErrInsufficientEncryptionKeySize   -> 0x040C
            BLErrInvalidAttributeValueLength     -> 0x040D
            BLErrUnlikelyError                   -> 0x040E
            BLErrInsufficientEncryption          -> 0x040F
            BLErrUnsupportedGroupType            -> 0x0410
            BLErrInsufficientResources           -> 0x0411
            BLErrApplicationErrorCode errC       -> (fromIntegral errC .&. 0x001f) .|. 0x0480
            BLErrUnknown errC                    -> errC

    get = do
        errC <- getWord16le
        return $ case errC of
            0x0000 -> BLErrSuccess
            0x0205 -> BLErrAuthenticationFailure
            0x0206 -> BLErrPinOrKeyMissing
            0x0207 -> BLErrMemoryCapacityExceeded
            0x0208 -> BLErrConnectionTimeout
            0x0209 -> BLErrConnectionLimitExceeded
            0x020C -> BLErrCommandDisallowed
            0x0212 -> BLErrInvalidCommandParameters
            0x0213 -> BLErrRemoteUserTerminatedConnection
            0x0216 -> BLErrConnectionTErminagedByLocalHost
            0x0222 -> BLErrLLResponseTimeout
            0x0228 -> BLErrLLInstantPassed
            0x023A -> BLErrControllerBusy
            0x023B -> BLErrUnacceptableConnectionInterval
            0x023C -> BLErrDirectedAdvertisingTimeout
            0x023D -> BLErrMICFailure
            0x023E -> BLErrConnectionFailedToBeEstablised
            0x0301 -> BLErrPasskeyEntryFailed
            0x0302 -> BLErrOOBDataIsNotAvailable
            0x0303 -> BLErrAuthenticationRequirements
            0x0304 -> BLErrConfirmValueFailed
            0x0305 -> BLErrPairingNotSupported
            0x0306 -> BLErrEncryptionKeySize
            0x0307 -> BLErrCommandNotSupported
            0x0308 -> BLErrUnspecifiedReason
            0x0309 -> BLErrRepeatedAttempts
            0x030A -> BLErrInvalidParameters
            0x0401 -> BLErrInvalidHandle
            0x0402 -> BLErrReadNotPermitted
            0x0403 -> BLErrWriteNotPermitted
            0x0404 -> BLErrInvalidPDU
            0x0405 -> BLErrInsufficientAuthentication
            0x0406 -> BLErrRequestNotSupported
            0x0407 -> BLErrInvalidOffset
            0x0408 -> BLErrInsufficientAuthorization
            0x0409 -> BLErrPrepareQueueFull
            0x040A -> BLErrAttributeNotFound
            0x040B -> BLErrAttributeNotLong
            0x040C -> BLErrInsufficientEncryptionKeySize
            0x040D -> BLErrInvalidAttributeValueLength
            0x040E -> BLErrUnlikelyError
            0x040F -> BLErrInsufficientEncryption
            0x0410 -> BLErrUnsupportedGroupType
            0x0411 -> BLErrInsufficientResources
            _      ->
                if errC >= 0x0480 && errC <= 0x049f
                    then BLErrApplicationErrorCode $ fromIntegral (errC .&. 0x1f)
                    else BLErrUnknown errC
