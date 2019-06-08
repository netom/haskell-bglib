{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Int8
    , UInt8
    , UInt16
    , UInt32
    , BdAddr
    , UInt8Array
    , BgMessageType(..)
    , BgTecnologyType(..)
    , BgCommandClass(..)
    , BgPacketHeader(..)
    , BgPacket(..)
    , GapDiscoveryMode(..)
    , HasSerialPort(..)
    , HasBGChan(..)
    , RebootMode(..)
    , askDupBGChan
    , askBGChan
    , askSerialPort
    , bgHeaderMatches
    , fromUInt8Array
    , prettyShowBS
    , toUInt8Array
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

-- 0 gap_discover_limited Discover only limited discoverable devices, that is, Slaves which have the
-- LE Limited Discoverable Mode bit set in the Flags AD type of their
-- advertisement packets.
-- 1 gap_discover_generic Discover limited and generic discoverable devices, that is, Slaves which
-- have the LE Limited Discoverable Mode or the LE General Discoverable
-- Mode bit set in the Flags AD type of their advertisement packets.
-- 2 gap_discover_observation Discover all devices regardless of the Flags AD typ
data GapDiscoveryMode
    = GapDiscoverLimited
    | GapDiscoverGeneric
    | GapDiscoverOvservation
    deriving (Show, Enum)

instance Binary GapDiscoveryMode where
    put m = do
        putWord16le $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord16le

data RebootMode
    = RebootNormal
    | RebootDfu
    deriving (Show, Enum)

instance Binary RebootMode where
    put m = do
        putWord8 $ fromIntegral $ fromEnum m
    get = do
        toEnum . fromIntegral <$> getWord8

-- 0 attclient_attribute_value_type_read Value was read
-- 1 attclient_attribute_value_type_notify Value was notified
-- 2 attclient_attribute_value_type_indicate Value was indicated
-- 3 attclient_attribute_value_type_read_by_type Value was read
-- 4 attclient_attribute_value_type_read_blob Value was part of a long attribute
-- 5 attclient_attribute_value_type_indicate_rsp_req Value was indicated and the remote device is
-- waiting for a confirmation
data AttclientAttributeValueType
    = AAVTRead
    | AAVTNotify
    | AAVTIndicate
    | AAVTReadByType
    | AAVTReadBlob
    | AAVTIndicateRsqReq

-- 0 attributes_attribute_change_reason_write_request Value was written by remote device using
-- write request
-- 1 attributes_attribute_change_reason_write_command Value was written by remote device using
-- write command
-- 2 attributes_attribute_change_reason_write_request_user Local attribute value was written by the
-- remote device, but the Bluetooth Smart
-- stack is waiting for the write to be
-- confirmed by the application.
-- User Write Response command should
-- be used to send the confirmation.
-- For this reason to appear the attribute in
-- the GATT database must have the user
-- property enabled.
data AttributesAttributeChangeReason
    = AACRWriteRequest
    | AACRWriteCommand
    | AACRWriteRequestUser

-- 1 attributes_attribute_status_flag_notify Notifications are enabled
-- 2 attributes_attribute_status_flag_indicate Indications are enabled
data AttributesAttributeStatusFlag
    = AASFNotify
    | AASFIndicate

-- Connection status flags
-- bit 0 connection_connected This status flag tells the connection exists to a remote device.
-- bit 1 connection_encrypted This flag tells the connection is encrypted.
-- bit 2 connection_completed Connection completed flag, which is used to tell a new connection
-- has been created.
-- bit 3 connection_parameters_change This flag tells that connection parameters have changed and. It is
-- set when connection parameters have changed due to a link layer
-- operation.

csConnected :: UInt8 -> Bool
csConnected = (/= 0) . (.&. 1)

csEncrypted = undefined

csCompleted = undefined

csParametersChanged = undefined

-- TODO
--
-- 0x01 GAP_AD_FLAG_LIMITED_DISCOVERABLE Limited discoverability
-- 0x02 GAP_AD_FLAG_GENERAL_DISCOVERABLE General discoverability
-- 0x04 GAP_AD_FLAG_BREDR_NOT_SUPPORTED BR/EDR not supported
-- 0x10 GAP_AD_FLAG_SIMULTANEOUS_LEBREDR_CTRL BR/EDR controller
-- 0x20 GAP_AD_FLAG_SIMULTANEOUS_LEBREDR_HOST BE/EDR host
-- 0x1f GAP_AD_FLAG_MASK -

--TODO
--
-- AD Type Flags--gap
-- Table: VALUES
-- Value Name
-- Description
-- 0 gap_ad_type_none
-- 1 gap_ad_type_flags
-- 2 gap_ad_type_services_16bit_more
-- 3 gap_ad_type_services_16bit_all
-- 4 gap_ad_type_services_32bit_more
-- 5 gap_ad_type_services_32bit_all
-- 6 gap_ad_type_services_128bit_more
-- 7 gap_ad_type_services_128bit_all
-- 8 gap_ad_type_localname_short
-- 9 gap_ad_type_localname_complete
-- 10 gap_ad_type_txpower

-- TODO
--
-- Advertising policy--gap
-- Advertising policy
-- Table: VALUES
-- Value Name Description
-- 0 gap_adv_policy_all Respond to scan requests from any master, allow connection
-- from any master (default)
-- 1 gap_adv_policy_whitelist_scan Respond to scan requests from whitelist only, allow connection
-- from any
-- 2 gap_adv_policy_whitelist_connect Respond to scan requests from any, allow connection from
-- whitelist only
-- 3 gap_adv_policy_whitelist_all Respond to scan re

-- TODO
--
-- Bluetooth Address Types--gap
-- Bluetooth address types
-- Table: VALUES
-- Value Name Description
-- 0 gap_address_type_public Public Address
-- 1 gap_address_type_random Random Address

-- TODO
--
-- GAP Connectable Mode--gap
-- GAP connectable modes
-- Table: VALUES
-- Value Name Description
-- 0 gap_non_connectable Not connectable
-- 1 gap_directed_connectable Directed Connectable
-- 2 gap_undirected_connectable Undirected connectable
-- 3 gap_scannable_non_connectable Same as non-connectable, but also supports ADV_SCAN_IND
-- packets. Device ac

-- TODO
--
-- GAP Discoverable Mode--gap
-- GAP discoverable modes
-- Table: VALUES
-- Value Name Description
-- 0 gap_non_discoverable Non-discoverable mode: the LE Limited Discoverable Mode and the
-- LE General Discoverable Mode bits are NOT set in the Flags AD
-- type. A master can still connect to the advertising slave in this mode.
-- 1 gap_limited_discoverable Discoverable using limited scanning mode: the advertisement
-- packets will carry the LE Limited Discoverable Mode bit set in the
-- Flags AD type.
-- 2 gap_general_discoverable Discoverable using general scanning mode: the advertisement
-- packets will carry the LE General Discoverable Mode bit set in the
-- Flags AD type.
-- 3 gap_broadcast Same as gap_non_discoverable above.
-- 4 gap_user_data In this advertisement the advertisement and scan response data
-- defined by user will be used. The user is responsible of building the
-- advertisement data so that it also contains the appropriate desired
-- Flags AD type.
-- 0x80 gap_enhanced_broadcasting When turning the most highest bit on in GAP discoverable mode, the
-- remote devices that send scan request packets to the advertiser are
-- reported back to the application through Scan Response event.
-- This is so called Enhanced Broadcasting mode.

-- TODO
--
-- GAP Discover Mode--gap
-- GAP Discover modes
-- Table: VALUES
-- Value Name Description
-- 0 gap_discover_limited Discover only limited discoverable devices, that is, Slaves which have
-- the LE Limited Discoverable Mode bit set in the Flags AD type of their
-- advertisement packets.
-- 1 gap_discover_generic Discover limited and generic discoverable devices, that is, Slaves which
-- have the LE Limited Discoverable Mode or the LE General
-- Discoverable Mode bit set in the Flags AD type of their advertisement
-- packets.
-- 2 gap_discover_observation Discover all devices regardless of the Flags AD type, so also devices in
-- non-discoverable mode will be reported to host.

-- TODO
--
-- SCAN_HEADER_FLAGS--gap
-- Scan header flags
-- Table: VALUES
-- Value Name Description
-- 0 GAP_SCAN_HEADER_ADV_IND Connectable undirected advertising event
-- 1 GAP_SCAN_HEADER_ADV_DIRECT_IND Connectable directed advertising event
-- 2 GAP_SCAN_HEADER_ADV_NONCONN_IND Non-connectable undirected advertising event
-- 3 GAP_SCAN_HEADER_SCAN_REQ Scanner wants information from Advertiser
-- 4 GAP_SCAN_HEADER_SCAN_RSP Advertiser gives more information to Scanner
-- 5 GAP_SCAN_HEADER_CONNECT_REQ Initiator wants to connect to Advertiser
-- 6 GAP_SCAN_HEADER_ADV_DISCOVER_IND Non-connectable undirected advertising event

-- TODO
--
-- Scan Policy--gap
-- Scan Policy
-- Table: VALUES
-- Value Name Description
-- 0 gap_scan_policy_all Accept All advertisement Packets (default)
-- 1 gap_scan_policy_whitelist Ignore advert

-- TODO
--
-- Bonding Keys--sm
-- Table: VALUES
-- Value Name Description
-- 0x01 sm_bonding_key_ltk LTK saved in master
-- 0x02 sm_bonding_key_addr_public Public Address
-- 0x04 sm_bonding_key_addr_static Static Address
-- 0x08 sm_bonding_key_irk Identity resolving key for resolvable private addresses
-- 0x10 sm_bonding_key_edivrand EDIV+RAND received from slave
-- 0x20 sm_bonding_key_csrk Connection signature resolving key
-- 0x40 sm_bonding_key_masterid EDIV+RAND sent to master

-- TODO
--
-- SMP IO Capabilities--sm
-- Security Manager I/O Capabilities
-- Table: VALUES
-- Value Name Description
-- 0 sm_io_capability_displayonly Display Only
-- 1 sm_io_capability_displayyesno Display with Yes/No-buttons
-- 2 sm_io_capability_keyboardonly Keyboard Only
-- 3 sm_io_capability_noinputnooutput No Input and No Output
-- 4 sm_io_capability_keyboarddisplay Display with Keyboard

-- TODO
--
-- Endpoints--system
-- Data Endpoints used in data routing and interface configuration
-- Table: VALUES
-- Value Name Description
-- 0 system_endpoint_api Command Parser
-- 1 system_endpoint_test Radio Test
-- 2 system_endpoint_script BGScript (not used)
-- 3 system_endpoint_usb USB Interface
-- 4 system_endpoint_uart0 USART 0
-- 5 system_endpoint_uart1 USART 1

-- TODO
--
-- 4.11.1 BGAPI Errors
-- Errors related to BGAPI protocol
-- Invalid Parameter (0x0180)
-- Command contained invalid parameter
-- Device in Wrong State (0x0181)
-- Device is in wrong state to receive command
-- Out Of Memory (0x0182)
-- Device has run out of memory
-- Silicon Labs
-- Page 213 of 219Feature Not Implemented (0x0183)
-- Feature is not implemented
-- Command Not Recognized (0x0184)
-- Command was not recognized
-- Timeout (0x0185)
-- Command or Procedure failed due to timeout
-- Not Connected (0x0186)
-- Connection handle passed is to command is not a valid handle
-- flow (0x0187)
-- Command would cause either underflow or overflow error
-- User Attribute (0x0188)
-- User attribute was accessed through API which is not supported
-- Invalid License Key (0x0189)
-- No valid license key found
-- Command Too Long (0x018A)
-- Command maximum length exceeded
-- Out of Bonds (0x018B)
-- Bonding procedure can't be started because device has no space left for bond.
-- Script Overflow (0x018C)
-- Module was reset due to script stack overflow.
-- In BLE BGScript there is a script stack overflow detection mechanism. This solution resets module
-- when script stack overflow is detected. After next boot script failure event with specific error code is
-- generated right after system boot event.
-- This feature works only with BLE SDK version 1.7.0 or newer that support script stack overflow
-- detection mechanism. For this feature to work correctly update of bootloader is needed.
-- 4.11.2 Bluetooth Errors
-- Bluetooth errors
-- Authentication Failure (0x0205)
-- Pairing or authentication failed due to incorrect results in the pairing or authentication procedure. This could be
-- due to an incorrect PIN or Link Key
-- Pin or Key Missing (0x0206)
-- Pairing failed because of missing PIN, or authentication failed because of missing Key.
-- Silicon Labs
-- Page 214 of 219Memory Capacity Exceeded (0x0207)
-- Controller is out of memory.
-- Connection Timeout (0x0208)
-- Link supervision timeout has expired.
-- Connection Limit Exceeded (0x0209)
-- Controller is at limit of connections it can support.
-- Command Disallowed (0x020C)
-- Command requested cannot be executed because the Controller is in a state where it cannot process this
-- command at this time.
-- Invalid Command Parameters (0x0212)
-- Command contained invalid parameters.
-- Remote User Terminated Connection (0x0213)
-- User on the remote device terminated the connection.
-- Connection Terminated by Local Host (0x0216)
-- Local device terminated the connection.
-- LL Response Timeout (0x0222)
-- Connection terminated due to link-layer procedure timeout.
-- LL Instant Passed (0x0228)
-- Received link-layer control packet where instant was in the past.
-- Controller Busy (0x023A)
-- Operation was rejected because the controller is busy and unable to process the request.
-- Unacceptable Connection Interval (0x023B)
-- The Unacceptable Connection Interval error code indicates that the remote device terminated the connection
-- because of an unacceptable connection interval.
-- Directed Advertising Timeout (0x023C)
-- Directed advertising completed without a connection being created.
-- MIC Failure (0x023D)
-- Connection was terminated because the Message Integrity Check (MIC) failed on a received packet.
-- Connection Failed to be Established (0x023E)
-- LL initiated a connection but the connection has failed to be established. Controller did not receive any packets
-- from remote end.
-- More in detail, an attempt to open a connection is made by the master by sending only one CONNECT_REQ ,
-- after which the master immediately transitions to connected state (BT4.1 Vol 6 Part B 4.4.4). If the advertiser for
-- any reason (like interference) does not catch the packet it will just continue advertising, while the master
-- remains in a fast termination mode, where it will only send 6 packets before failing, independent of supervision
-- Silicon Labs
-- Page 215 of 219timeout (in fact, a master starts using normal supervision timeout only after it has received at least one packet
-- from slave.) If the master does not receive anything by the time its 6 packets are sent, connection establishment
-- will be considered failed and this error will be reported to the host or to the BGScript. In a busy environment it is
-- normal to see roughly 1-2% error rate when opening connections.
-- 4.11.3 Security Manager Protocol Errors
-- Errors from Security Manager Protocol
-- Passkey Entry Failed (0x0301)
-- The user input of passkey failed, for example, the user cancelled the operation
-- OOB Data is not available (0x0302)
-- Out of Band data is not available for authentication
-- Authentication Requirements (0x0303)
-- The pairing procedure cannot be performed as authentication requirements cannot be met due to IO capabilities
-- of one or both devices
-- Confirm Value Failed (0x0304)
-- The confirm value does not match the calculated compare value
-- Pairing Not Supported (0x0305)
-- Pairing is not supported by the device
-- Encryption Key Size (0x0306)
-- The resultant encryption key size is insufficient for the security requirements of this device
-- Command Not Supported (0x0307)
-- The SMP command received is not supported on this device
-- Unspecified Reason (0x0308)
-- Pairing failed due to an unspecified reason
-- Silicon Labs
-- Page 216 of 219Repeated Attempts (0x0309)
-- Pairing or authentication procedure is disallowed because too little time has elapsed since last pairing request
-- or security request
-- Invalid Parameters (0x030A)
-- The Invalid Parameters error code indicates: the command length is invalid or a parameter is outside of the
-- specified range.
-- 4.11.4 Attribute Protocol Errors
-- Errors from Attribute Protocol
-- Invalid Handle (0x0401)
-- The attribute handle given was not valid on this server
-- Read Not Permitted (0x0402)
-- The attribute cannot be read
-- Write Not Permitted (0x0403)
-- The attribute cannot be written
-- Silicon Labs
-- Page 217 of 219Invalid PDU (0x0404)
-- The attribute PDU was invalid
-- Insufficient Authentication (0x0405)
-- The attribute requires authentication before it can be read or written.
-- Request Not Supported (0x0406)
-- Attribute Server does not support the request received from the client.
-- Invalid Offset (0x0407)
-- Offset specified was past the end of the attribute
-- Insufficient Authorization (0x0408)
-- The attribute requires authorization before it can be read or written.
-- Prepare Queue Full (0x0409)
-- Too many prepare writes have been queueud
-- Attribute Not Found (0x040A)
-- No attribute found within the given attribute handle range.
-- Attribute Not Long (0x040B)
-- The attribute cannot be read or written using the Read Blob Request
-- Insufficient Encryption Key Size (0x040C)
-- The Encryption Key Size used for encrypting this link is insufficient.
-- Invalid Attribute Value Length (0x040D)
-- The attribute value length is invalid for the operation
-- Unlikely Error (0x040E)
-- The attribute request that was requested has encountered an error that was unlikely, and therefore could not be
-- completed as requested.
-- Insufficient Encryption (0x040F)
-- The attribute requires encryption before it can be read or written.
-- Unsupported Group Type (0x0410)
-- The attribute type is not a supported grouping attribute as defined by a higher layer specification.
-- Insufficient Resources (0x0411)
-- Insufficient Resources to complete the request
-- Application Error Codes (0x0480)
-- Application error code defined by a higher layer specification.
