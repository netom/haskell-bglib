{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( testFunc
    ) where

import Data.Int as I
import Data.Word as W
import Data.Bits
import System.Hardware.Serialport
import Control.Applicative
import System.IO
import Data.Vector.Fixed.Unboxed
import Data.Bits

import qualified Data.ByteString as BS

-- int8           1 byte Signed 8-bit integer
type Int8 = I.Int8

-- uint8          1 byte Unsigned 8-bit integer
type UInt8 = W.Word8

-- uint16         2 bytes Unsigned 16-bit integer
type UInt16 = W.Word16

-- uint32         4 bytes Unsigned 32-bit integer
type UInt32 = W.Word32

-- uint8array     byte array, first byte is array size
type UInt8Array = BS.ByteString

-- bd_addr        Bluetooth address in little endian format
type BdAddr = BS.ByteString

data BgMessageType = BgMsgCR | BgMsgEvent | BgMsgUnknown UInt8 deriving (Eq, Show)

bgMT2I :: BgMessageType -> UInt8
bgMT2I = \case
    BgMsgCR        -> 0
    BgMsgEvent     -> 1
    BgMsgUnknown i -> i

bgI2MT :: UInt8 -> BgMessageType
bgI2MT = \case
    0 -> BgMsgCR
    1 -> BgMsgEvent
    i -> BgMsgUnknown i

data BgTecnologyType = BgBluetoot40SingleMode | BgWifi | BgTecnologyUnknown UInt8 deriving (Eq, Show)

bgTT2I :: BgTecnologyType -> UInt8
bgTT2I = \case
    BgBluetoot40SingleMode -> 0
    BgWifi                 -> 1
    BgTecnologyUnknown i   -> i

bgI2TT :: UInt8 -> BgTecnologyType
bgI2TT = \case
    0 -> BgBluetoot40SingleMode
    1 -> BgWifi
    i -> BgTecnologyUnknown i

data BgClassId
    = BgClsSystem
    | BgClsPersistentStore
    | BgClsAttributeDatabase
    | BgClsConnection
    | BgClsAttributeClient
    | BgClsSecurityManager
    | BgClsGenericAccessProfile
    | BgClsHardware
    | BgClsUnknown UInt8 deriving (Eq, Show)

bgCI2I :: BgClassId -> UInt8
bgCI2I = \case
    BgClsSystem               -> 0
    BgClsPersistentStore      -> 1
    BgClsAttributeDatabase    -> 2
    BgClsConnection           -> 3
    BgClsAttributeClient      -> 4
    BgClsSecurityManager      -> 5
    BgClsGenericAccessProfile -> 6
    BgClsHardware             -> 7
    BgClsUnknown i            -> i

bgI2CI :: UInt8 -> BgClassId
bgI2CI = \case
    0 -> BgClsSystem
    1 -> BgClsPersistentStore
    2 -> BgClsAttributeDatabase
    3 -> BgClsConnection
    4 -> BgClsAttributeClient
    5 -> BgClsSecurityManager
    6 -> BgClsGenericAccessProfile
    7 -> BgClsHardware
    i -> BgClsUnknown i

data BgPacket = BgPacket
    { bgpMessageType    :: BgMessageType
    , gbpTechnologyType :: BgTecnologyType
    , bgpLength         :: UInt16 -- Only 11 bits actually
    , bgpClassId        :: BgClassId
    , bgpCommandId      :: UInt8
    , bgpPayload        :: BS.ByteString
    } deriving Show

bgpOctet0 :: BgMessageType -> BgTecnologyType -> UInt16 -> UInt8
bgpOctet0 mt tt l
    =   bgMT2I mt `shift` 7
    .|. bgTT2I tt `shift` 3
    .|. fromIntegral (l .&. 0x0700 `shift` (-8))

writeBGPacket :: SerialPort -> BgPacket -> IO ()
writeBGPacket s BgPacket{..} = do
    send s $ BS.pack
        [ bgpOctet0 bgpMessageType gbpTechnologyType bgpLength
        , fromIntegral $ bgpLength .&. 0x00ff
        , bgCI2I bgpClassId
        , bgpCommandId
        ]
    send s bgpPayload
    return ()

readBGPacket :: SerialPort -> IO BgPacket
readBGPacket s = do
    header <- recv s 4

    if BS.length header /= 4
    then error "Could not read BGAPI header"
    else return ()

    let oct0  = BS.index header 0
    let lHigh = oct0 .&. 0x07
    let lLow  = BS.index header 1
    let clsId = BS.index header 2
    let cmdId = BS.index header 3

    let bgpMessageType = bgI2MT $ oct0 `shift` (-7)
    let gbpTechnologyType = bgI2TT $ oct0 `shift` (-3) .&. 0x0f
    let bgpLength = (fromIntegral $ lHigh `shift` 8) + (fromIntegral lLow) :: UInt16
    let bgpClassId = bgI2CI clsId
    let bgpCommandId = cmdId

    bgpPayload <- recv s (fromIntegral bgpLength)
    
    return $ BgPacket {..}

-- TODO: separate packet and header
-- TODO: write read operation that respects packet type

bs2UInt8 :: Int -> BS.ByteString -> UInt8
bs2UInt8 offs s =
    if BS.length s == offs + 1
    then fromIntegral (BS.index s offs)
    else error "ByteString length is not offset + 1, cannot convert to UInt8"

bs2UInt16 :: Int -> BS.ByteString -> UInt16
bs2UInt16 offs s =
    if BS.length s == offs + 2
    then  fromIntegral (BS.index s offs)
        + 0x10 * (fromIntegral $ BS.index s $ offs + 1)
    else error "ByteString length is not offset + 2, cannot convert to UInt16"

bs2UInt32 :: Int -> BS.ByteString ->  UInt32
bs2UInt32 offs s =
    if BS.length s == offs + 4
    then  fromIntegral (BS.index s offs)
        +     0x100 * (fromIntegral $ BS.index s $ offs + 1)
        +   0x10000 * (fromIntegral $ BS.index s $ offs + 2)
        + 0x1000000 * (fromIntegral $ BS.index s $ offs + 3)
    else error "ByteString length is not offset + 4, cannot convert to UInt32"

-- cmd_dfu_flash_set_address
--bgCmdDfuFlashSetAddress :: SerialPort -> IO UInt16
--bgCmdDfuFlashSetAddress s = do
--    writeBGPacket s $ BgPacket BgMsgCR 4 BgClsCoex 0x01 $ BS.pack [0, 0, 0, 0]
--    res <- readBGPacket s
--    return $ bs2UInt16 0 $ bgpPayload res

-- cmd_hardware_get_time
--bgCmdHardwareGetTime :: SerialPort -> IO (UInt32, UInt16)
--bgCmdHardwareGetTime s = do
--    writeBGPacket s $ BgPacket BgMsgCR 0 BgClsHw 0x0b ""
--    res <- readBGPacket s
--    let pl = bgpPayload res
--    return $ (bs2UInt32 0 pl, bs2UInt16 4 pl)

testFunc = do
    let port = "/dev/ttyACM3"

    s <- openSerial port $  SerialPortSettings CS115200 8 One NoParity NoFlowControl 1000

    res <- readBGPacket s
    print res
    res <- readBGPacket s
    print res

    closeSerial s
