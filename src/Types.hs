{-# LANGUAGE RecordWildCards #-}

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
    ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString as BSS
import qualified Data.Int as I
import qualified Data.Word as W
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

instance Binary UInt8Array where
    put UInt8Array{..} = do
        putWord8 $ fromIntegral $ BSS.length fromUInt8Array
        put fromUInt8Array

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
    , gbhTechnologyType :: BgTecnologyType
    , bghLength         :: UInt16 -- Only 11 bits actually
    , bghCommandClass   :: BgCommandClass
    , bghCommandId      :: UInt8
    } deriving Show

instance Binary BgPacketHeader where
    put BgPacketHeader{..} = do
        putWord8
            $   fromIntegral (fromEnum bghMessageType `shift` 7)
            .|. fromIntegral (fromEnum gbhTechnologyType `shift` 3)
            .|. fromIntegral (bghLength .&. 0x0700 `shift` (-8))
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
        let gbhTechnologyType = toEnum $ fromIntegral $ oct0 `shift` (-3) .&. 0x0f
        let bghLength         = (fromIntegral lHigh `shift` 8) + (fromIntegral lLow) :: UInt16
        let bghCommandClass   = toEnum $ fromIntegral clsId
        let bghCommandId      = cmdId

        return $ BgPacketHeader{..}

data BgPacket = BgPacket
    { bgpHeader  :: BgPacketHeader
    , bgpPayload :: BSS.ByteString
    } deriving Show

instance Binary BgPacket where
    put BgPacket{..} = do
        put bgpHeader
        put bgpPayload

    get = do
        bgpHeader@BgPacketHeader{..} <- get
        bgpPayload <- getByteString $ fromIntegral bghLength
        return BgPacket{..}
