{-# LANGUAGE TemplateHaskell #-}

module Messages where

import qualified Data.ByteString as BSS
import           TH
import           Types

mkMessages
    [ MTCmd
        ( MTTmp "CmdAttClientAttributeWrite" 0x05 BgMsgCR BgBlue BgClsAttributeClient [''UInt8, ''UInt16, ''UInt8Array] )
        ( MTTmp "RspAttClientAttributeWrite" 0x05 BgMsgCR BgBlue BgClsAttributeClient [''UInt8, ''UInt16] )

    , MTCmd
        ( MTTmp "CmdSystemAddressGet" 0x02 BgMsgCR BgBlue BgClsSystem [] )
        ( MTTmp "RspSystemAddressGet" 0x02 BgMsgCR BgBlue BgClsSystem [''BdAddr] )

    , MTEvt $ MTTmp "EvtAttClientProcedureCompleted" 0x01 BgMsgEvent BgBlue BgClsAttributeClient [''UInt8, ''UInt16, ''UInt16]
    ]
