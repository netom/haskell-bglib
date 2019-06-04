{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module TH
    ( MsgTmp(..)
    , mkMessages
    ) where
    

import qualified Data.ByteString as BSS
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax
import           Types

data MsgTmp
    = MTTmp
        { msgtName           :: String
        , msgtCommandId      :: Int
        , msgtMessageType    :: BgMessageType
        , msgtTechnologyType :: BgTecnologyType
        , msgtCommandClass   :: BgCommandClass
        , msgParams          :: [Name]
        }
    | MTCmd
        { cmdTmp :: MsgTmp
        , rspTmp :: MsgTmp
        }
    | MTEvt
        { evtTmp :: MsgTmp }

-- Apply a "ConT <name>" for each name to a type
appNames :: [Name] -> Type -> Type
appNames [] t = t
appNames (n:ns) t = AppT (appNames ns t) (ConT n)

-- Create a tuple from a list of type names
mkTupleType :: [Name] -> Type
mkTupleType ns = appNames (reverse ns) $ TupleT $ length ns

-- Create a data constructor from a name and a list of type names
mkDataConstr :: String -> [Name] -> ConQ
mkDataConstr conName fieldTypeNames = do
    name <- qNewName conName
    return $ NormalC name [ (Bang NoSourceUnpackedness NoSourceStrictness, mkTupleType fieldTypeNames ) ]

mkMessages :: [MsgTmp] -> DecsQ
mkMessages msgTmps = do
    msgsName <- qNewName "BgMessage"

    --let bgMessageCons = [mkDataConstr "CmdAttClientAttributeWrite" [''UInt8, ''UInt16, ''UInt8, ''BSS.ByteString]]
    let bgMessageCons = concatMap mkMessageCon msgTmps

    msgsDTDec <- dataD
        (return [])
        msgsName
        []
        Nothing
        bgMessageCons
        []

    -- TODO: mkPacket :: BgMessage -> BgPacket
    -- TODO: parsePacket :: BgPacket -> BgMessage

    return [msgsDTDec]

    where
        mkMessageCon :: MsgTmp -> [ConQ]
        mkMessageCon MTTmp{..} = error "You can only use MTCmd and MTEvt here."
        mkMessageCon (MTEvt (MTTmp n _ _ _ _ ns)) = [mkDataConstr n ns]
        mkMessageCon (MTCmd (MTTmp n1 _ _ _ _ ns1) (MTTmp n2 _ _ _ _ ns2)) = [mkDataConstr n1 ns1, mkDataConstr n2 ns2]
        mkMessageCon _ = error "Invalid event or command definition"
