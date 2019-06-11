{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BGLib.Commands
import           BGLib.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSS
import           Data.Semigroup ((<>))
import           Options.Applicative
import           Prelude hiding (print, putStrLn)
import qualified Prelude as P
import           System.Exit
import           System.Hardware.Serialport

data AppOptions = AppOptions
    { appOptSerialPort :: String
    , appOptDebug      :: Bool
    }

data App = App
    { appOptions :: AppOptions
    , appSerialPort :: SerialPort
    , appBGChan  :: TChan BgPacket
    }

instance HasSerialPort App where
    getSerialPort = appSerialPort

instance HasBGChan App where
    getBGChan = appBGChan

instance HasDebug App where
    getDebug = appOptDebug . appOptions

optParser :: Parser AppOptions
optParser = AppOptions
        <$> argument str 
            (  metavar "PORT"
            <> help "Serial port" )
        <*> switch
            ( long "debug"
            <> short 'd'
            <> help "Whether to be quiet" )

execApp :: env -> ReaderT env m a -> m a
execApp = flip runReaderT

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . P.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . P.print

main :: IO ()
main = do
    appOpts <- execParser $
        info
            ( optParser <**> helper )
            (  fullDesc
            <> progDesc "Execute a short battery of test on port PORT"
            <> header "bgapitest - a short text / example for haskell-bglib"
            )

    app <- App
        <$> return appOpts
        <*> openSerial
            (appOptSerialPort appOpts)
            (SerialPortSettings CS115200 8 One NoParity NoFlowControl 1000)
        <*> atomically newBroadcastTChan

    execApp app $ do
        -- Register an event handler for protocol errors.
        _ <- evtSystemProtocolError $ \reason -> do
            die $ "*** PROTOCOL ERROR " ++ show reason

            -- Starts a thread that keeps reading packets from the serial port,
        -- pushing them to the broadcast TChan
        startPacketReader

        putStrLn "Running hello"
        systemHello
        putStrLn "If you can read this, we're fine. :)"
        putStrLn ""

        putStrLn "Getting system information:"
        (major, minor, patch, build, llVersion, protocolVersion, hw) <- systemGetInfo
        putStrLn $ "Major version:      " ++ show major
        putStrLn $ "Minor version:      " ++ show minor
        putStrLn $ "Patch version:      " ++ show patch
        putStrLn $ "Build Version:      " ++ show build
        putStrLn $ "Link Layer version: " ++ show llVersion
        putStrLn $ "Protocol version:   " ++ show protocolVersion
        putStrLn $ "Hardware version:   " ++ show hw
        putStrLn ""

        putStrLn "We should get a \"not connected\" error:"
        attclientAttributeWrite 0 0 "e" >>= print
        putStrLn ""

        putStrLn "Getting Bluetooth Address:"
        systemAddressGet >>= print
        putStrLn ""

        putStrLn "Running some encryption-decription tests"
        putStrLn ""

        let aeskey = "abcdefgh12345678"
        putStrLn $ "Setting AES key to " ++ aeskey
        systemAesSetkey $ toUInt8Array $ BSS.pack $ aeskey
        putStrLn ""

        let plaintext = "This is plain"
        putStrLn $ "Encrypting: " ++ plaintext
        encrypted <- systemAesEncrypt $ toUInt8Array $ BSS.pack $ plaintext
        putStrLn $ "Encrypted: " ++ bsShowHex (fromUInt8Array encrypted)
        putStrLn ""

        putStrLn $ "Decrypting"
        decrypted <- systemAesDecrypt encrypted
        putStrLn $ "Decrypted: " ++ BSS.unpack (fromUInt8Array decrypted)
        putStrLn ""

        -- Register an event handler for scan responses. Can be done anywhere.
        -- The handler forks a thread that runs forever, and can be terminated
        -- later if necessary.
        tid <- evtGapScanResponse $ \rssi _ sender _ _ _ -> do
            print rssi
            print sender
            putStrLn ""
            return True -- We'd like to listen to further events.

        _ <- gapDiscover GapDiscoverGeneric

        liftIO $ threadDelay 5000000

        _ <- gapEndProcedure

        liftIO $ killThread tid

        -- Let's cause trouble.
        s <- askSerialPort
        _ <- liftIO $ send s $ BSS.pack "a"
        liftIO $ threadDelay 5000000
