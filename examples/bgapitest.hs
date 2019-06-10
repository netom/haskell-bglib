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

        liftIO $ putStrLn "Running hello"
        systemHello
        liftIO $ putStrLn ""

        liftIO $ putStrLn "We should get a \"not connected\" error:"
        attclientAttributeWrite 0 0 "e" >>= liftIO . print
        liftIO $ putStrLn ""

        liftIO $ putStrLn "Getting Bluetooth Address:"
        systemAddressGet >>= liftIO . print
        liftIO $ putStrLn ""

        let aeskey = "abcdefgh12345678"
        liftIO $ putStrLn $ "Setting AES key to " ++ aeskey
        systemAesSetkey $ toUInt8Array $ BSS.pack $ aeskey
        liftIO $ putStrLn ""

        let plaintext = "This is plain"
        liftIO $ putStrLn $ "Encrypting: " ++ plaintext
        encrypted <- systemAesEncrypt $ toUInt8Array $ BSS.pack $ plaintext
        liftIO $ putStrLn $ "Encrypted: " ++ bsShowHex (fromUInt8Array encrypted)
        liftIO $ putStrLn ""

        liftIO $ putStrLn $ "Decrypting"
        decrypted <- systemAesDecrypt encrypted
        liftIO $ putStrLn $ "Decrypted: " ++ BSS.unpack (fromUInt8Array decrypted)
        liftIO $ putStrLn ""

        -- Register an event handler for scan responses. Can be done anywhere.
        -- The handler forks a thread that runs forever, and can be terminated
        -- later if necessary.
        tid <- evtGapScanResponse $ \(rssi, _, sender, _, _, _) -> do
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
