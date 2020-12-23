{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           BGLib.Commands
import           BGLib.Types
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSS
import           Options.Applicative
import           Prelude hiding (print, putStrLn)
import qualified Prelude as P
import           System.Exit
import           System.Hardware.Serialport

-- This is our monad stack, most of the application runs inside this.
type AppM env a = ReaderT env IO a

-- We store the command line options here
data AppOptions = AppOptions
    { appOptSerialPort :: String
    , appOptDebug      :: Bool
    }

-- The data structure will be our "env", the environment stored in the
-- ReaderT env IO monad stack
data App = App
    { appOptions    :: AppOptions
    , appSerialPort :: SerialPort
    , appBGChan     :: TChan BgPacket
    }

-- Instances for our environment to properly serve the library
-- functions.

instance HasSerialPort App where
    getSerialPort = appSerialPort

instance HasBGChan App where
    getBGChan = appBGChan
    updateBGChan chan app = app{ appBGChan = chan }

instance HasDebug App where
    getDebug = appOptDebug . appOptions

-- Command line parser
optParser :: Parser AppOptions
optParser = AppOptions
        <$> argument str 
            (  metavar "PORT"
            <> help "Serial port" )
        <*> switch
            ( long "debug"
            <> short 'd'
            <> help "Whether to be quiet" )

-- Takes an environment and runs a program with it inside the IO monad.
execApp :: env -> AppM env a -> IO a
execApp = flip runReaderT

-- RUns a program in a new thread inside out AppM stack
forkApp :: AppM env () -> AppM env ThreadId
forkApp act = do
    env <- ask
    liftIO $ forkIO $ execApp env act

-- Can be used to wait for an event handler to return a value.
-- Waiting for a specific BLE device advertisement to appear for
-- example. Timeout is in microseconds.
withTimeOut :: Int -> AppM env a -> AppM env (Maybe a)
withTimeOut t a = do
    env <- ask
    res <- liftIO $ race (threadDelay t) (execApp env a)
    return $ case res of
        Left () -> Nothing
        Right x -> Just x

-- A few lifted functions

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . P.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . P.print

main :: IO ()
main = do
    -- Run the command line parser
    appOpts <- execParser $
        info
            ( optParser <**> helper )
            (  fullDesc
            <> progDesc "Execute a short battery of test on port PORT"
            <> header "bgapitest - a short text / example for haskell-bglib"
            )

    -- Build the application environment
    -- With certain hardware, you probably want to set baud rate,
    -- stop and parity bits. You can extract the file descriptor
    -- from the Handle and use System.Posix.Terminal functions
    -- to do that, or use the Serial library to open a serial port
    -- and build a Handle out of the file descriptor of the serial
    -- port.
    app <- App
        <$> return appOpts
        <*> openSerial (appOptSerialPort appOpts) defaultSerialSettings { commSpeed = CS115200 }
        <*> atomically newBroadcastTChan

    -- Run the application
    execApp app $ do

        -- Register an event handler for protocol errors.
        -- Event handlers are blocking. We use forkApp to make it
        -- "run in the background".
        -- The command 'packetBlock' creates a barrier, so new packets
        -- after this are guaranteed to be picked up by the event handlers
        -- in the block.
        packetBlock_ $ forkApp $ evtSystemProtocolError $ \reason -> do
            liftIO $ die $ "*** PROTOCOL ERROR " ++ show reason

        -- Starts a thread that keeps reading packets from the serial port,
        -- pushing them to the broadcast TChan
        startPacketReader $ \err -> fail err

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
        
        packetBlock_ $ do
            _ <- gapDiscover GapDiscoverGeneric

            -- Register an event handler for scan responses. Can be done anywhere.
            -- The handler forks a thread that runs forever, and can be terminated
            -- later if necessary.
            _ <- withTimeOut 5000000 $ evtGapScanResponse $ \rssi _ sender _ _ _ -> do
                    print rssi
                    print sender
                    putStrLn ""
                    return $ Nothing -- We'd like to listen to further events.

            gapEndProcedure

        putStrLn "Let's cause trouble:"
        h <- askSerialPort
        _ <- liftIO $ send h "a"
        liftIO $ threadDelay 2000000
