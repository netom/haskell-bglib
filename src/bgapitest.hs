import           Commands
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSS
import           System.Exit
import           System.Hardware.Serialport
import           Types

data App = App
    { appSerialPort :: SerialPort
    , appBGChan :: TChan BgPacket
    }

instance HasSerialPort App where
    getSerialPort = appSerialPort

instance HasBGChan App where
    getBGChan = appBGChan

execApp = flip runReaderT

main = do
    let port = "/dev/ttyACM3"

    app <- App
        <$> ( openSerial port $ SerialPortSettings CS115200 8 One NoParity NoFlowControl 1000 )
        <*> ( atomically newBroadcastTChan )

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
        liftIO $ putStrLn $ "Encrypted: " ++ prettyShowBS (fromUInt8Array encrypted)
        liftIO $ putStrLn ""

        liftIO $ putStrLn $ "Decrypting"
        decrypted <- systemAesDecrypt encrypted
        liftIO $ putStrLn $ "Decrypted: " ++ BSS.unpack (fromUInt8Array decrypted)
        liftIO $ putStrLn ""

        -- Register an event handler for scan responses. Can be done anywhere.
        -- The handler forks a thread that runs forever, and can be terminated
        -- later if necessary.
        tid <- evtGapScanResponse $ \(rssi, packet_type, sender, address_type, bond, dta) -> do
            print rssi
            print sender
            putStrLn ""
            return True -- We'd like to listen to further events.

        gapDiscover GapDiscoverGeneric

        liftIO $ threadDelay 5000000

        gapEndProcedure

        liftIO $ killThread tid

        -- Let's cause trouble.
        s <- askSerialPort
        liftIO $ send s $ BSS.pack "a"
        liftIO $ threadDelay 5000000
