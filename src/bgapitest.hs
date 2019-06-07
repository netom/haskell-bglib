import           Commands
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSS
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
        -- Starts a thread that keeps reading packets from the serial port,
        -- pushing them to the broadcast TChan
        startPacketReader

        liftIO $ putStrLn "Running hello"
        cmd_system_hello
        liftIO $ putStrLn ""

        liftIO $ putStrLn "Getting Bluetooth Address:"
        cmd_system_address_get >>= liftIO . print
        liftIO $ putStrLn ""

        let aeskey = "abcdefgh12345678"
        liftIO $ putStrLn $ "Setting AES key to " ++ aeskey
        cmd_system_aes_setkey $ toUInt8Array $ BSS.pack $ aeskey
        liftIO $ putStrLn ""

        let plaintext = "This is plain"
        liftIO $ putStrLn $ "Encrypting: " ++ plaintext
        encrypted <- cmd_system_aes_encrypt $ toUInt8Array $ BSS.pack $ plaintext
        liftIO $ putStrLn $ "Encrypted: " ++ prettyShowBS (fromUInt8Array encrypted)
        liftIO $ putStrLn ""

        liftIO $ putStrLn $ "Decrypting"
        decrypted <- cmd_system_aes_decrypt encrypted
        liftIO $ putStrLn $ "Decrypted: " ++ BSS.unpack (fromUInt8Array decrypted)
        liftIO $ putStrLn ""
        liftIO $ putStrLn "Test over."
