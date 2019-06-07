import           Commands
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSS
import           System.Hardware.Serialport
import           Types

data App = App {
    appSerialPort :: SerialPort
}

instance HasSerialPort App where
    getSerialPort = appSerialPort

main = do
    let port = "/dev/ttyACM3"

    s <- openSerial port $  SerialPortSettings CS115200 8 One NoParity NoFlowControl 1000

    (flip runReaderT) (App s) $ do
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

    closeSerial s
