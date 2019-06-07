import           Commands
import           Control.Monad.IO.Class
import           Control.Monad.Reader
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
        cmd_system_address_get >>= liftIO . print

    closeSerial s
