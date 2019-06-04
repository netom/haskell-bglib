import           Commands
import           System.Hardware.Serialport
import           Types

main = do
    let port = "/dev/ttyACM3"

    s <- openSerial port $  SerialPortSettings CS115200 8 One NoParity NoFlowControl 1000

    cmd_system_address_get s >>= print

    closeSerial s
