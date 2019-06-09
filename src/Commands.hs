{-# LANGUAGE RecordWildCards #-}

module Commands where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.Loops
import           Control.Monad.STM
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import           System.Hardware.Serialport
import           System.IO
import           Types
import           Control.Monad.IO.Class
import           Control.Monad.Reader

-- Write a BgPacket to a SerialPort
writeBGPacket' :: SerialPort -> BgPacket -> IO ()
writeBGPacket' s p = do
    let packetBS = BSL.toStrict $ encode p
    --putStr "* PACKET: "
    --putStrLn $ prettyShowBS packetBS
    --putStrLn ""
    send s packetBS
    return ()

-- Write the BgPacket to the SerialPort in env asked from the MonadReader
writeBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => BgPacket -> m ()
writeBGPacket p = do
    s <- askSerialPort
    liftIO $ writeBGPacket' s p

-- Read one BgPacket from a SerialPort
readBGPacket' :: SerialPort -> IO BgPacket
readBGPacket' s = do
    bgpHeader@BgPacketHeader{..} <- decode <$> BSL.fromStrict <$> recv s 4
    bgpPayload <- recv s (fromIntegral bghLength)
    --putStr "* HEADER: "
    --putStrLn $ show bgpHeader
    --putStrLn "* PAYLOAD: "
    --putStrLn $ prettyShowBS bgpPayload
    --putStrLn ""
    return $ BgPacket {..}

-- Read one BgPacket from the SerialPort in env asked from the MonadReader
readBGPacket :: (MonadIO m, MonadReader env m, HasSerialPort env) => m BgPacket
readBGPacket = do
    s <- askSerialPort
    liftIO $ readBGPacket' s

-- Launch a thread that reads packets and sends them down a TChan BgPacket
startPacketReader :: (MonadIO m, MonadReader env m, HasBGChan env, HasSerialPort env) => m () 
startPacketReader = do
    c <- askBGChan
    s <- askSerialPort
    _ <- liftIO $ forkIO $ forever $ do
        --putStrLn "* READ *"
        readBGPacket' s >>= atomically . writeTChan c
    return ()

-- Waits for any BgPacket to appear on the TChan
waitForAnyPacket :: TChan BgPacket -> IO BgPacket
waitForAnyPacket chan = do
    liftIO $ atomically $ readTChan chan

-- Wait for a packet with specific values in it's header
waitForPacket :: TChan BgPacket -> BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> IO BgPacket
waitForPacket chan mt tt cc cid = do
    untilJust $ do
        p@BgPacket{..} <- waitForAnyPacket chan
        return $ if bgHeaderMatches mt tt cc cid bgpHeader then Just p else Nothing

-- eXecute a Command, don't wait for any answer
xCmd' :: (MonadIO m, MonadReader env m, HasSerialPort env, Binary a ) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m ()
xCmd' mt tt cc cid inp = do
    let inpBS = BSL.toStrict $ encode inp
    writeBGPacket $ BgPacket (BgPacketHeader mt tt (fromIntegral $ BSS.length inpBS) cc cid) inpBS

-- Execute a command, wait for the appropriate answer
xCmd :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env, Binary a, Binary b) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> a -> m b
xCmd mt tt cc cid inp = do
    -- We need to duplicate the channel BEFORE sending the command, so we don't miss the answer by accident
    chan <- askDupBGChan
    xCmd' mt tt cc cid inp
    decode . BSL.fromStrict . bgpPayload <$> ( liftIO $ waitForPacket chan mt tt cc cid )

register_event_handler :: Binary a => (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => BgMessageType -> BgTecnologyType -> BgCommandClass -> UInt8 -> (a -> IO Bool) -> m ThreadId
register_event_handler mt tt cc cid handler = do
    chan <- askDupBGChan
    liftIO $ forkIO $ go chan
    where
        go chan = do
            BgPacket{..} <- waitForPacket chan mt tt cc cid
            continue <- handler $ decode $ BSL.fromStrict $ bgpPayload
            if continue then go chan else return ()

-----------------------------------------------------------------------
-- Attribute Client
-----------------------------------------------------------------------

attclient_attribute_write = undefined
attclient_execute_write = undefined
attclient_find_by_type_value = undefined
attclient_find_information = undefined
attclient_indicate_confirm = undefined
attclient_prepare_write = undefined
attclient_read_by_group_type = undefined
attclient_read_by_handle = undefined
attclient_read_by_type = undefined
attclient_read_long = undefined
attclient_read_multiple = undefined
attclient_write_command = undefined

evt_attclient_attribute_value :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((UInt8, UInt16, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evt_attclient_attribute_value handler = register_event_handler BgMsgEvent BgBlue BgClsAttributeClient 0x05 handler

evt_attclient_find_information_found = undefined
evt_attclient_group_found = undefined
evt_attclient_indicated = undefined
evt_attclient_procedure_completed = undefined
evt_attclient_read_multiple_response = undefined

-----------------------------------------------------------------------
-- Attribute Database
-----------------------------------------------------------------------

attributes_read = undefined
attributes_read_type = undefined
attributes_send = undefined
attributes_user_read_response = undefined
attributes_user_write_response = undefined
attributes_write = undefined

evt_attributes_status = undefined
evt_attributes_user_read_request = undefined
evt_attributes_value = undefined

-----------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------

connection_channel_map_get = undefined
connection_channel_map_set = undefined
connection_disconnect = undefined
connection_get_rssi = undefined
connection_get_status = undefined
connection_slave_latency_disable = undefined
connection_update = undefined
connection_version_update = undefined

evt_connection_disconnected = undefined
evt_connection_feature_ind = undefined
evt_connection_status = undefined
evt_connection_version_ind = undefined

-----------------------------------------------------------------------
-- Generic Access Profile
-----------------------------------------------------------------------

gap_connect_direct = undefined
gap_connect_selective = undefined

-- This command starts the GAP discovery procedure to scan for advertising devices i.e. to perform a device
-- discovery.
-- Scanning parameters can be configured with the Set Scan Parameters command before issuing this command.
-- To cancel on an ongoing discovery process use the End Procedure command.
gap_discover :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => GapDiscoveryMode -> m UInt16
gap_discover mode = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x02 mode

-- This command ends the current GAP discovery procedure and stop the scanning of advertising devices.
gap_end_procedure :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m UInt16
gap_end_procedure = xCmd BgMsgCR BgBlue BgClsGenericAccessProfile 0x04 ()

gap_set_adv_data = undefined
gap_set_adv_parameters = undefined
gap_set_directed_connectable_mode = undefined
gap_set_filtering = undefined
gap_set_initiating_con_parameters = undefined
gap_set_mode = undefined
gap_set_nonresolvable_address = undefined
gap_set_privacy_flags = undefined
gap_set_scan_parameters = undefined

-- Register an event handler for GAP scan responses
evt_gap_scan_response :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => ((Int8, UInt8, BdAddr, UInt8, UInt8, UInt8Array) -> IO Bool) -> m ThreadId
evt_gap_scan_response handler = register_event_handler BgMsgEvent BgBlue BgClsGenericAccessProfile 0x00 handler

-----------------------------------------------------------------------
-- Hardware
-----------------------------------------------------------------------

hardware_adc_read = undefined
hardware_analog_comparator_config_irq = undefined
hardware_analog_comparator_enable = undefined
hardware_analog_comparator_read = undefined
hardware_get_timestamp = undefined
hardware_i2c_read = undefined
hardware_i2c_write = undefined
hardware_io_port_config_direction = undefined
hardware_io_port_config_function = undefined
hardware_io_port_config_pull = undefined
hardware_io_port_irq_direction = undefined
hardware_io_port_irq_enable = undefined
hardware_io_port_read = undefined
hardware_io_port_write = undefined
hardware_set_rxgain = undefined
hardware_set_soft_timer = undefined
hardware_set_txpower = undefined
hardware_sleep_enable = undefined
hardware_spi_config = undefined
hardware_spi_transfer = undefined
hardware_timer_comparator = undefined
hardware_usb_enable = undefined

evt_hardware_adc_result = undefined
evt_hardware_analog_comparator_status = undefined
evt_hardware_io_port_status = undefined
evt_hardware_soft_timer = undefined

-----------------------------------------------------------------------
-- Persistent Store
-----------------------------------------------------------------------

flash_erase_page = undefined
flash_ps_defrag = undefined
flash_ps_dump = undefined
flash_ps_erase_all = undefined
flash_ps_erase = undefined
flash_ps_load = undefined
flash_ps_save = undefined
flash_read_data = undefined
flash_write_data = undefined

evt_flash_ps_key = undefined

-----------------------------------------------------------------------
-- Security Manager
-----------------------------------------------------------------------

sm_delete_bonding = undefined
sm_encrypt_start = undefined
sm_get_bonds = undefined
sm_passkey_entry = undefined
set_bondable_mode = undefined
sm_set_oob_data = undefined
sm_set_pairing_distribution_keys = undefined
sm_set_parameters = undefined
sm_whitelist_bonds = undefined

evt_sm_bonding_fail = undefined
evt_sm_bond_status = undefined
evt_sm_passkey_display = undefined
evt_sm_passkey_request = undefined


-----------------------------------------------------------------------
-- System
-----------------------------------------------------------------------

-- This command reads the local devices public Bluetooth address.
system_address_get :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m BdAddr
system_address_get = xCmd BgMsgCR BgBlue BgClsSystem 0x02 ()

-- This command decrypts the given data using the AES algorithm with the predefined key set with command Aes
-- Setkey .
system_aes_decrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
system_aes_decrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x11 dta

-- This command encrypts the given data using the AES algorithm with the predefined with command Aes Setkey .
system_aes_encrypt :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m UInt8Array
system_aes_encrypt dta = xCmd BgMsgCR BgBlue BgClsSystem 0x10 dta

-- This command defines the encryption key that will be used with the AES encrypt and decrypt commands.
system_aes_setkey :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => UInt8Array -> m ()
system_aes_setkey key = xCmd BgMsgCR BgBlue BgClsSystem 0x0f key

system_delay_reset = undefined
system_endpoint_rx = undefined
system_endpoint_set_watermarks = undefined
system_endpoint_tx = undefined
system_get_bootloader_crc = undefined
system_get_connections = undefined
system_get_counters = undefined
system_get_info = undefined


-- This command can be used to test if the local device is functional. Similar to a typical "AT" -> "OK" test.
system_hello :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => m ()
system_hello = xCmd BgMsgCR BgBlue BgClsSystem 0x01 ()

-- This command resets the local device immediately. The command does not have a response.
system_reset :: (MonadIO m, MonadReader env m, HasSerialPort env) => RebootMode -> m ()
system_reset mode = xCmd' BgMsgCR BgBlue BgClsSystem 0x01 mode

system_usb_enumeration_status_get = undefined
system_whitelist_append = undefined
system_whitelist_clear = undefined
system_whitelist_remove = undefined

evt_system_boot = undefined
evt_system_endpoint_watermark_rx = undefined
evt_system_endpoint_watermark_tx = undefined
evt_system_no_license_key = undefined

-- Event handler for protocol errors
evt_system_protocol_error :: (MonadIO m, MonadReader env m, HasSerialPort env, HasBGChan env) => (UInt16 -> IO Bool) -> m ThreadId
evt_system_protocol_error handler = register_event_handler BgMsgEvent BgBlue BgClsSystem 0x06 handler

evt_system_script_failure = undefined
evt_system_usb_enumerated = undefined

-----------------------------------------------------------------------
-- Testing
-----------------------------------------------------------------------

test_channel_mode = undefined
test_get_channel_map = undefined
test_phy_end = undefined
test_phy_rx = undefined
test_phy_tx = undefined

-----------------------------------------------------------------------
-- Device Firmware Upgrade
-----------------------------------------------------------------------

dfu_flash_set_address = undefined
dfu_flash_upload = undefined
dfu_flash_upload_finish = undefined
dfu_reset = undefined

evt_dfu_boot = undefined
