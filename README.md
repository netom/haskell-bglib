# haskell-bglib

An implementation of the BGAPI serial protocol.

This protocol is spoken by Silicon Laboratories (formely BlueGiga)
Bluetooth and Wifi products via UART or USB.

The Bluetooth Smart Software API Reference Manual can be found at:

https://www.silabs.com/products/wireless/bluetooth/bluetooth-low-energy-modules/ble113-bluetooth-smart-module

The library works over a SerialPort, so the hardware need to be
connected over a standard port that is recognized by the operating
system.

