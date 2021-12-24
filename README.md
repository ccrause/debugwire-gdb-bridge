# debugwire-gdb-bridge
## Work in progress...
This is a Pascal implementation of the debugwire protocol for small AVR controllers such as attiny85 or atmega328P. It includes a gdb remote serial protocol implementation so that avr-gdb can communicate with the debugwire code to debug on chip code. The aim is to focus on features that would enable one to use Lazarus to both write code for and debug the generated code on compatible AVR microcontrollers.

Extensively tested on Linux with the following USB-serial converters: FTDI's FT232RL and CP2102. The CH340 USB-serial converter is also [reported to work](https://github.com/dcwbrown/dwire-debug).  Basically any USB-serial converter that supports custom baud rates and either the break serial command or can send a clean binary zero byte at low baud should work. Note that currently there is an issue detecting the baud rate of an attiny85 running at 1 MHz, i.e. when the expected baud rate is around 7812, using a CP2102.

Based on the following sources:  
http://www.ruemohr.org/docs/debugwire.html  
https://github.com/dcwbrown/dwire-debug  
https://github.com/mvirkkunen/dwprog  
https://github.com/jbtronics/WireDebugger  

## TODO:
* ~~Add breakpoint manager for more than one breakpoint.~~
  * ~~Execute stored instruction when continuing after break - SW BP disabled for testing.~~
  * ~~When deleting a SW BP, restore original instruction (flash write). (disabled while testing)~~
  * ~~During flash read/write check if inactive SW BP may still have BREAK opcode in flash - if so handle specific opcode in BP manager.~~
  * ~~Put first BP in HW~~ Done
  * ~~Further BP in list, with corresponding opcode replaced with break (flash write). Store opcode in manager.~~ Done
  * ~~If HW BP deleted, keep HW BP open until next BP is set to minimize flash rewrites.~~ Done
* Scan (BreakResponse) candidate ports automatically - perhaps check port type and scan if a real serial port (hardware or usb) and can be opened exlusively. See this [link](https://stackoverflow.com/a/1394301) for Windows and this [link](https://stackoverflow.com/questions/2530096/how-to-find-all-serial-devices-ttys-ttyusb-on-linux-without-opening-them) for Linux.
* Add elf reader - long term, low priority.
* ~~Incorporate notes from https://www.embecosm.com/appnotes/ean4/embecosm-howto-rsp-server-ean4-issue-2.html#id3077923.~~
* ~~Add Windows support for serial functions and network socket.~~ Not well tested yet...
