# debugwire-gdb-bridge
## About
This is a Pascal implementation of the debugwire protocol for small AVR controllers such as attiny85 or atmega328P. It includes a gdb remote serial protocol server so that a debugger such as gdb or [LazDebuggerFpRspRemote](https://wiki.freepascal.org/LazDebuggerFpRspRemote) in Lazarus can communicate with the debugwire implementation to debug on-chip code. The aim is to focus on features that would enable one to use Lazarus to both write code for and debug the generated code on compatible AVR microcontrollers.

A simple USB-serial adapter can be used to interface with hardware, see [hardware documentation](documentation/hardware.md).

## Usage
Run the _dw_gdb_ executable and a serial port (mandatory) to connect to. It will connect to the serial port and if successfull, open a TCP port (default 1234) and wait for a TCP connection from a debugger. A baud rate can be specified for the serial connection - if not a baud rate scan will be done. The DWEN fuse can be diabled temporarily to enable ISP programming. The general syntax and command line options supported are:
```bash
dw_gdb -S <sp> [-B <bd>] [-T <tp>] [-I] [-V] [-H]

-S <sp>, -s <sp>, --serialport=<sp>
Connect to serial port <sp>, e.g. /dev/ttyUSB0

-B <bd>, -b <bd>, --baud=<bd>
Connect to serial port using baud rate <bd>.  If not specified, the debugWIRE baud rate will be scanned automatically.

-T <tp>, -t <tp>, --tcpport=<tp>
Set TCP port <tp> for remote connection.  If not specified, TCP port defaults to 1234.

-I, -i, --ispenable
Temporarily disable DWEN fuse to enable ISP functionality and exit.

-V, -v, --verbose
Enable verbose debug output.

-H, -h, -?, --help
Display this help and exit.
```
Note that parameters for long options can be separated by a `=` or a space.

## References
Special thanks to RikusW and others for documenting the debugwire protocol:  
http://www.ruemohr.org/docs/debugwire.html  

Other debugwire implementations that in some way inspired this work:  
https://github.com/dcwbrown/dwire-debug  
https://github.com/mvirkkunen/dwprog  
https://github.com/jbtronics/WireDebugger  

## Todo list:
* Scan candidate serial ports automatically to detect a debugwire compatible device. This could potentially affect other serial connected hardware, so caution is required. See this [link](https://stackoverflow.com/a/1394301) for Windows and this [link](https://stackoverflow.com/questions/2530096/how-to-find-all-serial-devices-ttys-ttyusb-on-linux-without-opening-them) for Linux.
