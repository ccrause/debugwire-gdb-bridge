# Hardware
## Connection
DebugWIRE is a single wire serial like protocol that communicates with the microcontroller
 via the reset pin. The TX and RX connectors of the UART should be
 joined with either a diode or a resistor. The reset pin of the controller is
 directly connected to the RX line, while the reset pin is connected to the TX line via
 a diode (cathode to TX). The diode needs to have a [small foward voltage
 drop and low capacitance](https://www.avrfreaks.net/comment/2279996#comment-2279996)
 for short signal rise times. I've used a [1N4148](https://www.avrfreaks.net/comment/2279996#comment-2279996).
 [Others](https://github.com/dcwbrown/dwire-debug/issues/35) have
 have reported success by replacing the diode with a 1 - 4.7 kOhm resistor.  The value of
 this resistor will probably depend on the value of the reset pull-up resistor,
 the serial converter impedance and the operating voltage of the controller.

  Notes on the reset pin:
* The value of the pull-up resistor should not be less than 10 kOhm
* No capacitor should be connected - this means that e.g. Arduino boards with
 compatible controllers (Uno, Nano etc.) needs to be modified to cut the reset capacitor connection
[1](https://awtfy.com/2010/02/21/modify-an-arduino-for-debugwire/),
[2](https://sites.google.com/site/wayneholder/debugwire3).
## Suitable serial drivers/converters
I've extensively tested both FT232 and CP2102 USB-serial converters.
 Success using the [PL-2303HX](https://github.com/dcwbrown/dwire-debug/issues/37) and
 the [CH340](https://github.com/dcwbrown/dwire-debug) converters have also been reported elsewhere.
 Any serial device can be used as long as it (and the OS driver) provides the following functionality:
 * Support custom baud rates (debugWIRE defaults to a baud rate of MCU clock / 128)
 * Support for either issuing a serial break or sending #0 at low (c.a. 19200) baud.
 Sending a #0 further requires a driver to switch baud rates very fast to allow
 reading the response following the break command

## AVR microcontrollers
In principle this project should work on any debugWIRE compatiple controller,
 provided that the needed information is present in the DeviceInfo list in debugwire.pas.
 It has been tested extensively on an ATtiny45.

## Manage debugWIRE/ISP functionality
The debugWIRE functionality is disabled by default. Enable the DWEN fuse using ISP.
 Once the DWEN fuse is set, no more communication over ISP will be possible until
 DWEN is disabled. One should therefore set all relevant fuses over ISP before setting DWEN.
 Note that the SPIEN fuse should preferably be left enabled so that ISP will work if
 DWEN is disabled. To enable ISP mode, disable the DWEN fuse over debugWIRE,
 this will disable debugWIRE temporarily. Then without removing power to the
 controller, disable the DWEN fuse over ISP. Alternatively DWEN can be disabled
 using high voltage ISP.

 To disable DWEN, one can specify `-i` or `--ispenable` on the command line. Full example:  
  `dw_gdb s /dev/ttyUSB0 -b 62500 -i`
