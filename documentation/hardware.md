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

## Suitable serial drivers/converters
I've extensively tested both FTDI's FT232 and the CP2102 USB-serial converters.
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
