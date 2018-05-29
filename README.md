# debugwire-gdb-bridge
## Work in progress...
This is a Pascal implementation of the debugwire protocol for small AVR controllers such as attiny85 or atmega328P. It includes a gdb remote serial protocol implementation so that avr-gdb can communicate with the debugwire code to debug on chip code. The aim is to focus on features that would enable one to use Lazarus to both write code for and debug the generated code on compatible AVR microcontrollers.

Based on the following sources:  
http://www.ruemohr.org/docs/debugwire.html  
https://github.com/dcwbrown/dwire-debug  
https://github.com/mvirkkunen/dwprog  
https://github.com/jbtronics/WireDebugger  

## TODO:
* Incorporate notes from https://www.embecosm.com/appnotes/ean4/embecosm-howto-rsp-server-ean4-issue-2.html#id3077923.
* Add Windows support for serial functions and network socket.
* Add elf reader - long term. Convenience.
* Add breakpoint manager for more than one breakpoint. gdb by default seem to only insert software breaks into flash.  This works, but causes flash wear.  Perhaps manage break points on internally to ensure that the single hardware break point is also utilized?
Idea:
  * ~~Put first BP in HW~~ Done
  * ~~Further BP in list, with corresponding opcode replaced with break (flash write). Store opcode in manager.~~ Done
  * Execute stored instruction when continuing after break.
  * ~~If HW BP deleted, pop next BP from manager. Or keep HW BP open until next BP is set to minimize flash rewrites.~~ Done
  * When deleting a SW BP, restore original instruction (flash write). (disabled while testing)
  * What will happen if server crashes, or hardware is switched off during debug session with active SW BP's?
