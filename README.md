# debugwire-gdb-bridge
Work in progress...

Based on the following sources:
http://www.ruemohr.org/docs/debugwire.html
https://github.com/dcwbrown/dwire-debug
https://github.com/mvirkkunen/dwprog
https://github.com/jbtronics/WireDebugger

## TODO:
Add Windows support for serial functions and network socket.
Add elf reader - long term. Convenience.
Add breakpoint manager for more than one breakpoint. gdb by default seem to insert software breaks into flash.  This works, but causes flash wear.  Perhaps manage break points on internally to ensure that the single hardware break point is alos utilized?
Idea:
- Put first BP in HW
- Further BP in list, with corresponding opcode replaced with break (flash write). Store opcode in manager.
- Execute stored instruction when continuing after break.
- If HW BP deleted, pop next BP from manager. Or keep HW BP open until next BP is set to minimize flash rewrites.
- When deleting a SW BP, restore original instruction (flash write).
- What will happen if server crashes, or hardware is switched off during debug session with active SW BP's?
