# Activating DWEN
Start by inspecting current fuse settings:  
```bash
$ avrdude -P /dev/ttyACM1 -c avrisp2 -p m328p
avrdude: AVR device initialized and ready to accept instructions
Reading | ################################################## | 100% 0.00s
avrdude: Device signature = 0x1e950f (probably m328p)
avrdude: safemode: Fuses OK (E:FF, H:D6, L:E2)
avrdude done.  Thank you.
```

Set DWEN fuse (HFUSE: DF -> 9F):  
```bash
$ avrdude -P /dev/ttyACM1 -c avrisp2 -p m328p -U hfuse:w:0x96:m
avrdude: AVR device initialized and ready to accept instructions
Reading | ################################################## | 100% 0.00s
avrdude: Device signature = 0x1e950f (probably m328p)
avrdude: reading input file "0x96"
avrdude: writing hfuse (1 bytes):
Writing | ################################################## | 100% 0.01s
avrdude: 1 bytes of hfuse written
avrdude: verifying hfuse memory against 0x96:
avrdude: load data hfuse data from input file 0x96:
avrdude: input file 0x96 contains 1 bytes
avrdude: reading on-chip hfuse data:
Reading | ################################################## | 100% 0.00s
avrdude: verifying ...
avrdude: 1 bytes of hfuse verified
avrdude: safemode: Fuses OK (E:FF, H:96, L:E2)
avrdude done.  Thank you.
```

Check if ISP works:  
```bash
$ avrdude -P /dev/ttyACM1 -c avrisp2 -p m328p
avrdude: stk500v2_command(): command failed
avrdude: initialization failed, rc=-1
         Double check connections and try again, or use -F to override
         this check.
avrdude done.  Thank you.
```

Now start dw_gdb:  
```bash
$ ./dw_gdb -s /dev/ttyUSB0 -v
13:24:12.518  BAUD: 200000. Data: 56. Bin: 00111000
13:24:12.518  Scale = 70%
13:24:12.524  BAUD: 140000. Data: 206. Bin: 11001110
13:24:12.524  Scale = 80%
13:24:12.526  BAUD: 112000. Data: 38. Bin: 00100110
13:24:12.526  Scale = 85%
13:24:12.528  BAUD: 95200. Data: 38. Bin: 00100110
13:24:12.528  Scale = 85%
13:24:12.531  BAUD: 80920. Data: 38. Bin: 00100110
13:24:12.531  Scale = 85%
13:24:12.533  BAUD: 68782. Data: 41. Bin: 00101001
13:24:12.533  Scale = 90%
13:24:12.536  BAUD: 61903. Data: 85. Bin: 01010101
13:24:12.536  Success, BAUD = 61903
13:24:12.536  Scanning for upper bound...
13:24:12.538  BAUD: 63141. Data: 85. Bin: 01010101
13:24:12.541  BAUD: 64403. Data: 41. Bin: 00101001
13:24:12.541  Scanning for lower bound...
13:24:12.543  BAUD: 60689. Data: 85. Bin: 01010101
13:24:12.546  BAUD: 59499. Data: 85. Bin: 01010101
13:24:12.548  BAUD: 58332. Data: 85. Bin: 01010101
13:24:12.550  BAUD: 57188. Data: 181. Bin: 10110101
13:24:12.551  BAUD estimate = 60736
13:24:12.554  Device ID: $950F [ATmega328P]
13:24:12.629  PC: $7E00
13:24:12.632  R28: $E0, R29: $08, R30: $10, R31: $02
Start accepting...
13:24:41.716  Accepting new connection
```
# Deactivating DWEN
Temporarily disable DWEN:  
```bash
$ ./dw_gdb -s /dev/ttyUSB0 -b 62000 -i
DWEN temporarily disabled until power to controller is cycled.
Connect ISP now to change fuses.
```

Reconnect ISP and test ISP connection:  
```bash
$ avrdude -c usbasp -p m328p -v
avrdude: AVR device initialized and ready to accept instructions
Reading | ################################################## | 100% 0.00s
avrdude: Device signature = 0x1e950f (probably m328p)
avrdude: safemode: hfuse reads as 96
avrdude: safemode: efuse reads as FF
avrdude: safemode: hfuse reads as 96
avrdude: safemode: efuse reads as FF
avrdude: safemode: Fuses OK (E:FF, H:96, L:E2)
avrdude done.  Thank you.
```

One can now either reprogram the DWEN fuse and continue using ISP, or cycle power to controller and continue using dw_gdb.
