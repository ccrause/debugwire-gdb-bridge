# Activating DWEN
Start by inspecting current fuse settings:  
```bash
~/LazProjs/debugwire-gdb-bridge $ avrdude -P /dev/ttyACM0 -c avrisp2 -p t24  
avrdude: AVR device initialized and ready to accept instructions  
Reading | ################################################## | 100% 0.00s  
avrdude: Device signature = 0x1e910b (probably t24)  
avrdude: safemode: Fuses OK (E:FF, H:DF, L:E2)  
avrdude done.  Thank you.
``` 

Set DWEN fuse (HFUSE: DF -> 9F):  
```~/LazProjs/debugwire-gdb-bridge $ avrdude -P /dev/ttyACM0 -c avrisp2 -p t24 -U hfuse:w:0x9f:m  
avrdude: AVR device initialized and ready to accept instructions  
Reading | ################################################## | 100% 0.00s  
avrdude: Device signature = 0x1e910b (probably t24)  
avrdude: reading input file "0x9f"  
avrdude: writing hfuse (1 bytes):  
Writing | ################################################## | 100% 0.01s  
avrdude: 1 bytes of hfuse written  
avrdude: verifying hfuse memory against 0x9f:  
avrdude: load data hfuse data from input file 0x9f:  
avrdude: input file 0x9f contains 1 bytes  
avrdude: reading on-chip hfuse data:  
Reading | ################################################## | 100% 0.00s  
avrdude: verifying ...  
avrdude: 1 bytes of hfuse verified  
avrdude: safemode: Fuses OK (E:FF, H:9F, L:E2)  
avrdude done.  Thank you.
```

Check if ISP works:  
```bash
~/LazProjs/debugwire-gdb-bridge $ avrdude -P /dev/ttyACM0 -c avrisp2 -p t24   
avrdude: stk500v2_command(): command failed  
avrdude: initialization failed, rc=-1  
         Double check connections and try again, or use -F to override  
         this check.  
avrdude done.  Thank you.
```

Now start dw_gdb:  
```bash
~/LazProjs/debugwire-gdb-bridge $ ./dw_gdb -s /dev/ttyUSB0   
11:00:48.682  BAUD: 200000. Data: 56. Bin: 00111000  
11:00:48.682  Scale = 70%  
11:00:48.685  BAUD: 140000. Data: 206. Bin: 11001110  
11:00:48.685  Scale = 80%  
11:00:48.695  BAUD: 112000. Data: 54. Bin: 00110110  
11:00:48.695  Scale = 85%  
11:00:48.697  BAUD: 95200. Data: 38. Bin: 00100110  
11:00:48.697  Scale = 85%  
11:00:48.700  BAUD: 80920. Data: 38. Bin: 00100110  
11:00:48.700  Scale = 85%  
11:00:48.702  BAUD: 68782. Data: 41. Bin: 00101001  
11:00:48.702  Scale = 90%  
11:00:48.704  BAUD: 61903. Data: 85. Bin: 01010101  
11:00:48.704  Success, BAUD = 61903  
11:00:48.704  Scanning for upper bound...  
11:00:48.707  BAUD: 63141. Data: 85. Bin: 01010101  
11:00:48.709  BAUD: 64403. Data: 41. Bin: 00101001  
11:00:48.709  Scanning for lower bound...  
11:00:48.712  BAUD: 60689. Data: 85. Bin: 01010101  
11:00:48.714  BAUD: 59499. Data: 85. Bin: 01010101  
11:00:48.717  BAUD: 58332. Data: 85. Bin: 01010101  
11:00:48.719  BAUD: 57188. Data: 165. Bin: 10100101  
11:00:48.719  BAUD estimate = 60736  
11:00:48.723  Device ID: $910B [ATtiny24]  
11:00:48.793  PC: $0000  
11:00:48.796  R28: $D5, R29: $00, R30: $9C, R31: $00  
Start accepting...
```

Connect to dw_gdb using gdb compiled for AVR target:  
```bash
(gdb) target remote :2345  
Remote debugging using :2345  
warning: No executable has been specified and target does not support  
determining executable automatically.  Try using the "file" command.  
0x00000000 in ?? ()  
11:01:12.967  Accepting new connection  
11:01:12.967  Incoming connection from  
11:01:12.967  -> +$qSupported:multiprocess+;swbreak+;hwbreak+;qRelocInsn+;fork-events+;vfork-events+;exec-events+;vContSupported+;QThreadEvents+;no-resumed+#df  
11:01:12.967  <- +  
11:01:12.967  <- $hwbreak+;swbreak+#64  
11:01:12.968  -> +$vMustReplyEmpty#3a  
11:01:12.968  <- +  
11:01:12.968  <- $#00  
11:01:13.008  -> +  
11:01:13.008  -> $Hg0#df  
11:01:13.008  <- +  
11:01:13.008  <- $OK#9A  
11:01:13.052  -> +  
11:01:13.052  -> $qTStatus#49  
11:01:13.052  <- +  
11:01:13.053  <- $#00  
11:01:13.096  -> +  
11:01:13.096  -> $?#3f  
11:01:13.096  <- +  
11:01:13.114  <-  $T05hwbreak00:50;01:00;02:FF;03:FF;04:BF;05:FF;06:FF;07:FD;08:5F;09:CF;0A:FD;0B:7C;0C:BE;0D:16;0E:C8;0F:00;10:0A;11:00;12:D6;13:00;14:0A;15:00;16:00;17:00;18:47;19:48;1A:D6;1B:00;1C:D5;1D:00;1E:9C;1F:00;20:00;21:DF00;22:00000000;#5D
11:01:13.140  -> +$qfThreadInfo#bb  
11:01:13.140  <- +  
11:01:13.140  <- $#00  
11:01:13.184  -> +$qL1160000000000000000#55  
11:01:13.184  <- +  
11:01:13.184  <- $#00  
11:01:13.228  -> +$Hc-1#09  
11:01:13.228  <- +  
11:01:13.228  <- $OK#9A  
11:01:13.272  -> +$qC#b4  
11:01:13.272  <- +  
11:01:13.272  <- $#00  
11:01:13.316  -> +$qAttached#8f  
11:01:13.316  <- +  
11:01:13.316  <- $#00  
11:01:13.364  -> +$g#67  
11:01:13.364  <- +  
11:01:13.381  <- $5000FFFFBFFFFFFD5FCFFD7CBE16C8000A00D6000A0000004748D600D5005F0000DF0000000000#43  
11:01:13.412  -> +$qL1160000000000000000#55  
11:01:13.412  <- +  
11:01:13.412  <- $#00  
11:01:13.456  -> +
```

```bash
(gdb) detach  
Detaching from program: , Remote target  
Ending remote debugging.
```

```bash
11:01:26.887  -> $D#44  
11:01:26.888  <- +  
11:01:26.888  <- $OK#9A  
11:01:26.891  FActiveThreadOnTerminate
```

# Deactivating DWEN
Temporarily disable DWEN:  
```bash
~/LazProjs/debugwire-gdb-bridge $ ./dw_gdb -s /dev/ttyUSB0  -b 62500 -i  
DWEN temporarily disabled until power to controller is cycled.  
Connect ISP now to change fuses.
```

Reconnect ISP and test ISP connection:  
```bash
~/LazProjs/debugwire-gdb-bridge $ avrdude -P /dev/ttyACM0 -c avrisp2 -p t24   
avrdude: AVR device initialized and ready to accept instructions  
Reading | ################################################## | 100% 0.00s  
avrdude: Device signature = 0x1e910b (probably t24)  
avrdude: safemode: Fuses OK (E:FF, H:9F, L:E2)  
avrdude done.  Thank you.
```

One can now either reprogram the DWEN fuse and continue using ISP, or cycle power to controller and continue using dw_gdb.
