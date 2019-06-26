program debugwire_cli;

uses
  crt, sysutils, debugwire,
  binutils, Classes, getopts, genutils;

var
  DW: TDebugWire;
  data: TBytes;

procedure cmdlineHelp;
begin
  WriteLn('Usage:');
  WriteLn('dw_gdb_cli [-S <sp>] [-B <bd>] [-T <tp>] [-H]');
  WriteLn('');
  WriteLn('-S <sp>, --serialport=<sp>');
  {$ifdef Windows}
  WriteLn('Connect to serial port <sp>, e.g. COM3 or \\.\COM13');
  {$else}
  WriteLn('Connect to serial port <sp>, e.g. /dev/ttyUSB0');
  {$endif}
  WriteLn('');
  WriteLn('-B <bd>, --baud=<bd>');
  WriteLn('Connect to serial port using baud rate <bd>.  If not specified, the debugWIRE baud rate will be scanned automatically.');
  WriteLn('');
  WriteLn('-T <tp>, --tcpport=<tp>');
  WriteLn('Set GDB server to listen on TCP port <tp>.  If not specified, TCP port defaults to 2345.');
  WriteLn('');
  WriteLn('-I, -i, --ispenable');
  WriteLn('Temporarily disable DWEN fuse to enable ISP functionality.');
  WriteLn('');
  WriteLn('-H, -h, -?, --help');
  WriteLn('Display this help');
end;

procedure runCmdHelp;
begin
  WriteLn('  b <val>           - Set break point at <val> (each value overwrites previous value). Negative value means no break point.');
  WriteLn('  d <start> <count> - Dump <count> bytes of data starting at <start> address. Addresses start at registers, then I/O registers, then SRAM.');
  WriteLn('  f <start> <count> - Dump <count> bytes of flash starting at <start> address.');
  WriteLn('  h, ?              - Show this help');
  WriteLn('  l <filename.hex>  - Load hex file to controller');
  WriteLn('  r                 - Run controller');
  WriteLn('  s                 - Step single instruction');
  WriteLn('  q                 - Quit');
  WriteLn('  x                 - Interrupt running controller');
end;

procedure DumpSRAM(start, count: word);
begin
  DW.ReadAddress(start + $60, count, data);  // offset after regs & IOregs
  WriteLn('ReadAddress:');
  WriteLn(FormatDataWithIndex(data, start+$60, 32));
end;

procedure DumpFlash(start, count: word);
begin
  DW.ReadFlash(start, count, data);  // offset after regs & IOregs
  //WriteLn('ReadFlash:');
  WriteLn(FormatDataWithIndex(data, start, 32));
end;

procedure printDebugState;
begin
  WriteLn('PC: $', hexStr(DW.PC, 4));
  WriteLn('Flash just after PC:');
  DW.ReadFlash(DW.PC, 16, data);  // offset after regs & IOregs
  WriteLn(FormatDataWithIndex(data, DW.PC, 4));
  //WriteLn;
  WriteLn('Registers:');
  DW.ReadAddress(0, 32, data);
  WriteLn(PrintRegs(data, 0));
  //WriteLn;
end;

var
  opts: array [0..4] of TOption;
  c: char;
  baud: integer = 0;
  serialPort: string;
  DisableDWENfuse: boolean = false;
  ID, err, startID, count: integer;
  b: byte;
  isRunning: boolean = false;
  s, s1, s2: string;

begin
  DW := TDebugWire.Create;

  opts[0].Flag := nil;
  opts[0].Has_arg := 1;
  opts[0].Name := 'serialport';
  opts[0].Value := 'S';

  opts[1].Flag := nil;
  opts[1].Has_arg := 1;
  opts[1].Name := 'baud';
  opts[1].Value := 'B';

  opts[2].Flag := nil;
  opts[2].Has_arg := 0;
  opts[2].Name := 'help';
  opts[2].Value := 'H';

  opts[3].Flag := nil;
  opts[3].Has_arg := 0;
  opts[3].Name := 'ispenable';
  opts[3].Value := 'I';

  opts[4].Flag := nil;
  opts[4].Has_arg := 0;
  opts[4].Name := '';
  opts[4].Value := #0;

  repeat
    c := GetLongOpts('S:s:B:b:Hh?Ii', @opts[0], ID);
    case c of
      'S','s': serialPort := OptArg;
      'B', 'b': baud := StrToInt(OptArg);
      'H', 'h', '?':
        begin
          cmdlineHelp;
          Halt;
        end;
      'I', 'i':  DisableDWENfuse := true;
    end;
  until c = EndOfOptions;

  if serialPort = '' then
  begin
    cmdlineHelp;
    halt(-1);
  end
  else
  begin
    DW := TDebugWire.Create();

    if not DW.Connect(serialPort, baud) then halt(-1);
    DW.BreakCmd;
    if not DW.IdentifyTarget then halt(-1);
    WriteLn('Device ID: $', hexStr(DW.Device.ID, 4), ' - ', DW.Device.name);
    DW.Reset;   // reset MCU
    DW.PC := 0;

    DW.ReadConfig(2, b);
    Write('efuse: $', hexStr(b, 2), ' ');
    DW.ReadConfig(3, b);
    Write('hfuse: $', hexStr(b, 2), ' ');
    DW.ReadConfig(0, b);
    WriteLn('lfuse: $', hexStr(b, 2));
    DW.ReadConfig(1, b);
    WriteLn('Lock bits: $', hexStr(b, 2));

    runCmdHelp;
    repeat
      if crt.keypressed then
        system.Read(c)
      else
        c := #0;
      case c of
        'b': if not isRunning then
             begin
               system.ReadLn(s);
               Val(s, ID, err);
               if err = 0 then
                 DW.BP := ID
               else
                 WriteLn('Invalid <val>');
             end;
        'd': if not isRunning then
             begin
               // format: d <start> <count>
               system.ReadLn(s);
               s := trim(s);
               ID := pos(' ', s);
               s1 := copy(s, 1, ID-1);
               s2 := copy(s, ID, length(s));
               Val(s1, startID, err);
               if err = 0 then
                 Val(s2, count, err);
               if err = 0 then
               begin
                 DW.ReadAddress(startID, count, data);  // offset after regs & IOregs
                 WriteLn(FormatDataWithIndex(data, startID, 32));
               end
               else
               begin
                 WriteLn('Invalid format for command <d>. Example:');
                 WriteLn('d $100 $20');
               end;
             end;
        'f': if not isRunning then
             begin
               // format: f <start> <count>
               system.ReadLn(s);
               s := trim(s);
               ID := pos(' ', s);
               s1 := copy(s, 1, ID-1);
               s2 := copy(s, ID, length(s));
               Val(s1, startID, err);
               if err = 0 then
                 Val(s2, count, err);
               if err = 0 then
               begin
                 DW.ReadFlash(startID, count, data);
                 WriteLn(FormatDataWithIndex(data, startID, 32));
               end
               else
               begin
                 WriteLn('Invalid format for command <f>. Example:');
                 WriteLn('f $100 $20');
               end;
             end;
        'h', '?': runCmdHelp;
        'l': if not isRunning then
             begin
               system.ReadLn(s);
               s := trim(s);
               if FileExists(s) then
               begin
                 programHexFile(s, DW);
                 DW.Reset;
                 DW.Reconnect;
                 DW.PC := 0;
               end
               else
                 WriteLn('Invalid file name - file doesn''t exist')
             end;
        'r': if not isRunning then
             begin
               isRunning := true;
               WriteLn('Running...');
               DW.Run;
             end;
        's': if not isRunning then
             begin
               DW.Step;
               printDebugState;
             end;
        'x': if isRunning then
             begin
               DW.BreakCmd;
               printDebugState;
               isRunning := false;
             end;
      end;

      if isRunning then
        if DW.TrySync then
        begin
          DW.Reconnect;
          printDebugState;
          isRunning := false;
        end;

    until c = 'q';
  end;

  if Assigned(DW) then
  begin
    DW.BP := -1;
    DW.Run;
    DW.Free;
  end;

  exit;

  WriteLn('Running until address $9E.');
  DW.Reset;
  DW.PC := 0;
  DW.BP := $9E;  // MAIN
  DW.ContinueUntilBreak;
  WriteLn('Hit break point at: $', hexStr(DW.PC, 2));
  WriteLn('Hit any key to step through code, space to stop');
  WriteLn('PC   Instruction code');
  repeat
    DW.ReadFlash(DW.PC, 4, data);
    WriteLn(format('%.4X %.2X %.2X %.2X %.2X', [DW.PC, data[0], data[1], data[2], data[3]]));
    DW.Step;
    //c := ReadKey();
  until (c = ' ');

  //DW.Run;
  //SetLength(data, 2);
  //data[0] := 1;
  //data[1] := 2;
  //DW.WriteRegs(0, data);
  //WriteLn('Write to regs..');
  //
  //sleep(1000);
  //DW.ReadRegs(0, 32, data);
  //
  //for i := 0 to length(data)-1 do
  //begin
  //  Write('R', i:2, ' - $', hexStr(data[i],2));
  //  if (i mod 4) = 3 then
  //    Write(#10)
  //  else
  //    Write(#9);
  //end;

  // Skip registers and io registers
  //DW.ReadFlash(0, 240, data);

//  DW.ReadAddress(96, 8, data);
  //WriteLn('First 8 bytes from SRAM:');

  //DW.ReadFlash(0, 248, data);
  //WriteLn('First ', length(data), ' bytes from FLASH:');
  //for i := 0 to length(data)-1 do
  //begin
  //  if (i mod 4) = 0 then
  //  begin
  //    Write('$', hexStr(i, 4), ': ', hexStr(data[i],2));
  //  end
  //  else if (i mod 4) = 3 then
  //  begin
  //    Write(' ', hexStr(data[i],2), #13);
  //  end
  //  else
  //    Write(' ', hexStr(data[i],2));
  //end;


  // Working
  //addr := $16 + 32;
  //DW.ReadAddress(addr, 4, data);
  //WriteLn('ReadAddress:');
  //for i := 0 to length(data)-1 do
  //begin
  //  if (i mod 4) = 0 then
  //  begin
  //    Write('$', hexStr(i+addr, 4), ': ', hexStr(data[i],2));
  //  end
  //  else if (i mod 4) = 3 then
  //  begin
  //    Write(' ', hexStr(data[i],2), #13);
  //  end
  //  else
  //    Write(' ', hexStr(data[i],2));
  //end;

  //for i := 0 to length(data)-1 do
  //begin
  //  Write('$', hexStr(data[i],2));
  //  if (i mod 4) = 3 then
  //    Write(#10,i+96:3, ': ')
  //  else
  //    Write(' ');
  //end;

  //DW.ReadAddress(60, 60, data);
  //WriteLn('ReadAddress:');
  //for i := 0 to length(data)-1 do
  //begin
  //  Write('$', hexStr(data[i],2));
  //  if (i mod 4) = 3 then
  //    Write(#10,i:3, ': ')
  //  else
  //    Write(' ');
  //end;

  //DW.SetPC(0);
  //
  //DW.ReadFlash(DW.PC, 4, data);
  //WriteLn('First 4 bytes from flash starting at PC:');
  //for i := 0 to length(data)-1 do
  //begin
  //  Write('$', hexStr(data[i],2));
  //  if (i mod 4) = 3 then
  //    Write(#10)
  //  else
  //    Write(' ');
  //end;
  //Write(#13);

  DW.TimersDisabled := false;
  DW.Run;

  DW.Free;
end.
