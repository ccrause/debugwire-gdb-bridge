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
  WriteLn('-L <filename.hex>, -l <filename.hex>, --loadfile <filename.hex>');
  WriteLn('UpLoad file <filename.hex> to controller and exit. Use as standalone programmer.');
  WriteLn('');
  WriteLn('-H, -h, -?, --help');
  WriteLn('Display this help');
end;

procedure runCmdHelp;
begin
  WriteLn('  b <val>           - Set break point at <val> (each value overwrites previous value). Negative value means no break point.');
  WriteLn('  d <start> <count> - Dump <count> bytes of data starting at <start> address. Addresses start at registers, then I/O registers, then SRAM.');
  WriteLn('  e <start> <count> - Dump <count> bytes of EEPROM starting at <start> address.');
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

function readTwoArguments(out a1, a2: integer): boolean;
var
  s, s1, s2: string;
  ID, err: integer;
begin
  system.ReadLn(s);
  s := trim(s);
  ID := pos(' ', s);
  s1 := copy(s, 1, ID-1);
  s2 := copy(s, ID, length(s));
  Val(s1, a1, err);
  if err = 0 then
    Val(s2, a2, err);
  Result := err = 0;
end;

var
  opts: array [0..5] of TOption;
  c: char;
  baud: integer = 0;
  serialPort: string;
  DisableDWENfuse: boolean = false;
  ID, err, startID, count: integer;
  b: byte;
  isRunning: boolean = false;
  s: string;

procedure upload(const filename: string);
begin
  DW := TDebugWire.Create();

  if not DW.Connect(serialPort, baud) then
  begin
    WriteLn('Could not connect to serial port: ', serialport);
    halt(-1);
  end;
  DW.BreakCmd;
  if not DW.IdentifyTarget then halt(-1);
  WriteLn('Device ID: $', hexStr(DW.Device.ID, 4), ' - ', DW.Device.name);
  DW.Reset;

  if FileExists(filename) then
  begin
    programHexFile(filename, DW);
    writeln('Done uploading file: ', s);
  end;
  Halt(0);
end;

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
  opts[4].Has_arg := 1;
  opts[4].Name := 'loadfile';
  opts[4].Value := 'L';

  opts[5].Flag := nil;
  opts[5].Has_arg := 0;
  opts[5].Name := '';
  opts[5].Value := #0;

  repeat
    c := GetLongOpts('S:s:B:b:Hh?IiL:l:', @opts[0], ID);
    case c of
      'S','s': serialPort := OptArg;
      'B', 'b': baud := StrToInt(OptArg);
      'H', 'h', '?':
        begin
          cmdlineHelp;
          Halt;
        end;
      'I', 'i':  DisableDWENfuse := true;
      'L', 'l':
        begin
          upload(OptArg);
        end;
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
    if DisableDWENfuse then
    begin
      DW.DisableDWEN;
    end;

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

    if DisableDWENfuse then
    begin
      WriteLn('Finished resetting DWEN fuse. Please disable DWEN over ISP before resetting controller');
      Halt(0);
    end;

    runCmdHelp;
    repeat
      if crt.keypressed then
        system.Read(c)
      else
      begin
        c := #0;
        Sleep(100);
      end;

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
               if readTwoArguments(startID, count) then
               begin
                 WriteLn('Data dump:');
                 DW.ReadAddress(startID, count, data);  // offset after regs & IOregs
                 WriteLn(FormatDataWithIndex(data, startID, 32));
               end
               else
               begin
                 WriteLn('Invalid format for command <d>. Example:');
                 WriteLn('d $100 $20');
               end;
             end;
        'e': if not isRunning then
             begin
               if readTwoArguments(startID, count) then
               begin
                 WriteLn('EEPROM dump:');
                 DW.ReadEEPROM(startID, count, data);  // offset after regs & IOregs
                 WriteLn(FormatDataWithIndex(data, startID, 32));
               end
               else
               begin
                 WriteLn('Invalid format for command <e>. Example:');
                 WriteLn('e $00 $20');
               end;
             end;
        'f': if not isRunning then
             begin
               // format: f <start> <count>
               if readTwoArguments(startID, count) then
               begin
                 WriteLn('Flash dump:');
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
                 writeln('Programmed file: ', s);
                 DW.Reset;
                 DW.Reconnect;
                 DW.PC := 0;
                 writeln('Controller reset.');
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
    DW.TimersDisabled := false;
    DW.BP := -1;
    DW.Run;
    DW.Free;
  end;
end.
