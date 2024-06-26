unit rsp;

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, ssockets, debugwire, bpmanager;

type

  { $DEFINE memorymap}

  {TGdbRspThread }
  TDebugState = (dsPaused, dsRunning);
  TDebugStopReason = (srCtrlC, srHWBP, srSWBP);

  TFlashWriteBuffer = record
    addr: word;
    Data: TBytes;
  end;

  // To view rsp communication as received by gdb:
  // (gdb) set debug remote 1
  TGdbRspThread = class(TThread)
  private
    FClientStream: TSocketStream;
    FDebugWire: TDebugWire;
    FDebugState: TDebugState;
    FLogger: TLog;
    FBPManager: TBPManager;
    FLastCmd: string;  // in case a resend is required;
    FFlashWriteBuffer: array of TFlashWriteBuffer;
    {$ifdef memorymap}
    FMemoryMap: string;
    {$endif}
    FLoggingEnabled: boolean;
    procedure FLog(s: string);
    procedure FLoggingEnabledProc(val: boolean);
    function gdb_fieldSepPos(cmd: string): integer;
    procedure gdb_response(s: string);
    procedure gdb_response(data: TBytes);
    procedure gdb_response_hexencoded(s: string);
    procedure gdb_qSupported(cmd: string);

    function FTCPDataAvailable: boolean;
    function FSerialDataAvailable: boolean;

    // Debugwire interface
    procedure DebugContinue;
    procedure DebugStep;
    procedure DebugGetRegisters;
    procedure DebugGetRegister(cmd: string);
    procedure DebugSetRegisters(cmd: string);
    procedure DebugSetRegister(cmd: string);
    procedure DebugGetMemory(cmd: string);
    procedure DebugSetMemoryHex(cmd: string);
    procedure DebugSetMemoryBinary(cmd: string);
    procedure DebugSetMemory(addr: dword; data: TBytes);
    {$IFDEF memorymap}
    procedure DebugMemoryMap(cmd: string);
    {$ENDIF memorymap}
    procedure DecodeBinary(const s: string; out data: TBytes);
    procedure EncodeBinary(const data: TBytes; out s: string);
    procedure DebugFlashErase(cmd: string);
    procedure DebugFlashWrite(cmd: string);
    procedure DebugFlashWriteDone;
    procedure DebugStopReason(signal: integer; stopReason: TDebugStopReason);
    function HexDecode(hexcode: string): string;
    procedure HandleRcmd(req: string);
  public
    constructor Create(AClientStream: TSocketStream; dw: TDebugWire; logger: TLog);
    procedure Execute; override;
    property LoggingEnabled: boolean read FLoggingEnabled write FLoggingEnabledProc;
  end;

  { TGdbRspServer }

  TGdbRspServer = class(TInetServer{TTCPServer})
  private
    FActiveThread: TGdbRspThread;
    FActiveThreadRunning: boolean;
    FDebugWire: TDebugWire;
    FSerialPort: string;
    FBaud: integer;
    FVerboseLogging: boolean;

    procedure FLog(s: string);
    procedure FActiveThreadOnTerminate(Sender: TObject);
    procedure FAcceptConnection(Sender: TObject; Data: TSocketStream);
    procedure FQueryConnect(Sender: TObject; ASocket: LongInt; var doaccept: Boolean);
  public
    constructor Create(APort: Word);
    function SerialConnect(serialPort: string; baud: integer): boolean; overload;
    property VerboseLogging: boolean read FVerboseLogging write FVerboseLogging;
  end;

implementation

uses
  sockets,
  {$IFNDEF WINDOWS}BaseUnix, math;
  {$ELSE}windows, winsock2;
  {$ENDIF}

function AddrToString(Addr: sockets.TSockAddr): String;
begin
  Result := NetAddrToStr(Addr.sin_addr);
  Result := Result  + ':' + IntToStr(Addr.sin_port);
end;

procedure TGdbRspThread.FLog(s: string);
begin
  if FLoggingEnabled and Assigned(FLogger) then
    Flogger(s);
end;

procedure TGdbRspThread.FLoggingEnabledProc(val: boolean);
begin
  FLoggingEnabled := val;
  FBPManager.LoggingEnabled := val;
  FDebugWire.LoggingEnabled := val;
end;

function TGdbRspThread.gdb_fieldSepPos(cmd: string): integer;
var
  colonPos, commaPos, semicolonPos: integer;
begin
  colonPos := pos(':', cmd);
  commaPos := pos(',', cmd);
  semicolonPos := pos(';', cmd);

  result := Max(colonPos, semicolonPos);
  result := Max(result, commaPos);

  if (colonPos > 0) and (result > colonPos) then
    result := colonPos;

  if (commaPos > 0) and (result > commaPos) then
    result := commaPos;

  if (semicolonPos > 0) and (result > semicolonPos) then
    result := semicolonPos;
end;

procedure TGdbRspThread.gdb_response(s: string);
var
  checksum, i: integer;
  reply: string;
begin
  FLastCmd := s;
  checksum := 0;

  for i := 1 to length(s) do
    checksum := checksum + ord(s[i]);

  reply := '$' + s + '#' + hexStr(byte(checksum), 2);
  FClientStream.WriteBuffer(reply[1], length(reply));
  FLog('<- ' + reply);
end;


procedure TGdbRspThread.gdb_response(data: TBytes);
var
  resp: string;
  i: integer;
begin
  resp := '';
  for i := 0 to length(data)-1 do
    resp := resp + hexStr(data[i], 2);

  gdb_response(resp);
end;

procedure TGdbRspThread.gdb_response_hexencoded(s: string);
var
  enc: string;
  i, v: integer;
begin
  enc := '';
  for i := 1 to length(s) do
  begin
    v := ord(s[i]);
    enc := enc + HexStr(v, 2);
  end;
  gdb_response(enc);
end;

procedure TGdbRspThread.gdb_qSupported(cmd: string);
begin
  if pos('Supported', cmd) > 0 then
    gdb_response('hwbreak+;swbreak+;'
      // For flash writes transfer at least a flash page full of data
      // to prevent potential repeated erasing of the same flash page
      // due to data transfer fragementation
      + 'PacketSize=' + IntToStr(FDebugWire.Device.FlashPageSize) + ';'
    {$IFDEF memorymap} + 'qXfer:memory-map:read+' {$ENDIF memorymap})
  else if pos('Offsets', cmd) > 0 then
    gdb_response('Text=0;Data=0;Bss=0')
  else if pos('Symbol', cmd) > 0 then
    gdb_response('OK');
end;

function TGdbRspThread.FTCPDataAvailable: boolean;
{$if defined(unix) or defined(windows)}
var
  FDS: TFDSet;
  TimeV: TTimeVal;
{$endif}
begin
  Result:=False;
{$if defined(unix) or defined(windows)}
  TimeV.tv_usec := 1 * 1000;  // 1 msec
  TimeV.tv_sec := 0;
{$endif}
{$ifdef unix}
  FDS := Default(TFDSet);
  fpFD_Zero(FDS);
  fpFD_Set(self.FClientStream.Handle, FDS);
  Result := fpSelect(self.FClientStream.Handle + 1, @FDS, nil, nil, @TimeV) > 0;
{$else}
{$ifdef windows}
  FDS := Default(TFDSet);
  FD_Zero(FDS);
  FD_Set(self.FClientStream.Handle, FDS);
  Result := Select(self.FClientStream.Handle + 1, @FDS, nil, nil, @TimeV) > 0;
{$endif}
{$endif}
end;

function TGdbRspThread.FSerialDataAvailable: boolean;
{$if defined(unix)}
var
  FDS: TFDSet;
  TimeV: TTimeVal;
{$else}
var
  eventMask: dword;
{$endif}
begin
  Result:=False;
{$if defined(unix)}
  TimeV.tv_usec := 10 * 1000;  // 10 msec
  TimeV.tv_sec := 0;
  FDS := Default(TFDSet);
  fpFD_Zero(FDS);
  fpFD_Set(self.FDebugWire.Serial.SerialHandle, FDS);
  Result := fpSelect(self.FDebugWire.Serial.SerialHandle + 1, @FDS, nil, nil, @TimeV) > 0;
{$elseif defined(windows)}
  eventMask := EV_BREAK or EV_RXCHAR;  // signal on BREAK or any other char
  SetCommMask(self.FDebugWire.Serial.SerialHandle, eventMask);
  eventMask := 0;
  WaitCommEvent(self.FDebugWire.Serial.SerialHandle, eventMask, nil);
  result := eventMask and (EV_BREAK or EV_RXCHAR) > 0;
{$endif}
end;

procedure TGdbRspThread.DebugContinue;
begin
  FLog('PC = ' + hexStr(FDebugWire.PC, 4));
  FDebugState := dsRunning;
  FDebugWire.Run;
end;

procedure TGdbRspThread.DebugStep;
var
  instruction, oldPC: word;
  ActiveBPRecord: PBP;
begin
  oldPC := FDebugWire.PC;
  ActiveBPRecord := FBPManager.findSWBPFromAddress(oldPC);
  if ActiveBPRecord <> nil then
  begin
    if length(ActiveBPRecord^.origCode) = 2 then
    begin
      instruction := ActiveBPRecord^.origCode[0] + (ActiveBPRecord^.origCode[1] shl 8);
      FDebugWire.SendInstruction16(instruction);
      FDebugWire.PC := oldPC + 2;
    end
    else
      FLog('Unexpected instruction code length');
  end
  else
    FDebugWire.Step;
end;

procedure TGdbRspThread.DebugGetRegisters;
var
  data: TBytes;
  resp: string;
  i: integer;
begin
  FDebugWire.safeReadRegs(0, 32, data);
  resp := '';
  for i := 0 to length(data)-1 do
    resp := resp + hexStr(data[i], 2);

  // Eliminate gdb error when reply starts with E
  resp := LowerCase(resp);

  // SREG
  FDebugWire.ReadAddress($5F, 1, data);
  resp := resp + hexStr(data[0], 2);

  // SPL & SPH
  FDebugWire.ReadAddress($5D, 2, data);
  resp := resp + hexStr(data[0], 2) + hexStr(data[1], 2);

  // PC in bytes
  resp := resp + hexStr(FDebugWire.PC and $FF, 2) + hexStr(FDebugWire.PC shr 8, 2) + '0000';
  gdb_response(resp);
end;

procedure TGdbRspThread.DebugGetRegister(cmd: string);
var
  regID: integer;
  data: TBytes;
  s: string;
  i: integer;
begin
  // cmd still contain full gdb string with p prefix
  delete(cmd, 1, 1);
  if Length(cmd) = 2 then // Hex number of a byte value
  begin
    regID := StrToInt('$'+cmd);
    case regID of
      0..31: begin // normal registers
               FDebugWire.safeReadRegs(regID, 1, data);
             end;
      32: FDebugWire.ReadAddress($5F, 1, data); // SREG

      33: FDebugWire.ReadAddress($5D, 2, data); // SPL, SPH

      34: begin // PC
            SetLength(data, 4);
            data[0] := FDebugWire.PC and $FF;
            data[1] := FDebugWire.PC shr 8;
            data[2] := 0;
            data[3] := 0;
          end;
    end;
  end;

  if length(data) > 0 then
  begin
    s := '';
    for i := 0 to high(data) do
      s := s + hexStr(data[i], 2);
    gdb_response(s);
  end
  else
    gdb_response('E00');
end;

procedure TGdbRspThread.DebugSetRegisters(cmd: string);
var
  data: TBytes;
  l1, len, i: integer;
  s: string;
begin
  // cmd still contain full gdb string with G prefix
  delete(cmd, 1, 1);
  len := length(cmd) div 2;  // in byte equivalents

  // extract normal registers
  l1 := min(len, 32);
  SetLength(data, l1);
  for i := 0 to l1-1 do
  begin
    s := '$' + cmd[2*i + 1] + cmd[2*i + 2];
    data[i] := StrToInt(s);
  end;
  FDebugWire.WriteAddress(0, data);

  // Check for SREG
  if (len > 32) then
  begin
    s := '$' + cmd[65] + cmd[66];
    SetLength(data, 1);
    data[0] := StrToInt(s);
    FDebugWire.WriteAddress($5F, data);
  end;

  // Check for SPL/SPH
  if (len > 34) then
  begin
    SetLength(data, 2);
    s := '$' + cmd[67] + cmd[68];
    data[0] := StrToInt(s);
    SetLength(data, 2);
    s := '$' + cmd[69] + cmd[70];
    data[1] := StrToInt(s);
    FDebugWire.WriteAddress($5D, data);
  end;

  // Should be PC
  if (len = 39) then
  begin
    s := '$' + copy(cmd, 71, 8);
    FDebugWire.PC := word(StrToInt(s));
  end;

  gdb_response('OK');
end;

procedure TGdbRspThread.DebugSetRegister(cmd: string);
var
  regID: integer;
  data: TBytes;
  sep, val, numbytes, i: integer;
begin
  // cmd still contain full gdb string with P prefix
  delete(cmd, 1, 1);
  sep := pos('=', cmd);
  if sep = 3 then // regID is before '='
  begin
    regID := StrToInt('$' + copy(cmd, 1, 2));

    numbytes := (length(cmd) - 3) div 2;
    SetLength(data, numbytes);
    for i := 0 to numbytes-1 do
    begin
      val := StrToInt('$' + cmd[4 + i*2] + cmd[4 + i*2 + 1]);
      data[i] := (val shr (8*i)) and $ff;
    end;

    case regID of
      // normal registers
      0..31: FDebugWire.WriteRegs(regID, data);
      // SREG
      32: FDebugWire.WriteAddress($5F, data);
      // SPL, SPH
      33: FDebugWire.WriteAddress($5D, data);
      // PC
      34: FDebugWire.PC := data[0] + data[1] shl 8;
    end;
  end;

  gdb_response('OK');
end;

procedure TGdbRspThread.DebugGetMemory(cmd: string);
var
  len, i, err: integer;
  s: string;
  addr: dword;
  data: TBytes;
begin
  delete(cmd, 1, 1);
  len := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, len-1);
  delete(cmd, 1, len);

  val(s, addr, err);
  if err <> 0 then
    addr := $FFFFFFFF; // invalid address, should be caught below

  len := gdb_fieldSepPos(cmd);
  delete(cmd, len, length(cmd) - len);
  len := StrToInt('$' + cmd);

  s := '';
  if dword(addr + len) < $800000 then // flash memory
  begin
    if dword(addr + len) < FDebugWire.Device.flashSize then
      FDebugWire.ReadFlash(addr, len, data)
    else
    begin
      FLog('Error: Flash address exceeds device limit');
      SetLength(data, 0);
    end;
  end
  else if dword(addr + len) < $810000 then // SRAM
  begin
    addr := addr and $FFFF;
    if addr < 32 + FDebugWire.Device.ioregSize + FDebugWire.Device.sramSize then
      FDebugWire.ReadAddress(addr and $FFFF, len, data)
    else
    begin
      FLog('Error: Memory address exceeds device limit');
      SetLength(data, 0);
    end;
  end
  else
  begin
    if dword(addr + len) < ($810000 + FDebugWire.Device.eepromSize) then // must be EEPROM then
      FDebugWire.ReadEEPROM(addr, len, data)
    else
    begin
      FLog('Error: Address beyond EEPROM.');
      SetLength(data, 0);
    end;
  end;

  if length(data) > 0 then
    for i := 0 to high(data) do
      s := s + hexStr(data[i], 2)
  else
    s := 'E00';

  gdb_response(s);
end;

procedure TGdbRspThread.DebugSetMemoryHex(cmd: string);
var
  len, i: integer;
  s: string;
  addr: dword;
  data: TBytes;
begin
  delete(cmd, 1, 1);
  i := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, i-1);
  addr := StrToInt(s);
  delete(cmd, 1, i);

  i := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, i-1);
  len := StrToInt(s);
  delete(cmd, 1, i);

  // now convert data
  if len = (length(cmd) div 2) then
  begin
    SetLength(data, len);
    s := '';
    for i := 0 to len-1 do
    begin
      s := '$' + cmd[2*i + 1] + cmd[2*i + 2]; // 1 based index
      data[i] := StrToInt(s);
    end;

    DebugSetMemory(addr, data);
  end
  else
  begin
    FLog('Decoded length <> expected length of Hex data.');
    gdb_response('E02');
  end;
end;

// X addr,len:XX...
procedure TGdbRspThread.DebugSetMemoryBinary(cmd: string);
var
  len: integer;
  s: string;
  addr: dword;
  data: TBytes;
begin
  delete(cmd, 1, 1);
  len := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, len-1);
  addr := StrToInt(s);
  delete(cmd, 1, len);

  len := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, len-1);
  delete(cmd, 1, len);
  len := StrToInt(s);

  // now convert data
  DecodeBinary(cmd, data);
  if len = length(data) then // ensure decoding yields the expected length data
  begin
    DebugSetMemory(addr, data);
  end
  else
  begin
    FLog('Decoded length <> expected length of binary data.');
    gdb_response('E02');
  end;
end;

procedure TGdbRspThread.DebugSetMemory(addr: dword; data: TBytes);
var
  len: integer;
begin
  len := length(data);
  if (addr + len) < $800000 then // flash memory
  begin
    if (addr + len) < FDebugWire.Device.flashSize then
      FDebugWire.WriteFlash(addr, data)
    else
    begin
      FLog('Error: Flash address exceeds device limit');
      len := 0;
    end;
  end
  else if (addr + len) < $810000 then // SRAM
  begin
    addr := addr and $FFFF;
    if addr < 32 + FDebugWire.Device.ioregSize + FDebugWire.Device.sramSize then
      FDebugWire.WriteAddress(addr, data)
    else
    begin
      FLog('Error: Memory address exceeds device limit');
      len := 0;
    end;
  end
  else // must be EEPROM then
  begin
    if dword(addr + len) < ($810000 + FDebugWire.Device.eepromSize) then // must be EEPROM then
      FDebugWire.WriteEEPROM(addr, data)
    else
    begin
      FLog('Error: Address beyond EEPROM.');
      len := 0;
    end;
  end;

  if len > 0 then
    gdb_response('OK')
  else
    gdb_response('E02');
end;

// Support for qXfer:memory-map
{$IFDEF memorymap}
procedure TGdbRspThread.DebugMemoryMap(cmd: string);
var
  i, offset, len: integer;
  s: string;
begin
  i := pos('::', cmd);
  delete(cmd, 1, i+1);
  i := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, i-1);
  offset := StrToInt(s);
  delete(cmd, 1, i);
  len := StrToInt('$' + cmd);

  // If requested length < memory map size, prepend "m", else "l"
  if (offset + len) < length(FMemoryMap) then
    s := 'm' + copy(FMemoryMap, 1+offset, len)
  else
    s := 'l' + copy(FMemoryMap, 1+offset, len);

  gdb_response(s);
end;
{$ENDIF memorymap}

procedure TGdbRspThread.DecodeBinary(const s: string; out data: TBytes);
var
  i, j: integer;
begin
  SetLength(data, 128);
  // s should start before ':', then scan until ending #
  i := 1;
  j := 0;

  while (i <= length(s)) and (s[i] <> '#') do
  begin
    if s[i] = '}' then  // escape character
    begin
      inc(i);  // read next character
      data[j] := ord(s[i]) XOR $20;
      inc(j);
    end
    //else if s[i] = '*' then //run length encoding
    //begin
    //  if j > 0 then
    //  begin
    //    // repeat count in next byte
    //    // previous byte is repeated
    //    inc(i);
    //    n := ord(s[i]) - 29;
    //    repeatData := data[j-1];
    //    while n > 0 do
    //    begin
    //      data[j] := repeatData;
    //      inc(j);
    //      dec(n);
    //    end;
    //  end
    //  else
    //    FLog('Unexpected run length encoding detected at start of data.');
    //end
    else
    begin
      data[j] := ord(s[i]);
      inc(j);
    end;
    inc(i);
    if j >= length(data) then
      SetLength(data, j + 128);
  end;

  SetLength(data, j);
end;

procedure TGdbRspThread.EncodeBinary(const data: TBytes; out s: string);
var
  i, j: integer;
begin
  SetLength(s, 128);

  j := 1;
  for i := 0 to high(data) do
  begin
    case data[i] of
      $23, $24, $2A, $7D:    // #, $, *, }
        begin
          s[j] := '}';
          inc(j);
          s[j] := char(data[i] XOR $20);
        end
      else
        s[j] := char(data[i]);
    end;

    inc(j);
    if j > length(s)-1 then
      SetLength(s, Length(s) + 128);
  end;
  SetLength(s, j-1);
end;

// ‘vFlashErase:addr,length’
// Do nothing for now
// debugwire automatically erase flash as required when writing
procedure TGdbRspThread.DebugFlashErase(cmd: string);
begin
  gdb_response('OK');
end;

// ‘vFlashWrite:addr:XX...’
procedure TGdbRspThread.DebugFlashWrite(cmd: string);
var
  i, j: integer;
  addr: integer;
  data: TBytes;
  newbuffer: boolean;
begin
  i := gdb_fieldSepPos(cmd);
  delete(cmd, 1, i);
  i := gdb_fieldSepPos(cmd);
  addr := StrToInt('$' + copy(cmd, 1, i-1));
  delete(cmd, 1, i);

  DecodeBinary(cmd, data);

  newbuffer := true;
  i := length(FFlashWriteBuffer);
  if i > 0 then
  begin
    j := length(FFlashWriteBuffer[i-1].Data);
    if (FFlashWriteBuffer[i-1].addr + j) = addr then // contiguous memory, append to this buffer
    begin
      SetLength(FFlashWriteBuffer[i-1].Data, j + length(data));
      Move(data[0], FFlashWriteBuffer[i-1].Data[j], length(data));
      newbuffer := false;
    end
    else
      newbuffer := true;
  end;

  if newbuffer then
  begin
    SetLength(FFlashWriteBuffer, i+1);
    FFlashWriteBuffer[i].addr := addr;
    FFlashWriteBuffer[i].Data := copy(data, 0, length(data));
  end;

  gdb_response('OK')
end;

procedure TGdbRspThread.DebugFlashWriteDone;
var
  i: integer;
  isGood: boolean;
begin
  isGood := true;
  for i := 0 to high(FFlashWriteBuffer) do
  begin
    if FFlashWriteBuffer[i].addr <= FDebugWire.Device.flashSize then
      FDebugWire.WriteFlash(FFlashWriteBuffer[i].addr, FFlashWriteBuffer[i].data)
    else
    begin
      isGood := false;
      FLog('unexpected address passed to DebugFlashWrite');
    end;
  end;

  if isGood then
    gdb_response('OK')
  else
    gdb_response('E00');
end;

procedure TGdbRspThread.DebugStopReason(signal: integer;
  stopReason: TDebugStopReason);
var
  s: string;
  data: TBytes;
  i: integer;
begin
  s := 'T' + hexStr(signal, 2);
  case stopReason of
    srHWBP: s := s + 'hwbreak:;';
    srSWBP: s := s + 'swbreak:;';
  end;

  // 32 General data
  FDebugWire.ReadAddress(0, 32, data);
  for i := 0 to 31 do
  begin
    s := s + hexStr(i, 2) + ':' + HexStr(data[i], 2) + ';';
  end;

  // SREG
  FDebugWire.ReadAddress($5F, 1, data);
  s := s + '20:' + hexStr(data[0], 2) + ';';

  // SPL & SPH
  FDebugWire.ReadAddress($5D, 2, data);
  s := s + '21:' + hexStr(data[0], 2) + hexStr(data[1], 2) +';';

  // PC in bytes
  s := s + '22:' + hexStr(FDebugWire.PC and $FF, 2) + hexStr(FDebugWire.PC shr 8, 2) + '0000;';

  gdb_response(s);
end;

function TGdbRspThread.HexDecode(hexcode: string): string;
var
  i: integer;
  s: string;
begin
  SetLength(Result, length(hexCode) div 2);
  for i := 1 to length(Result) do
  begin
    s := '$' + hexCode[2*i-1] + hexCode[2*i];
    result[i] := char(StrToInt(s));
  end;
end;

procedure TGdbRspThread.HandleRcmd(req: string);
var
  resp: string;
  cmds: TStringList;
begin
  if length(req) = 0 then exit;

  cmds := TStringList.Create;
  try
    cmds.Delimiter := ' ';
    cmds.DelimitedText := lowercase(HexDecode(req));
    FLog('Rcmd received: ' + cmds.DelimitedText);

    resp := 'OK';  // Only override on actual error or alternative output
    case cmds[0] of
      'help':
        begin
          resp := '  help - List of commands supported.'+LineEnding +
                  '  set verbose [1 or 0] - Enable or disable verbose messages'+LineEnding +
                  '  set timers-disabled [1 or 0] - Disable or enable timers'+LineEnding;
        end;
      'set':  // Options: verbose, timers-disabled
        begin
          if cmds.Count = 3 then
          begin
            case cmds[1] of
              'verbose':
                begin
                  if cmds[2] = '1' then
                    Self.LoggingEnabled := true
                  else if cmds[2] = '0' then
                    Self.LoggingEnabled := false
                  else
                    resp := 'E00';
                end;

              'timers-disabled':
                begin
                  if cmds[2] = '1' then
                    FDebugWire.TimersDisabled := true
                  else if cmds[2] = '0' then
                    FDebugWire.TimersDisabled := false
                  else
                    resp := 'E00';
                end;
              else
                resp := 'E00';
            end
          end
          else
            resp := 'E00';
        end;
      else
        resp := 'E00';
    end;
  finally
    cmds.Free;
  end;

  // Monitor responses should be encoded,
  // but normal protocol responses (OK, Enn) should be returned plain text
  if (resp = '') or (resp = 'OK') or ((length(resp) = 3) and (resp[1] = 'E')) then
    gdb_response(resp)
  else
    gdb_response_hexencoded(resp);
end;

{ TGdbRspThread }

constructor TGdbRspThread.Create(AClientStream: TSocketStream; dw: TDebugWire;
  logger: TLog);
begin
  inherited Create(false);
  FreeOnTerminate := true;
  FClientStream := AClientStream;
  FDebugWire := dw;
  FDebugState := dsPaused;
  FLogger := logger;
  FLoggingEnabled := Assigned(logger);
  dw.LoggingEnabled := FLoggingEnabled;
  FBPManager := TBPManager.Create(FDebugWire, logger);
  FBPManager.LoggingEnabled := FLoggingEnabled;

  {$IFDEF memorymap}
  if dw.Device.ID > 0 then
    FMemoryMap := format('<memory-map> <memory type="ram" start="0x800000" length="0x%.4x"/> <memory type="flash" start="0" length="0x%.4x">  <property name="blocksize">0x40</property> </memory></memory-map>',
                  [32 + FDebugWire.Device.ioregSize + FDebugWire.Device.sramSize, FDebugWire.Device.flashSize]);
  {$ENDIF memorymap}

  //FMemoryMap := '<?xml version="1.0"?> '+ LineEnding +
  //     '<!DOCTYPE memory-map PUBLIC "+//IDN gnu.org//DTD GDB Memory Map V1.0//EN" "http://sourceware.org/gdb/gdb-memory-map.dtd"> '+ LineEnding +
  //     '<memory-map> '+ LineEnding +
  //     '  <memory type="ram" start="0x800000" length="0x'+ HexStr(FDebugWire.Device.sramSize, 4) + '"/>'+ LineEnding +
  //     '  <memory type="flash" start="0x00" length="0x'+ HexStr(FDebugWire.Device.flashSize, 4) +'">'+ LineEnding +
  //     '    <property name="blocksize">0x'+ HexStr(FDebugWire.Device.FlashPageSize, 4) +'</property>'+ LineEnding +
  //     '  </memory>'+ LineEnding +
  //     '</memory-map>';
end;

function pos_(const c: char; const s: RawByteString): integer;
var
  i, j: integer;
begin
  i := 1;
  j := length(s);
  while (i < j) and (s[i] <> c) do
    inc(i);

  if s[i] = c then result := i
  else
  result := 0;
end;

procedure TGdbRspThread.Execute;
var
  msg, cmd : RawByteString;
  buf: array[0..1023] of char;
  count, i, j, idstart, idend, addr: integer;
  Done: boolean;
begin
  Done := false;
  msg := '';
  count := 0;

  repeat
    try
      if FTCPDataAvailable then
      begin
        count := count + FClientStream.Read(buf[0], length(buf));
        // if socket signaled and no data available -> connection closed
        if count = 0 then
        begin
          // simulate a kill command, it will delete hw BP and run target
          // then exit this thread
          buf[0]:='$'; buf[1]:='k'; buf[2]:='#'; buf[3]:='0'; buf[4]:='0';  // CRC is not checked...
          count := 5;
          FLog('No data read, exiting read thread by simulating [k]ill...');
        end;
      end
      else
        count := 0;

      // If running target, check check for break
      if FDebugState = dsRunning then
      begin
        if FDebugWire.TrySync then
        begin
          FDebugState := dsPaused;
          FDebugWire.Reconnect;
          DebugStopReason(5, srHWBP);
        end
      end;

      if count > 0 then
      begin
        SetLength(msg, count);
        Move(buf[0], msg[1], count);
        //msg := copy(buf, 1, count);

        // ctrl+c falls outside of normal gdb message structure
        if msg[1] = #3 then // ctrl+C
        begin
          // ack received command
          FClientStream.WriteByte(ord('+'));
          FLog('-> Ctrl+C');
          FDebugWire.BreakCmd;
          FDebugWire.Reconnect;
          FDebugState := dsPaused;
          DebugStopReason(2, srCtrlC); // SIGINT, because the program was interrupted...
        end
        else
          FLog('-> ' + msg);

        i := pos('$', msg);  // start of command
        j := pos('#', msg);  // end of command

        // Check if complete message received
        if (i > 0) and (j = 0) then
          while j = 0 do
          begin
            idstart := FClientStream.Read(buf[0], length(buf));
            if idstart > 0 then
            begin
              SetLength(msg, count + idstart);
              Move(buf[0], msg[count + 1], idstart);
              count := count + idstart;
              j := pos('#', msg);  // end of command
            end
            else
              break;
          end;

        // This check also skip ack / nack replies (+/-)
        if (i > 0) and ((count - 2) >= j) then  // start & terminator + 2 byte CRC received
        begin
          // ack received command
          FClientStream.WriteByte(ord('+'));
          FLog('<- +');

          cmd := copy(msg, i + 1, (j - i - 1));

          case cmd[1] of
            // stop reason
            '?': DebugStopReason(5, srHWBP);

            // continue
            'c': begin
                   FBPManager.FinalizeBPs;  // check which BPs needs to be removed/written
                   FBPManager.PrintBPs;     // debug
                   DebugContinue;
                 end;

            // detach, kill
            // Reset and continue MCU - no way to kill target anyway
            'D', 'k':
              begin
                Done := true;         // terminate debug connection
                if cmd[1] = 'k' then  // kill handled as break/reset/run
                  FDebugWire.Reset
                else                  // assume detach leaves system in a runnable state
                  gdb_response('OK');

                //Done: call BP manager to remove HW & SW BPs
                FBPManager.DeleteAllBPs;
                FBPManager.FinalizeBPs;
                FBPManager.PrintBPs;     // debug
                FDebugWire.Run;
              end;

            // read general registers 32 registers + SREG + PCL + PCH + PCHH
            'g': DebugGetRegisters;

            // write general registers 32 registers + SREG + PCL + PCH + PCHH
            'G': DebugSetRegisters(cmd);

            // Set thread for operation - only 1 so just acknowledge
            'H': gdb_response('OK');

            // Read memory
            'm': DebugGetMemory(cmd);

            // Write memory
            'M': DebugSetMemoryHex(cmd);

            'p': DebugGetRegister(cmd);

            'P': DebugSetRegister(cmd);

            'q': if pos('Supported', cmd) > 0 then
                   gdb_qSupported(cmd)
            {$IFDEF memorymap}
                 else if pos('Xfer:memory-map:read', cmd) > 0 then
                   DebugMemoryMap(cmd)
            {$ENDIF memorymap}
                 else if pos('Rcmd', cmd) > 0 then
                 begin
                   delete(cmd, 1, pos(',', cmd));
                   if length(cmd) > 0 then
                     HandleRcmd(cmd)
                   else
                     gdb_response('');
                 end
                 else
                   gdb_response('');

            // step
            's': begin
                   // TODO: Check if stepping into a sw BP?
                   DebugStep;
                   DebugStopReason(5, srSWBP);
                 end;

            'v': begin
                   if pos('FlashErase', cmd) > 0 then
                   begin
                     DebugFlashErase(cmd);
                   end
                   else if pos('FlashWrite', cmd) > 0 then
                   begin
                     DebugFlashWrite(cmd);
                   end
                   else if pos('FlashDone', cmd) > 0 then
                   begin
                     DebugFlashWriteDone;
                   end
                   else
                     gdb_response('');
                 end;

            'X': begin
                   DebugSetMemoryBinary(cmd);
                 end;

            // delete breakpoint
            'z': begin
                   if (cmd[2] in ['0', '1']) then
                   begin
                     idstart := gdb_fieldSepPos(cmd);
                     delete(cmd, 1, idstart);
                     idend := gdb_fieldSepPos(cmd);
                     msg := copy(cmd, 1, idend-1);
                     addr := StrToInt('$' + msg);

                     FBPManager.DeleteBP(addr);
                     gdb_response('OK');
                   end
                   else
                     gdb_response('');
                 end;

            // insert sw or hw breakpoint via BP manager
            'Z': begin
                   if (cmd[2] in ['0', '1']) then
                   begin
                     idstart := gdb_fieldSepPos(cmd);
                     delete(cmd, 1, idstart);
                     idend := gdb_fieldSepPos(cmd);
                     msg := copy(cmd, 1, idend-1);

                     addr := StrToInt('$'+msg);
                     FBPManager.AddBP(addr);
                     gdb_response('OK');
                   end
                   else  // other BPs such as watch points not supported
                     gdb_response('');
                 end;
            else
              gdb_response('');
          end; // case
        end     // if (i > 0) and ((count - 2) >= j)
        else if (msg[1] = '-') then
        begin
          FLog('Resending previous message');
          gdb_response(FLastCmd);
        end;
        count := 0;  // so that next loop doesn't echo...
      end;       // if (count > 0)
    except
      on e: Exception do
      begin
        Done := true;
        FLog('Exception: ' + e.Message);
      end;
    end;
  until Done;

  // Update status of server thread
  OnTerminate(self);
end;

{ TGdbRspServer }

procedure TGdbRspServer.FLog(s: string);
var
  h, m, sec, ms: word;
begin
  DecodeTime(Now, h, m, sec, ms);
  WriteLn(Format('%.2d:%.2d:%.2d.%.3d  ', [h, m, sec, ms]) + s);
end;

procedure TGdbRspServer.FActiveThreadOnTerminate(Sender: TObject);
begin
  FActiveThreadRunning := false;
  FLog('FActiveThreadOnTerminate');
  halt;
end;

procedure TGdbRspServer.FAcceptConnection(Sender: TObject; Data: TSocketStream);
begin
  if not FActiveThreadRunning then
  begin
    FLog('Incoming connection from ' + AddrToString(Data.RemoteAddress));
    FActiveThread := TGdbRspThread.Create(Data, FDebugWire, @self.FLog);
    FActiveThread.OnTerminate := @FActiveThreadOnTerminate;
    FActiveThread.LoggingEnabled := VerboseLogging;
    FActiveThreadRunning := true;
  end
  else
  begin
    FLog('Multiple connections not allowed...');
  end;
end;

procedure TGdbRspServer.FQueryConnect(Sender: TObject; ASocket: LongInt;
  var doaccept: Boolean);
begin
  if FActiveThreadRunning then
  begin
    doaccept := false;
    FLog('Refusing new connection - already connected');
  end
  else
  begin
    doaccept := true;
    FLog('Accepting new connection');
  end;
end;

constructor TGdbRspServer.Create(APort: Word);
begin
  inherited Create(APort);
  { On posix, ReuseAddress must be set to ensure socket can be re-opened
    without waiting for a time-out. }
  ReuseAddress := true;
  OnConnect := @FAcceptConnection;
  OnConnectQuery := @FQueryConnect;
  FActiveThreadRunning := false;

  FDebugWire := TDebugWire.Create;
  FDebugWire.OnLog := @FLog;
end;

function TGdbRspServer.SerialConnect(serialPort: string; baud: integer): boolean;
begin
  Result := (serialPort <> '');
  if Result then
  begin
    FDebugWire.LoggingEnabled := VerboseLogging;
    Result := FDebugWire.Connect(serialPort, baud);
    if Result then
    begin
      FSerialPort := serialPort;
      FBaud := baud;
      FDebugWire.BreakCmd;
      Result := FDebugWire.IdentifyTarget;
    end;
  end;

  if Result then
  begin
    FDebugWire.Reset;   // reset MCU
    FDebugWire.PC := 0;
  end
  else
  begin
    FLog('Error connecting to debugwire target on: ' + serialPort);
    MaxConnections := 0;  // effectively stop listening
  end;
end;

end.
