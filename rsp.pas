unit rsp;

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, ssockets, debugwire, bpmanager;

type

  { TGdbRspThread }
  TDebugState = (dsPaused, dsRunning);
  TDebugStopReason = (srCtrlC, srHWBP, srSWBP);

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
    procedure FLog(s: string);

    function gdb_fieldSepPos(cmd: string): integer;
    procedure gdb_response(s: string);
    procedure gdb_response(data: TBytes);
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
    procedure DebugSetMemory(cmd: string);
    procedure DebugMemoryMap;
    procedure DebugStopReason(signal: integer; stopReason: TDebugStopReason);
  public
    constructor Create(AClientStream: TSocketStream; dw: TDebugWire; logger: TLog);
    procedure Execute; override;
    property OnLog: TLog read FLogger write FLogger;
  end;

  { TGdbRspServer }

  TGdbRspServer = class(TInetServer{TTCPServer})
  private
    FActiveThread: TGdbRspThread;
    FActiveThreadRunning: boolean;
    FDebugWire: TDebugWire;
    FSerialPort: string;
    FBaud: integer;

    procedure FLog(s: string);
    procedure FActiveThreadOnTerminate(Sender: TObject);
    procedure FAcceptConnection(Sender: TObject; Data: TSocketStream);
    procedure FQueryConnect(Sender: TObject; ASocket: LongInt; var doaccept: Boolean);
  public
    constructor Create(APort: Word);
    constructor Create(APort: Word; serialPort: string);
    constructor Create(APort: Word; serialPort: string; baud: integer);
    procedure SerialConnect;
  end;

implementation

uses
  {$IFNDEF WINDOWS}BaseUnix, sockets, math;
  {$ELSE}winsock2, windows;
  {$ENDIF}

function AddrToString(Addr: TSockAddr): String;
{$IFDEF WINDOWS}
//var

{$ENDIF}
begin
  {$IFNDEF WINDOWS}
  Result := NetAddrToStr(Addr.sin_addr);
  {$ELSE}
  Result := inet_ntoa(Addr.sin_addr);
  {$ENDIF}

  Result := Result  + ':' + IntToStr(Addr.sin_port);
end;

procedure TGdbRspThread.FLog(s: string);
begin
  if Assigned(FLogger) then
    Flogger({TimeToStr(Now) + ' : ' +} s);
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

procedure TGdbRspThread.gdb_qSupported(cmd: string);
begin
  if pos('Supported', cmd) > 0 then
    gdb_response('hwbreak+;swbreak+')
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
  FDebugState := dsRunning;
  FDebugWire.Run;
end;

procedure TGdbRspThread.DebugStep;
begin
  FDebugWire.Step;
end;

procedure TGdbRspThread.DebugGetRegisters;
var
  data: TBytes;
  resp: string;
  i: integer;
begin
  FDebugWire.ReadRegs(0, 32, data);
  resp := '';
  for i := 0 to length(data)-1 do
    resp := resp + hexStr(data[i], 2);

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
               FDebugWire.ReadRegs(regID, 1, data);
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
      0..31: FDebugWire.ReadRegs(regID, 1, data);
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
  len, i: integer;
  s: string;
  addr: dword;
  data: TBytes;
begin
  delete(cmd, 1, 1);
  len := gdb_fieldSepPos(cmd);
  s := '$' + copy(cmd, 1, len-1);
  delete(cmd, 1, len);

  addr := StrToInt(s);
  len := gdb_fieldSepPos(cmd);
  delete(cmd, len, length(cmd) - len);
  len := StrToInt('$' + cmd);

  if (addr + len) < $800000 then // flash memory
  begin
    if (addr + len) < FDebugWire.Device.flashSize then
      FDebugWire.ReadFlash(addr, len, data)
    else
    begin
      FLog('Error: Flash address exceeds device limit');
      SetLength(data, len);
      FillByte(data[0], length(data), 0);
    end;
  end
  else if (addr + len) < $810000 then // SRAM
  begin
    addr := addr and $FFFF;
    if addr < 32 + FDebugWire.Device.ioregSize + FDebugWire.Device.sramSize then
      FDebugWire.ReadAddress(addr and $FFFF, len, data)
    else
    begin
      FLog('Error: Memory address exceeds device limit');
      SetLength(data, len);
      FillByte(data[0], length(data), 0);
    end;
  end
  else // must be EEPROM then
  begin
    FLog('Error: EEPROM access not supported yet.');
    SetLength(data, 0);
  end;

  s := '';
  for i := 0 to high(data) do
    s := s + hexStr(data[i], 2);

  gdb_response(s);
end;

procedure TGdbRspThread.DebugSetMemory(cmd: string);
var
  len, i: integer;
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

  // now convert data
  len := length(cmd) div 2;
  SetLength(data, len);
  s := '';
  for i := 0 to len-1 do
  begin
    s := '$' + cmd[2*i + 1] + cmd[2*i + 2]; // 1 based index
    data[i] := StrToInt(s);
  end;

  if (addr + len) < $800000 then // flash memory
  begin
    if (addr + len) < FDebugWire.Device.flashSize then
      FDebugWire.WriteFlash(addr, data)
    else
    begin
      FLog('Error: Flash address exceeds device limit');
      SetLength(data, 0);
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
      SetLength(data, 0);
    end;
  end
  else // must be EEPROM then
  begin
    FLog('Error: EEPROM access not supported yet.');
    SetLength(data, 0);
  end;

  gdb_response('OK');
end;

procedure TGdbRspThread.DebugMemoryMap;
begin
{recv: qXfer:memory-map:read::0,fff
query: Xfer;memory-map:read::0,fff
Xfer: type:memory-map;op:read;annex:;addr:0;length:4095
send: m<?xml version="1.0"?><!DOCTYPE memory-map PUBLIC "+//IDN gnu.org//DTD GDB Memory Map V1.0//EN"     "http://sourceware.org/gdb/gdb-memory-map.dtd"><memory-map>  <memory type="rom" start="0x00000000" length="0x20000"/>  <memory type="ram" start="0x20000000" length="0x5000"/>  <memory type="flash" start="0x08000000" length="0x20000">    <property name="blocksize">0x400</property>  </memory>  <memory type="ram" start="0x40000000" length="0x1fffffff"/>  <memory type="ram" start="0xe0000000" length="0x1fffffff"/>  <memory type="rom" start="0x1ffff000" length="0x800"/>  <memory type="rom" start="0x1ffff800" length="0x10"/></memory-map>
recv: qXfer:memory-map:read::27c,d83
query: Xfer;memory-map:read::27c,d83
Xfer: type:memory-map;op:read;annex:;addr:636;length:3459
send: l
recv: m800010c,4
send: 154880f3
}
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
    srHWBP: s := s + 'hwbreak';
    srSWBP: s := s + 'swbreak';
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
  FBPManager := TBPManager.Create(FDebugWire, logger);
end;

procedure TGdbRspThread.Execute;
var
  msg, cmd : String;
  buf: array[0..1023] of char;
  count, i, j, idstart, idend, addr: integer;
  Done: boolean;
begin
  Done := false;
  msg := '';

  repeat
    try

      if FTCPDataAvailable then
      begin
        count := FClientStream.Read(buf[0], length(buf));
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
        msg := copy(buf, 1, count);

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

            // step
            's': begin
                   // TODO: Check if stepping into a sw BP?
                   DebugStep;
                   DebugStopReason(5, srSWBP);
                 end;

            // read general registers 32 registers + SREG + PCL + PCH + PCHH
            'g': DebugGetRegisters;

            // write general registers 32 registers + SREG + PCL + PCH + PCHH
            'G': DebugSetRegisters(cmd);

            // Set thread for operation - only 1 so just acknowledge
            'H': gdb_response('OK');

            'p': DebugGetRegister(cmd);

            'P': DebugSetRegister(cmd);

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

            // Read memory
            'm': DebugGetMemory(cmd);

            // Write memory
            // TODO: also support X - binary equivalent.
            'M': DebugSetMemory(cmd);

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
                FDebugWire.Run;
              end;

            'q': if pos('Supported', cmd) > 0 then
                   gdb_qSupported(cmd)
                 //else if cmd[2] = 'L' then
                 //  gdb_response('qM0010000000000000000')
                 else
                   gdb_response('');
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
    FLog('Incoming connection from ');// + AddrToString(AStream.PeerAddress));
    FActiveThread := TGdbRspThread.Create(Data, FDebugWire, @self.FLog);
    FActiveThread.OnTerminate := @FActiveThreadOnTerminate;
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
  OnConnect := @FAcceptConnection;
  OnConnectQuery := @FQueryConnect;
  FActiveThreadRunning := false;

  FDebugWire := TDebugWire.Create;
  FDebugWire.OnLog := @FLog;

  SerialConnect;
end;

constructor TGdbRspServer.Create(APort: Word; serialPort: string);
begin
  FSerialPort := serialPort;
  FBaud := 0;
  Create(APort);
end;

constructor TGdbRspServer.Create(APort: Word; serialPort: string; baud: integer);
begin
  FSerialPort := serialPort;
  FBaud := baud;
  Create(APort);
end;

procedure TGdbRspServer.SerialConnect;
var
  targetOK: boolean;
begin
  targetOK := false;
  if (FSerialPort <> '') then
  begin
    if FDebugWire.Connect(FSerialPort, FBaud) then
    begin
      FDebugWire.BreakCmd;
      targetOK := FDebugWire.IdentifyTarget;
    end;
  end;
{  else  // no port name specified, scan possible candidates
  begin
    if FDebugWire.Connect('/dev/ttyUSB0', FBaud) then
    begin
      FDebugWire.BreakCmd;
      targetOK := FDebugWire.IdentifyTarget;
    end;

    if not (targetOK) and FDebugWire.Connect('/dev/ttyACM1', FBaud) then
    begin
      FDebugWire.BreakCmd;
      targetOK := FDebugWire.IdentifyTarget;
    end;

    if not (targetOK) and FDebugWire.Connect('/dev/ttyACM0', FBaud) then
    begin
      FDebugWire.BreakCmd;
      targetOK := FDebugWire.IdentifyTarget;
    end;
  end;
}
  //targetOK := true;
  if targetOK then
  begin
    FDebugWire.Reset;   // reset MCU
    //Self.Port := aport;
    //self.Active := true;
    //FLog('Listening on port: ' + IntToStr(aport));
    //EventLoop.Run;    // doesn't yet close
    //FLog('Closing...');
  end
  else
  begin
    FLog('Target not started...');
    MaxConnections := 0;  // effectively stop listening
  end;
end;

end.
