unit rsp;

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, Sockets, fpAsync, fpSock,
  math, debugwire;

type

  { TGdbRspThread }
  TDebugState = (dsPaused, dsRunning);

  TGdbRspThread = class(TThread)
  private
    FClientStream: TSocketStream;
    FDebugWire: TDebugWire;
    FDebugState: TDebugState;
    FLogger: TLog;
    procedure FLog(s: string);

    // check if data available
    function FPeek(): integer;

    function gdb_fieldSepPos(cmd: string): integer;
    procedure gdb_response(s: string);
    procedure gdb_response(data: TBytes);
    procedure gdb_qSupported(cmd: string);

    // Debugwire interface
    procedure DebugContinue;
    procedure DebugStep;
    procedure DebugGetRegisters;
    procedure DebugSetRegisters(cmd: string);
    procedure DebugGetMemory(cmd: string);
    procedure DebugSetMemory(cmd: string);
    procedure DebugMemoryMap;
    procedure DebugStopReason;
  public
    constructor Create(AClientStream: TSocketStream; dw: TDebugWire);
    procedure Execute; override;
    property OnLog: TLog read FLogger write FLogger;
  end;

  { TGdbRspServer }

  TGdbRspServer = class(TTCPServer)
  private
    FActiveThread: TGdbRspThread;
    FActiveThreadRunning: boolean;
    FDebugWire: TDebugWire;
    FSerialPort: string;
    FBaud: integer;

    procedure FLog(s: string);
    procedure FActiveThreadOnTerminate(Sender: TObject);
    procedure FAcceptConnection(Sender: TConnectionBasedSocket; AStream: TSocketStream);
    procedure FQueryConnect(Sender: TConnectionBasedSocket; Socket: Integer;
    var accept: Boolean);
  public
    constructor Create(AOwner: TComponent);
    constructor Create(AOwner: TComponent; serialPort: string);
    constructor Create(AOwner: TComponent; serialPort: string; baud: integer);
    procedure Listen(aport: Word);
  end;

implementation

uses
  BaseUnix;

function AddrToString(Addr: TSockAddr): String;
begin
  Result := NetAddrToStr(Addr.sin_addr) + ':' + IntToStr(Addr.sin_port);
end;

procedure TGdbRspThread.FLog(s: string);
begin
  if Assigned(FLogger) then
    Flogger({TimeToStr(Now) + ' : ' +} s);
end;

function TGdbRspThread.FPeek(): integer;
begin
  Result := fprecv(FClientStream.Handle, nil, 1, MSG_PEEK);
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
  checksum := 0;

  for i := 1 to length(s) do
    checksum := checksum + ord(s[i]);

  reply := '$' + s + '#' + hexStr(checksum and $ff, 2);
  FClientStream.WriteAnsiString(reply);
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
var
  opt, reply: string;
  i: integer;
begin
  reply := 'hwbreak+;swbreak+';
{  reply := '';
  // Strip qSupported from cmd...
  i := gdb_fieldSepPos(cmd);
  delete(cmd, 1, i);

  // Separator between this and next option
  i := gdb_fieldSepPos(cmd);

  while length(cmd) > 0 do//i > 0 do
  begin
    if i > 1 then
    begin
      opt := copy(cmd, 1, i-1);
      delete(cmd, 1, i);
    end
    else
    begin
      opt := cmd;
      cmd := '';
    end;

    if opt = 'hwbreak+' then
      reply := reply + 'hwbreak+'
    else
    begin
      reply := reply + copy(opt, 1, length(opt)-1) + '-';
    end;

    reply :=reply + ';';

    i := gdb_fieldSepPos(cmd);
  end;

  // Strip separator at end of reply
  if length(reply) > 0 then
    delete(reply, length(reply), 1);
}
  gdb_response(reply);
end;

procedure TGdbRspThread.DebugContinue;
begin
  //FDebugWire.Run;
  FDebugWire.ContinueUntilBreak;
  //FDebugState := dsRunning;
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

procedure TGdbRspThread.DebugSetRegisters(cmd: string);
var
  data: TBytes;
  l1, len, i: integer;
  s: string;
begin
  // cmd still contain full gdb string with G prefix
  delete(cmd, 1, 1);
  len := length(cmd) div 2;

  // extract normal registers
  l1 := min(len, 32);
  SetLength(data, l1);
  for i := 0 to l1-1 do
  begin
    s := '$' + cmd[2*i] + cmd[2*i + 1];
    data[i] := StrToInt(s);
  end;
  FDebugWire.WriteAddress(0, data);

  // Check for SREG
  if (len > 32) then
  begin
    s := '$' + cmd[64] + cmd[65];
    SetLength(data, 1);
    data[0] := StrToInt(s);
    FDebugWire.WriteAddress($5F, data);
  end;

  // Should be PC
  if (len > 33) then
  begin
    l1 := len - 33; // number of bytes for PC
    s := '$' + copy(cmd, 34, l1);
    data[0] := StrToInt(s);
    FDebugWire.WriteAddress($5F, data);
  end;

  gdb_response('OK');
end;

procedure TGdbRspThread.DebugGetMemory(cmd: string);
var
  len: integer;
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

  gdb_response(data);
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

procedure TGdbRspThread.DebugStopReason;
var
  s: string;
  data: TBytes;
  i: integer;
begin
  s := 'T05';//'T05hwbreak:;';
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

constructor TGdbRspThread.Create(AClientStream: TSocketStream; dw: TDebugWire);
begin
  inherited Create(false);
  FreeOnTerminate := true;
  FClientStream := AClientStream;
  FDebugWire := dw;
  FDebugState := dsPaused;
end;

procedure TGdbRspThread.Execute;
var
  msg, cmd : String;
  buf: array[0..1023] of char;
  count, i, j, idstart, idend, addr: integer;
  rdfd, edfd: TFDSet;
  timeout: TTimeVal;
  maxhandle: integer;
  serhandle, tcphandle: THandle;
  Done: boolean;
begin
  Done := false;
  msg := '';

  repeat
    try
      timeout.tv_sec := 5;
      timeout.tv_usec := 0;
      serhandle := FDebugWire.Serial.SerialHandle;
      tcphandle := FClientStream.Handle;
      fpFD_ZERO(rdfd);
      fpFD_ZERO(edfd);
      fpFD_SET(tcphandle, rdfd);
      fpFD_SET(tcphandle, edfd);
      maxhandle := tcphandle;
      fpFD_SET(serhandle, rdfd);
      if serhandle > maxhandle then
        maxhandle := serhandle;

      if fpSelect(maxhandle+1, @rdfd, nil, @edfd, @timeout) > 0 then
      begin
        if (FDebugState = dsPaused) and (fpFD_ISSET(tcphandle, rdfd) > 0) then  // TCP data available
        begin
          count := FClientStream.Read(buf[0], length(buf));
          //https://www.linuxquestions.org/questions/programming-9/how-could-server-detect-closed-client-socket-using-tcp-and-c-824615/
          if (count = 0) then
          begin
            // simulate a kill command, it will delete hw BP and run target
            // then exit this thread
            buf[0]:='$'; buf[1]:='k'; buf[2]:='#'; buf[3]:='0'; buf[4]:='0';  // CRC is not checked...
            count := 5;
            FLog('No data read, exiting read thread...');
          end;
        end;

        if (count = 0) and (fpFD_ISSET(tcphandle, edfd) > 0) then
        begin
          // Done := true;
          // simulate a kill command, it will delete hw BP and run target
          // then exit this thread
          buf[0]:='$'; buf[1]:='k'; buf[2]:='#'; buf[3]:='0'; buf[4]:='0';  // CRC is not checked...
          count := 5;
          FLog('Error reading socket handle, done...');
        end;

        if (FDebugState = dsRunning) and (fpFD_ISSET(serhandle, rdfd) > 0) then // Serial data available
        begin
          if FDebugWire.TrySync then
          begin
            FDebugState := dsPaused;
            FDebugWire.Reconnect;
            DebugStopReason;
            //gdb_response('S05');
          end
          else
            FLog('Couldn''t detect break signal');
        end;
      end;

      if count > 0 then
      begin
        msg := copy(buf, 1, count);
        count := length(msg);
        i := pos('$', msg);  // start of command
        j := pos('#', msg);  // end of command

        if (i > 0) and ((count - 2) >= j) then  // start & terminator + 2 byte CRC received
        begin
          // ack received command
          FClientStream.WriteByte(ord('+'));

          cmd := copy(msg, i + 1, (j - i - 1));
          FLog('-> ' + cmd);

          // Chop logic into a commands acceptable when target is running and
          // other commands applicable only if target is paused

          if FDebugState = dsRunning then
          begin
            case cmd[1] of
              // detach, kill, ctrl-c
              // Resume/continue MCU?
              // perhaps reset then run?
              'D', 'k':
                begin
                  Done := true;         // terminate debug connection
                  if cmd[1] = 'k' then  // kill handled as break/reset/run
                  begin
                    FDebugWire.BreakCmd;
                    FDebugWire.Reset;
                    FDebugWire.BP := -1;  // in case a hw break is set
                    FDebugWire.Run;
                  end
                  else                  // assume detach leaves system in a runnable state
                    gdb_response('OK');
                end;

              #3 : begin                // ctrl+c interrupts running process
                     FDebugWire.BreakCmd;
                     FDebugWire.Sync;
                     FDebugWire.Reconnect;
                     FDebugState := dsPaused;
                     gdb_response('OK');
                   end;

              else
                gdb_response('');
            end; // case
          end    // if
          else
          begin
            case cmd[1] of
              // stop reason
              '?': DebugStopReason;//gdb_response('S05');

              // continue
              'c': begin
                     DebugContinue;
                     DebugStopReason;
                     //gdb_response('OK');
                   end;

              // step
              's': begin
                     DebugStep;
                     DebugStopReason;
                     //gdb_response('S05');
                   end;

              // read general registers 32 registers + SREG + PCL + PCH + PCHH
              'g': DebugGetRegisters;

              // write general registers 32 registers + SREG + PCL + PCH + PCHH
              'G': DebugSetRegisters(cmd);

              // delete breakpoint
              'z': begin
                     if (cmd[2] in ['0', '1']) and (FDebugWire.BP > -1) then  // only delete HW breakpoint, SW BP not supported internally yet
                     begin
                       idstart := gdb_fieldSepPos(cmd);
                       delete(cmd, 1, idstart);
                       idend := gdb_fieldSepPos(cmd);
                       msg := copy(cmd, 1, idend-1);
                       addr := StrToInt('$' + msg);
                       if addr = FDebugWire.BP then
                       begin
                         FDebugWire.BP := -1;
                         gdb_response('OK');
                       end
                       else
                         gdb_response('E01');  // address should match stored BP
                     end
                     else
                       gdb_response('');
                   end;

              // insert breakpoint
              'Z': begin
                     if (cmd[2] in ['0', '1']) and (FDebugWire.BP = -1) then // only hardware breakpoint accepted - > gdb will then manage software breakpoints from outside
                     begin
                       idstart := gdb_fieldSepPos(cmd);
                       delete(cmd, 1, idstart);
                       idend := gdb_fieldSepPos(cmd);
                       msg := copy(cmd, 1, idend-1);

                       FDebugWire.BP := StrToInt('$'+msg);
                       gdb_response('OK');
                     end
                     else
                       gdb_response('');
                   end;

              // Read memory
              'm': DebugGetMemory(cmd);

              // Write memory
              // TODO: also support X - binary equivalent.
              'M': DebugSetMemory(cmd);

              // detach, kill
              // Reset and continue MCU
              'D', 'k':
                begin
                  Done := true;
                  FDebugWire.BP := -1;
                  FDebugWire.Reset;
                  FDebugWire.Run;

                  // Detach (D) requires an acknowledge
                  if cmd[1] = 'D' then
                    gdb_response('OK');
                end;

              'q': if pos('Supported', cmd) > 0 then
                     gdb_qSupported(cmd)
                   //else if pos('fThreadInfo', cmd) > 0 then
                   //  gdb_response('l')   // end of list, i.e. none
                   else
                     gdb_response('');
              else
                gdb_response('');
            end; // case
          end;   // else
        end;     // if (i > 0) and ((count - 2) >= j)
      end;       // if (count > 0)
    except
      on e: Exception do
      begin
        Done := true;
        FLog('Exception: ' + e.Message);
      end;
    end;
  until Done;
  FLog(AddrToString(FClientStream.PeerAddress) + ' disconnected');

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
end;

procedure TGdbRspServer.FAcceptConnection(Sender: TConnectionBasedSocket; AStream: TSocketStream);
begin
  if not FActiveThreadRunning then
  begin
    FLog('Incoming connection from ' + AddrToString(AStream.PeerAddress));
    FActiveThread := TGdbRspThread.Create(AStream, FDebugWire);
    FActiveThread.OnTerminate := @FActiveThreadOnTerminate;
    FActiveThread.OnLog := @FLog;
    FActiveThreadRunning := true;
  end
  else
  begin
    FLog('Multiple connections not allowed...');
  end;
end;

procedure TGdbRspServer.FQueryConnect(Sender: TConnectionBasedSocket;
  Socket: Integer; var accept: Boolean);
begin
  if FActiveThreadRunning then
  begin
    accept := false;
    FLog('Refusing new connection - already connected');
    CloseSocket(Socket);
  end
  else
  begin
    accept := true;
    FLog('Accepting new connection');
  end;
end;

constructor TGdbRspServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnConnect := @FAcceptConnection;
  OnQueryConnect := @FQueryConnect;
  EventLoop := TEventLoop.Create;
  FActiveThreadRunning := false;

  FDebugWire := TDebugWire.Create;
  FDebugWire.OnLog := @FLog;
end;

constructor TGdbRspServer.Create(AOwner: TComponent; serialPort: string);
begin
  FSerialPort := serialPort;
  FBaud := 0;
  Create(AOwner);
end;

constructor TGdbRspServer.Create(AOwner: TComponent; serialPort: string; baud: integer);
begin
  FSerialPort := serialPort;
  FBaud := baud;
  Create(AOwner);
end;

procedure TGdbRspServer.Listen(aport: Word);
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
  end
  else  // no port name specified, scan possible candidates
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

  if targetOK then
  begin
    FDebugWire.Reset;   // reset MCU
    Self.Port := aport;
    self.Active := true;
    FLog('Listening on port: ' + IntToStr(aport));
    EventLoop.Run;    // doesn't yet close
    FLog('Closing...');
  end
  else
    FLog('Target not started...');
  //Halt;
end;

end.
