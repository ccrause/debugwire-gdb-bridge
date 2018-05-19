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
  reply := 'hwbreak+';
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
  FDebugWire.Run;
  FDebugState := dsRunning;
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
  SetLength(data, len);  // TODO: check consistency between len & l1
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
  Done: Boolean;
  buf: array[0..1023] of char;
  count, i, j, idstart, idend, addr: integer;
  rdfd: TFDSet;
  timeout: TTimeVal;
  maxhandle: integer;
  serhandle, tcphandle: THandle;
begin
  Done := false;
  msg := '';

  timeout.tv_sec := 5;
  timeout.tv_usec := 0;
  fpFD_ZERO(rdfd);
  fpFD_SET(FClientStream.Handle, rdfd);
  maxhandle := FClientStream.Handle;
  fpFD_SET(FDebugWire.Serial.SerialHandle, rdfd);
  if FDebugWire.Serial.SerialHandle > maxhandle then
    maxhandle := FDebugWire.Serial.SerialHandle;

  repeat
    try
      timeout.tv_sec := 5;
      timeout.tv_usec := 0;
      serhandle := FDebugWire.Serial.SerialHandle;
      tcphandle := FClientStream.Handle;
      fpFD_ZERO(rdfd);
      fpFD_SET(tcphandle, rdfd);
      maxhandle := tcphandle;
      fpFD_SET(serhandle, rdfd);
      if serhandle > maxhandle then
        maxhandle := serhandle;

      if fpSelect(maxhandle+1, @rdfd, nil, nil, @timeout) > 0 then
      begin
        if fpFD_ISSET(tcphandle, rdfd) > 0 then  // TCP data available
        begin
          count := FClientStream.Read(buf[0], length(buf));
        end;

        if (FDebugState = dsRunning) and (fpFD_ISSET(serhandle, rdfd) > 0) then // Serial data available
        begin
          if FDebugWire.TrySync then
          begin
            FDebugState := dsPaused;
            FDebugWire.Reconnect;
            gdb_response('S05');
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
              '?': gdb_response('S05');

              // continue
              'c': begin
                     DebugContinue;
                     gdb_response('OK');
                   end;

              // step
              's': begin
                     DebugStep;
                     gdb_response('S05');
                   end;

              // read general registers 32 registers + SREG + PCL + PCH + PCHH
              'g': DebugGetRegisters;

              // write general registers 32 registers + SREG + PCL + PCH + PCHH
              'G': DebugSetRegisters(cmd);

              // delete breakpoint
              'z': begin
                     if (cmd[2] = '1') and (FDebugWire.BP > -1) then  // only delete HW breakpoint, SW BP not supported internally yet
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
                     if (cmd[2] = '1') and (FDebugWire.BP = -1) then // only hardware breakpoint accepted - > gdb will then manage software breakpoints from outside
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
                   else
                     gdb_response('');
              else
                gdb_response('');
            end; // case
          end;   // else
        end;     // if (i > 0) and ((count - 2) >= j)
      end;       // if (count > 0)
    except
      on e: EStreamError do
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
