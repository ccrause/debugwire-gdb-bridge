program dw_gdb;

uses
  {$ifdef unix}cthreads,{$endif}
  serial, sysutils, serialutils, debugwire,
  binutils, rsp, getopts;

const
  DefaultTcpPort = 1234;

var
  rspserver: TGdbRspServer;
  opts: array [0..6] of TOption;
  c: char;
  TcpPort: word = DefaultTcpPort;
  ID: integer;
  baud: integer = 0;
  serialPort: string;
  DisableDWENfuse: boolean = false;
  verbose: boolean = false;

procedure printHelp;
begin
  WriteLn('Usage:');
  WriteLn('dw_gdb -S <sp> [-B <bd>] [-T <tp>] [-I] [-V] [-H]');
  WriteLn('');
  WriteLn('-S <sp>, -s <sp>, --serialport=<sp>');
  {$ifdef Windows}
  WriteLn('Connect to serial port <sp>, e.g. COM3 or \\.\COM13');
  {$else}
  WriteLn('Connect to serial port <sp>, e.g. /dev/ttyUSB0');
  {$endif}
  WriteLn('');
  WriteLn('-B <bd>, -b <bd>, --baud=<bd>');
  WriteLn('Connect to serial port using baud rate <bd>.  If not specified, the debugWIRE baud rate will be scanned automatically.');
  WriteLn('');
  WriteLn('-T <tp>, -t <tp>, --tcpport=<tp>');
  WriteLn('Set TCP port <tp> for remote connection.  If not specified, TCP port defaults to 1234.');
  WriteLn('');
  WriteLn('-I, -i, --ispenable');
  WriteLn('Temporarily disable DWEN fuse to enable ISP functionality and exit.');
  WriteLn('');
  WriteLn('-V, -v, --verbose');
  WriteLn('Enable verbose debug output.');
  WriteLn('');
  WriteLn('-H, -h, -?, --help');
  WriteLn('Display this help and exit.');
end;

procedure DisableDWEN;
var
  dw: TDebugWire;
begin
  dw := TDebugWire.Create;
  try
    if dw.Connect(serialPort, baud) then
    begin
      dw.BreakCmd;
      if dw.IdentifyTarget then
      begin
        dw.DisableDWEN;
        WriteLn('DWEN temporarily disabled until power to controller is cycled.');
        WriteLn('Connect ISP now to change fuses.');
      end
      else
        WriteLn('ERROR - Failed to connect to device.');
    end
    else
      WriteLn('ERROR - couldn''t open serial port.');
  finally
    dw.Free;
  end;
end;

begin
  opts[0].Flag := nil;
  opts[0].Has_arg := 1;
  opts[0].Name := 'serialport';
  opts[0].Value := 'S';

  opts[1].Flag := nil;
  opts[1].Has_arg := 1;
  opts[1].Name := 'baud';
  opts[1].Value := 'B';

  opts[2].Flag := nil;
  opts[2].Has_arg := 1;
  opts[2].Name := 'tcpport';
  opts[2].Value := 'T';

  opts[3].Flag := nil;
  opts[3].Has_arg := 0;
  opts[3].Name := 'help';
  opts[3].Value := 'H';

  opts[4].Flag := nil;
  opts[4].Has_arg := 0;
  opts[4].Name := 'ispenable';
  opts[4].Value := 'I';

  opts[5].Flag := nil;
  opts[5].Has_arg := 0;
  opts[5].Name := 'verbose';
  opts[5].Value := 'V';

  opts[6].Flag := nil;
  opts[6].Has_arg := 0;
  opts[6].Name := '';
  opts[6].Value := #0;

  serialPort := '';

  repeat
    c := GetLongOpts('S:s:B:b:T:t:Hh?IiVv', @opts[0], ID);
    case c of
      'S','s': serialPort := OptArg;
      'B', 'b': baud := StrToInt(OptArg);
      'T', 't': TcpPort := StrToInt(OptArg);
      'H', 'h', '?':
        begin
          printHelp;
          Halt;
        end;
      'I', 'i': DisableDWENfuse := true;
      'V', 'v': verbose := true;
    end;
  until c = EndOfOptions;

  if serialPort = '' then
  begin
    WriteLn;
    WriteLn('No serial port specified. This is mandatory.');
    WriteLn;
    Halt;
  end;

  if DisableDWENfuse then
  begin
    DisableDWEN;
  end
  else
  begin
    rspserver := TGdbRspServer.Create(TcpPort);
    rspserver.VerboseLogging := verbose;

    // Check if a debugwire connection is available over serial
    if rspserver.SerialConnect(serialPort, baud) then
    begin
      WriteLn('Waiting for TCP connection on port ' + IntToStr(rspserver.Port));
      rspserver.StartAccepting;
    end;
    WriteLn('Done...');
    rspserver.Free;
  end;
end.
