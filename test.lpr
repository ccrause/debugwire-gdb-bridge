program test;

uses
  {$ifdef unix}cthreads,{$endif}
  serial, crt, sysutils, serialutils, debugwire,
  binutils, rsp, genutils, getopts,
  classes;

type

  { TDebugWireLog }

  TDebugWireLog = class(TDebugWire)
    procedure MyLog(s: string);
    constructor Create;
  end;

var
  dw: TDebugWireLog;
  data: TBytes;
  SL: TStringList;
  bin: TBinArray;
  total, i, extAddr: integer;

{ TDebugWireLog }

procedure TDebugWireLog.MyLog(s: string);
var
  h, m, sec, ms: word;
begin
  DecodeTime(Now, h, m, sec, ms);
  WriteLn(Format('%.2d:%.2d:%.2d.%.3d  ', [h, m, sec, ms]) + s);
end;

constructor TDebugWireLog.Create;
begin
  inherited Create;
  OnLog := @MyLog;
end;

procedure dumpHexdata(const data: TBytes);
var
  i, lenwritten: integer;
begin
  lenwritten := 0;
  while lenwritten < length(data) do
  begin
    // Address prefix
    write(HexStr(lenwritten, 4), ' ');
    if length(data) - lenwritten > 15 then
    begin
      for i := 0 to 15 do
        write(HexStr(data[lenwritten + i], 2), ' ');
      writeln(' ');
      inc(lenwritten, 16);
    end
    else
    begin
      for i := 0 to (length(data)-lenwritten) do
        write(HexStr(data[lenwritten + i], 4), ' ');
      writeln();
      inc(lenwritten, Length(data)-lenwritten);
    end;
  end;
end;

begin
  dw := TDebugWireLog.Create;
  dw.Connect('/dev/ttyUSB0', 61000);

  if dw.IdentifyTarget then
  begin
    if FileExists('/home/christo/fpc/fpc-avr/src/examples/blink2/blink.hex') then
    begin
      SL :=TStringList.Create;
      SL.LoadFromFile('/home/christo/fpc/fpc-avr/src/examples/blink2/blink.hex');
      bin := readHexFile(SL);
      SL.Free;

      total := 0;
      extAddr := 0;
      for i := 0 to high(bin) do
      begin
        case bin[i].recordType of
        brData:
          begin
            dw.MyLog(format('Writing %d bytes starting at address $%4x', [bin[i].count, bin[i].address]));
            dw.WriteFlash(bin[i].address, bin[i].data);
            total := total + bin[i].count;
          end;
        brEOF: break;
        else
          dw.MyLog('Unexpected field type: '+ IntToStr(ord(bin[i].recordType)));
        end;
      end;
    end;

    //SetLength(data, 1);
    //data[0] := $EC;
    //dw.WriteFlash($10, data);

    dw.ReadFlash(0, 512, data);
    dumpHexdata(data);
  end;
  dw.Run;
  dw.Free;
end.

