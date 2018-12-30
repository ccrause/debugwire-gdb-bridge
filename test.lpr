program test;

uses
  {$ifdef unix}cthreads,{$endif}
  serial, sysutils, serialutils, debugwire,
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
  addr: word;
  b: byte;

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

procedure dumpHexdata(start: integer; const data: TBytes);
var
  i, lenwritten: integer;
begin
  lenwritten := 0;
  while lenwritten < length(data) do
  begin
    // Address prefix
    write(HexStr(start+lenwritten, 4), ' ');
    if length(data) - lenwritten > 15 then
    begin
      for i := 0 to 15 do
        write(HexStr(data[lenwritten + i], 2), ' ');
      writeln(' ');
      inc(lenwritten, 16);
    end
    else
    begin
      for i := 0 to (high(data)-lenwritten) do
        write(HexStr(data[lenwritten + i], 2), ' ');
      writeln();
      inc(lenwritten, Length(data)-lenwritten);
    end;
  end;
end;

begin
  dw := TDebugWireLog.Create;
  dw.Connect('/dev/ttyUSB0');
  dw.BreakCmd;
  //dw.BreakCmd;

  if dw.IdentifyTarget then
  begin
    //if FileExists('/home/christo/fpc/fpc-avr/src/examples/blink2/blink.hex') then
    //begin
    //  SL :=TStringList.Create;
    //  SL.LoadFromFile('/home/christo/fpc/fpc-avr/src/examples/blink2/blink.hex');
    //  bin := readHexFile(SL);
    //  SL.Free;
    //
    //  total := 0;
    //  extAddr := 0;
    //  for i := 0 to high(bin) do
    //  begin
    //    case bin[i].recordType of
    //    brData:
    //      begin
    //        dw.MyLog(format('Writing %d bytes starting at address $%4x', [bin[i].count, bin[i].address]));
    //        dw.WriteFlash(bin[i].address, bin[i].data);
    //        total := total + bin[i].count;
    //      end;
    //    brEOF: break;
    //    else
    //      dw.MyLog('Unexpected field type: '+ IntToStr(ord(bin[i].recordType)));
    //    end;
    //  end;
    //end;


    dw.Reconnect;
    dw.ReadRegs(1, 1, data);
    dw.MyLog('R1 = ' + IntToStr(data[0]));

    addr := $0;
    SetLength(data, 1);
    data[0] := $fE;
    //data[1] := $F0;
    //dw.WriteFlash(addr, data);

    dw.MyLog('Flash:');
    i := 0;
    while i < dw.Device.flashSize do
    begin
      dw.ReadFlash(i, dw.Device.FlashPageSize, data);
      dumpHexdata(i, data);
      i := i + dw.Device.FlashPageSize;
    end;

    dw.MyLog('SRAM:');
    i := dw.Device.ioregSize + 32;
    while i < (dw.Device.sramSize + dw.Device.ioregSize + 32) do
    begin
      dw.ReadAddress(i, dw.Device.FlashPageSize, data);
      dumpHexdata(i, data);
      i := i + dw.Device.FlashPageSize;
    end;

    for i := 0 to 3 do
    begin
      dw.ReadConfig(i, b);
      dw.MyLog(format('Config #%d: %2x', [i, b]));
    end;
    dw.Run;
  end;
  dw.Free;
end.

