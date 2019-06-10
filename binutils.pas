unit binutils;

interface

uses
  Classes, SysUtils, debugwire;

type
  TBinRecType = (brData=0, brEOF, brExtSegAddr, brStartSegAddr, brExtLinearAddr, brStartLinearAddr);

  TBinRecord = record
    count: word;   // number of bytes in this record
    address: word; // address of first byte
    recordType: TBinRecType;
    data: TBytes;
  end;

  TBinArray = array of TBinRecord;

function readHexFile(SL: TStringList): TBinArray;
procedure programHexFile(const fileName: string; dw: TDebugWire);

implementation

function ConCatArray(a1, a2: TBytes): TBytes;
begin
  SetLength(Result, Length(a1) + Length(a2));
  FillChar(Result[0], Length(Result), 0);
  Move(a1[0], Result[0], Length(a1));
  Move(a2[0], Result[Length(a1)], Length(a2));
end;

function readHexFile(SL: TStringList): TBinArray;
var
  line, s: string;
  i, j, k: integer;
  data, checksum: byte;
begin
  SetLength(Result, 0);
  i := 0;

  while SL.Count > 0 do
  begin
    line := SL[0];
    SL.Delete(0);
    if length(line) < 1 then
      continue;

    if line[1] <> ':' then
      WriteLn('Invalid start, expected '':'' but got ', line[1])
    else
    begin
      SetLength(Result, i+1);
      // Read byte count
      s := '$' + copy(line, 2, 2);
      Result[i].count := StrToInt(s);
      checksum := Result[i].count;

      // address, 2 bytes, 4 char
      s := '$' + copy(line, 4, 2);
      Result[i].address := StrToInt(s);
      checksum := checksum + Result[i].address;
      Result[i].address := Result[i].address shl 8;
      s := '$' + copy(line, 6, 2);
      data := StrToInt(s);
      Result[i].address := Result[i].address + data;
      {$PUSH}
      {$R-}
      checksum := checksum + data;
      {$POP}

      // Record type
      s := '$' + copy(line, 8, 2);
      Result[i].recordType := TBinRecType(StrToInt(s));
      {$PUSH}
      {$R-}
      checksum := checksum + ord(Result[i].recordType);
      {$POP}

      // Data
      j := 0;
      SetLength(Result[i].data, Result[i].count);
      while j < Result[i].count do
      begin
        s := '$' + copy(line, 10+2*j, 2);
        Result[i].data[j] := StrToInt(s);
        {$PUSH}
        {$R-}
        checksum := checksum + Result[i].data[j];
        {$POP}
        inc(j);
      end;

      // checksum
      s := '$' + copy(line, 10+2*Result[i].count, 2);
      data := StrToInt(s);
      {$PUSH}
      {$R-}
      checksum := checksum + data;
      {$POP}
      if checksum <> 0 then
        WriteLn('Checksum error in hex line');

      inc(i);
    end;
  end;

  // Consolidate data runs
  i := 0;
  j := 1;
  while (j < length(Result) - 1) do
  begin
    if (Result[j].recordType = Result[i].recordType) and
       (Result[i].address + Result[i].count = Result[j].address) then
    begin
      Result[i].data := ConCatArray(Result[i].data, Result[j].data);
      Result[i].count := Result[i].count + Result[j].count;

      // Naive compaction, could probably just let 2nd index increment and truncate at end
      for k := j to length(Result)-2 do
      begin
        Result[k] := Result[k+1];
      end;
      SetLength(Result, length(Result)-1);
    end
    else
    begin
      i := j;
      inc(j);
    end;
  end;
end;

procedure programHexFile(const fileName: string; dw: TDebugWire);
var
  i, j: integer;
  SL: TStringList;
  binArray: TBinArray;
begin
  if FileExists(fileName) then
  begin
    SL := TStringList.Create;
    SL.LoadFromFile(fileName);
    binArray := readHexFile(SL);

    for i := 0 to length(binArray)-1 do
    begin
      case binArray[i].recordType of
        brData : DW.WriteFlash(binArray[i].address, binArray[i].data);
        brEOF  : WriteLn('End of Hex data');
      else
        WriteLn('Unexpected record type for hex record', binArray[i].recordType);
      end;
    end;
  end;
end;

end.

