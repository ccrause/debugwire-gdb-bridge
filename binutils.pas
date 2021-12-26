unit binutils;

interface

uses
  Classes, SysUtils, debugwire;

{ General layout of a line of information in Intel hex file:
  :BBAAAATT[DDDDDDDD]CC

  where
  : is start of line marker
  BB is number of data bytes on line
  AAAA is address in bytes
  TT is data type
  DD is data bytes, number depends on BB value
  CC is checksum (2s-complement of number of bytes+address+data)
}

type
  TBinRecType = (brData=0,             // Record contains data and a 16-bit starting address
                 brEOF,                // Indicate end of file, byte count 0 and address typically 0
                 brExtSegAddr,         // Extended segment address. Data field contains 16-bit segment address (big endian).  Multiply this with 16 and add to subsequent record addresses
                 brStartSegAddr,       // For x86 this record specifies the CS:IP registers (4 bytes data)
                 brExtLinearAddr,      // 2 data bytes in big endian which form upper 16 bits of extended address for subsequent 00 records.
                 brStartLinearAddr);   // 32 bit address, loaded into EIP register for 386+ CPUs

  TBinRecord = record
    address: dword; // address of first byte
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
  typ: TBinRecType;
  numbytes: word;
  address, addressOffset: dword;
  gotEOF: boolean;
begin
  SetLength(Result, 0);
  i := 0;
  addressOffset := 0;
  gotEOF := false;

  while (SL.Count > 0) and not(gotEOF) do
  begin
    line := SL[0];
    SL.Delete(0);
    if length(line) < 1 then
      continue;

    if line[1] <> ':' then
      WriteLn('Invalid start, expected '':'' but got ', line[1])
    else
    begin
      // Read byte count, 1 byte
      s := '$' + copy(line, 2, 2);
      numbytes := StrToInt(s);
      checksum := numbytes;

      // Address, 2 bytes
      s := '$' + copy(line, 4, 2);
      data := StrToInt(s);
      address := data shl 8;
      checksum := byte(checksum + data);
      s := '$' + copy(line, 6, 2);
      data := StrToInt(s);
      address := address or data;
      checksum := byte(checksum + data);

      // Record type, 1 byte
      s := '$' + copy(line, 8, 2);
      typ := TBinRecType(StrToInt(s));

      case typ of
        brData:
          begin
            SetLength(Result, i+1);
            //Result[i].count := numbytes;
            Result[i].address := addressOffset + address;
            //Result[i].recordType := typ;    // TODO: remove this, return only data indexed by linear address
            // Data
            j := 0;
            SetLength(Result[i].data, numbytes);
            while j < numbytes do
            begin
              s := '$' + copy(line, 10+2*j, 2);
              Result[i].data[j] := StrToInt(s);
              checksum := byte(checksum + Result[i].data[j]);
              inc(j);
            end;
            inc(i);
          end;

        brEOF: gotEOF := true;

        brExtSegAddr:
          begin
            // Data, should be 2 bytes
            if numbytes <> 2 then
              writeln('Unexpected number of bytes for brExtLinearAddr');
            j := 0;
            addressOffset := 0;
            while j < numbytes do
            begin
              s := '$' + copy(line, 10+2*j, 2);
              data := StrToInt(s);
              addressOffset := (addressOffset shl 8) + data;
              checksum := byte(checksum + data);
              inc(j);
            end;
            addressOffset := addressOffset shl 4;
          end;

        brStartSegAddr:
          begin
            // CS:IP register pair - ignore for now
            // Data
            j := 0;
            while j < numbytes do
            begin
              s := '$' + copy(line, 10+2*j, 2);
              data := StrToInt(s);
              checksum := byte(checksum + data);
              inc(j);
            end;
          end;

        brExtLinearAddr:
          begin
            // Data, should be just 2 bytes
            if numbytes <> 2 then
              writeln('Unexpected number of bytes for brExtLinearAddr');
            j := 0;
            addressOffset := 0;
            while j < numbytes do
            begin
              s := '$' + copy(line, 10+2*j, 2);
              data := StrToInt(s);
              addressOffset := (addressOffset shl 8) + data;
              checksum := byte(checksum + data);
              inc(j);
            end;
            addressOffset := addressOffset shl 16;
          end;

        brStartLinearAddr:
          begin
            // Start of linear address for 386+ CPUs. Ignore for now.
            // Data
            j := 0;
            while j < numbytes do
            begin
              s := '$' + copy(line, 10+2*j, 2);
              data := StrToInt(s);
              checksum := byte(checksum + data);
              inc(j);
            end;
          end;
      else
        writeln('Unexpected HEX record type');
      end;

      // checksum
      s := '$' + copy(line, 10+2*numbytes, 2);
      data := StrToInt(s);
      checksum := byte(checksum + data);
      if (checksum <> 0) and (typ <> brEOF) then
        WriteLn('Checksum error in hex line');
    end;
  end;

  // Consolidate data runs
  i := 0;
  j := 1;
  while (j < length(Result) - 1) do
  begin
    if (Result[i].address + length(Result[i].data) = Result[j].address) then
    begin
      Result[i].data := ConCatArray(Result[i].data, Result[j].data);

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
  i: integer;
  SL: TStringList;
  binArray: TBinArray;
  memsection: word;
begin
  if FileExists(fileName) then
  begin
    SL := TStringList.Create;
    SL.LoadFromFile(fileName);
    binArray := readHexFile(SL);

    for i := 0 to length(binArray)-1 do
    begin
      memsection := hi(binArray[i].address);
      if memsection < $80 then
      begin
        WriteLn('Programming flash.');
        DW.WriteFlash(binArray[i].address, binArray[i].data)
      end
      else if memsection = $81 then
      begin
        WriteLn('Programming EEPROM.');
        DW.WriteEEPROM(binArray[i].address, binArray[i].data);
      end
      else
        writeln('Unrecognized virtual memory prefix: $', HexStr(memsection, 4));
    end;
  end;
end;

end.

