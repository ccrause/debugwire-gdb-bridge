unit genutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FormatDataWithIndex(data: TBytes; startIndex: word; cols: byte): string;
function PrintRegs(regs: TBytes; startreg: byte): string;

implementation

function FormatDataWithIndex(data: TBytes; startIndex: word; cols: byte): string;
var
  i: integer;
  colstart, colend: boolean;
begin
  for i := 0 to Length(data)-1 do
  begin
    colstart := (i mod cols) = 0;
    colend := (i mod cols) = cols-1;

    if colstart then
      result := result + hexStr(i+startIndex, 4) + ': ' + hexStr(data[i], 2)
    else
      result := result + ' ' + hexStr(data[i], 2);

    if colend then
      result := result + LineEnding;
  end;
end;

function PrintRegs(regs: TBytes; startreg: byte): string;
begin
  result := '';
  repeat
    result := result + Format('R%.2d: $%.2x', [startreg, regs[startreg]]) + LineEnding;
    inc(startreg);
  until startreg >= length(regs);
end;

end.

