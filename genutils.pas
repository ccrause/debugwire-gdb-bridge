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
  result := '';
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
var
  s1, s2: string;
begin
  result := '';
  s1 := '';
  s2 := '';
  repeat
    s1 := s1 + Format('R%.2d ', [startreg]);
    s2 := s2 + Format('$%.2x ', [regs[startreg]]);
    inc(startreg);
    if startreg mod 16 = 0 then
    begin
      result := result + s1 + LineEnding + s2 + LineEnding + LineEnding;
      s1 := '';
      s2 := '';
    end;
  until startreg >= length(regs);

  if s1 <> '' then
    result := result + s1 + LineEnding + s2 + LineEnding;
end;

end.

