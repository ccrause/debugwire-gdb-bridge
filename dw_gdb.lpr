program dw_gdb;

uses
  {$ifdef unix}cthreads,{$endif}
  serial, crt, termio, BaseUnix, sysutils, serialutils, debugwire,
  binutils, rsp, Classes, genutils, unix;

var
  rspserver: TGdbRspServer;

begin
  rspserver := TGdbRspServer.Create(nil);
  rspserver.Listen(2345);
  rspserver.Free;
end.
