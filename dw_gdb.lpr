program dw_gdb;

uses
  {$ifdef unix}cthreads,{$endif}
  serial, crt, sysutils, serialutils, debugwire,
  binutils, rsp, genutils;

var
  rspserver: TGdbRspServer;

begin
  rspserver := TGdbRspServer.Create(nil, '\COM3', 62500);
  // Keep server running, FQueryConnect will reject connections while TGdbRspThread is running with current connection
  // Server will close down once MaxConnections is reached.  Could then wait on Connection thread to finish?
  if rspserver.MaxConnections <> 0 then
  begin
    WriteLn('Start accepting...');
    rspserver.StartAccepting;
  end;
  WriteLn('Freeing...');
  rspserver.Free;
end.
