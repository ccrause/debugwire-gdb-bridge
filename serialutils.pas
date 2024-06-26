unit serialutils;

interface

uses
  serial, sysutils;

const
  NCCS = 19;
  BAUDforManualBreak = 9600;

type
  {$PACKRECORDS C}
    Termios_ = record
      c_iflag,
      c_oflag,
      c_cflag,
      c_lflag  : cardinal;
      c_line   : char;
      c_cc     : array[0..NCCS-1] of byte;
      c_ispeed,
      c_ospeed : cardinal;
    end;

  { TSerialObj }

  TBreakType = (btShort, btStandard, btSendZero);

  TSerialObj = class
  private
    FBaudRate: integer;
    {$IFNDEF WINDOWS}Ftios: Termios_;{$ENDIF}
    FBreakType: TBreakType;
    FBreakBaudRate: integer;
    FHandle: THandle;
    procedure FSetParams(ByteSize: Integer; Parity: TParityType; StopBits: Integer;
      Flags: TSerialFlags);
    procedure FUpdateBaudRate(aBaud: integer);
  public
    portName: string;  // full name, e.g. /dev/ttyUSB0 or COM1
    function OpenPort(aPortName: string; baud: integer): boolean;
    procedure Break;
    function Read(var Buffer; Count: LongInt): integer;
    function ReadTimeout(var Buffer: array of byte; count, mSec: LongInt): integer;
    function Write(const Buffer; Count: LongInt): LongInt;

    procedure Flush;
    procedure FlushInput;
    procedure FlushOutput;

    constructor Create;
    destructor Destroy; override;

    property BaudRate: integer read FBaudRate write FUpdateBaudRate;
    property BreakBaudRate: integer read FBreakBaudRate write FBreakBaudRate;
    property SerialHandle: THandle read FHandle;
  end;

const
  TCGETS2 = $802C542A; //longint(2150388778);
  TCSETS2 = $402C542B; //1076646955;

implementation

uses
  {$IFDEF UNIX}
  termio, BaseUnix, errors;
  {$ELSE}
  windows;
  {$ENDIF}

{ TSerialObj }

{$ifdef windows}
// Need Windows equivalent still
// Check if custom baud is supported on Windows by checking COMMPROP.dwMaxBaud (https://msdn.microsoft.com/en-us/library/windows/desktop/aa363189(v=vs.85).aspx)
// Set custom baud directly in DCB.BaudRate (https://msdn.microsoft.com/en-us/library/windows/desktop/aa363214(v=vs.85).aspx)
procedure TSerialObj.FSetParams(ByteSize: Integer; Parity: TParityType;
  StopBits: Integer; Flags: TSerialFlags);
begin
  SerSetParams(FHandle, BaudRate, ByteSize, Parity, StopBits, Flags);
end;

{$else}
// For custom baud rate on Linux see e.g. 
// https://www.downtowndougbrown.com/2013/11/linux-custom-serial-baud-rates/
procedure TSerialObj.FSetParams(ByteSize: Integer; Parity: TParityType;
  StopBits: Integer; Flags: TSerialFlags);
var
  customBAUD: boolean;
begin
  FillChar(Ftios, SizeOf(Ftios), #0);
  customBAUD := false;

  case baudRate of
    50: Ftios.c_cflag := B50;
    75: Ftios.c_cflag := B75;
    110: Ftios.c_cflag := B110;
    134: Ftios.c_cflag := B134;
    150: Ftios.c_cflag := B150;
    200: Ftios.c_cflag := B200;
    300: Ftios.c_cflag := B300;
    600: Ftios.c_cflag := B600;
    1200: Ftios.c_cflag := B1200;
    1800: Ftios.c_cflag := B1800;
    2400: Ftios.c_cflag := B2400;
    4800: Ftios.c_cflag := B4800;
    19200: Ftios.c_cflag := B19200;
    38400: Ftios.c_cflag := B38400;
    57600: Ftios.c_cflag := B57600;
    115200: Ftios.c_cflag := B115200;
    230400: Ftios.c_cflag := B230400;
{$ifndef BSD}
    460800: Ftios.c_cflag := B460800;
{$endif}
    else
    begin
      customBAUD := true;
      Ftios.c_cflag     := CBAUDEX or CLOCAL;
      Ftios.c_iflag     := IGNPAR or IGNBRK;
      Ftios.c_oflag     := 0;
      Ftios.c_lflag     := 0;
      Ftios.c_ispeed    := baudRate;
      Ftios.c_ospeed    := baudRate;
      Ftios.c_cc[VMIN]  := 0;           // Return as soon as one byte is available
      Ftios.c_cc[VTIME] := 10;//5;           // 0.5 seconds timeout per byte
    end;
  end;

  Ftios.c_cflag := Ftios.c_cflag or CREAD or CLOCAL;

  case ByteSize of
    5: Ftios.c_cflag := Ftios.c_cflag or CS5;
    6: Ftios.c_cflag := Ftios.c_cflag or CS6;
    7: Ftios.c_cflag := Ftios.c_cflag or CS7;
    else Ftios.c_cflag := Ftios.c_cflag or CS8;
  end;

  case Parity of
    OddParity: Ftios.c_cflag := Ftios.c_cflag or PARENB or PARODD;
    EvenParity: Ftios.c_cflag := Ftios.c_cflag or PARENB;
  end;

  if StopBits = 2 then
    Ftios.c_cflag := Ftios.c_cflag or CSTOPB;

  if RtsCtsFlowControl in Flags then
    Ftios.c_cflag := Ftios.c_cflag or CRTSCTS;

  tcflush(FHandle, TCIOFLUSH);
  if customBAUD then
    FpIOCtl(FHandle, TCSETS2, @Ftios)
  else
    tcsetattr(FHandle, TCSANOW, Termios(pointer(@Ftios)^));
end;
{$endif}

procedure TSerialObj.FUpdateBaudRate(aBaud: integer);
begin
  FBaudRate := aBaud;
  self.FSetParams(8, NoneParity, 1, []);
end;

procedure TSerialObj.Break;
var
  status: {$IFDEF WINDOWS}longbool;{$ELSE}integer;{$ENDIF}
  b: word;
begin
  if FBreakType = btShort then
  begin
    {$IFDEF WINDOWS}
    status := SetCommBreak(FHandle);
    {$ELSE}
    status := FpIOCtl(FHandle, TIOCSBRK, pointer(ptrint(0)));
    {$ENDIF}

    // Stretch break signal at lower baud rates
    if FBaudRate > 50000 then
      sleep(1)
    else if FBaudRate > 25000 then
      sleep(2)
    else if FBaudRate > 12000 then
      sleep(3)
    else
      sleep(4);

    {$IFDEF WINDOWS}
    status := ClearCommBreak(FHandle);
    {$ELSE}
    status := FpIOCtl(FHandle, TIOCCBRK, pointer(ptrint(0)));
    {$ENDIF}
    SerSync(FHandle);

    if {$IFDEF WINDOWS} not status {$ELSE} status <> 0 {$ENDIF}then // possible that break isn't supported by driver or chip?
    begin
      FBreakType := {$IFDEF WINDOWS}btSendZero{$ELSE}btStandard{$ENDIF};
      WriteLn('ERROR - break status: ', {$IFNDEF WINDOWS}StrError(errno){$ELSE} IntToStr(GetLastOSError) {$ENDIF});
      WriteLn('Switching to standard break signal');
      SerFlush(FHandle);    // clear buffers
    end;
  end;

  {$IFNDEF WINDOWS}
  if FBreakType = btStandard then
  begin
    status := TCSendBreak(FHandle, 0);
    SerSync(FHandle);

    if status <> 0 then // possible that break isn't supported by driver or chip?
    begin
      FBreakType := btSendZero;
      WriteLn('ERROR - break status: ', StrError(errno));
      WriteLn('Switching to sending #0 at low BAUD');
      SerFlush(FHandle);    // clear buffers
    end;
  end;
  {$ENDIF}

  if FBreakType = btSendZero then
  begin
    b := 0;
    SerSync(FHandle);
    self.BaudRate := FBreakBaudRate;
    //sleep(1);
    SerWrite(FHandle, b, 1);
    //sleep(10);                    // TODO: is delay necessary?
    self.BaudRate := FBaudRate;
  end;
end;

function TSerialObj.OpenPort(aPortName: string; baud: integer): boolean;
begin
  portName := aPortName;
  {$IFNDEF WINDOWS}
  FHandle := FpOpen(portName, O_RDWR or O_NOCTTY or O_NONBLOCK);
  {$ELSE}
  FHandle := SerOpen(aPortName);
  {$ENDIF}

  if {$IFNDEF WINDOWS}(FHandle < 0) {$ELSE} (FHandle = ERROR_INVALID_HANDLE) {$ENDIF} then
  begin
    WriteLn('Couldn''t open serial port: ', portname);
    result := false;
    portName := '';
  end
  else
  begin
    BaudRate := baud;
    Result := true;
  end;
end;

function TSerialObj.Read(var Buffer; Count: LongInt): integer;
begin
  result := SerRead(FHandle, Buffer, Count);
end;

function TSerialObj.ReadTimeout(var Buffer: array of byte; count, mSec: LongInt): integer;
begin
  result := SerReadTimeout(FHandle, Buffer[0], count, mSec);
end;

function TSerialObj.Write(const Buffer; Count: LongInt): LongInt;
begin
  result := SerWrite(FHandle, Buffer, Count);
end;

procedure TSerialObj.Flush;
begin
  SerFlush(FHandle);
end;

procedure TSerialObj.FlushInput;
begin
  SerFlushInput(FHandle);
end;

procedure TSerialObj.FlushOutput;
begin
  SerFlushOutput(FHandle);
end;

constructor TSerialObj.Create;
begin
  inherited Create;

  {$IFNDEF WINDOWS}FHandle := -1{$ELSE}FHandle := ERROR_INVALID_HANDLE{$ENDIF};
  FBreakType := btShort;  // use shorter break sequence
  FBreakBaudRate := BAUDforManualBreak; // baud rate for sending 0 to simulate a break signal
end;

destructor TSerialObj.Destroy;
begin
  if {$IFNDEF WINDOWS}FHandle > -1{$ELSE}FHandle > ERROR_INVALID_HANDLE{$ENDIF} then
  begin
    SerClose(FHandle);
    FHandle := 0;
  end;

  inherited Destroy;
end;

end.

