unit debugwire;
// Code works with FT232 chip (Arduino Nano)

// Kind of works with Pololu chip (doesn't support TCSendBreak, fake by writing 0)
// - Custom BAUD setting works
// - Break mostly works by writing 0 to serial port
// - Cannot reliably read responses
// - Seems to work when starting BAUD is correct

// Arduino Uno with Atmega16U2 doesn't really work
// - Custom BAUD setting works
// - Break doesn't often work by writing 0 to serial port, lots of spurious noise?
// - Seems that changing BAUD rate causes noise on next character written/read?

// CP2102 USB-serial converter works
// - custom BAUD seems to work OK

interface

uses
  serialutils, SysUtils;

type
  TDeviceInfo = record
    name: string;
    ID: word;
    ioregSize,
    sramSize,
    eepromSize,
    EepromPageSize,
    flashSize,  // In bytes
    FlashPageSize,
    DWDR,       // DebugWIRE data register, aka MONDR - Monitor data register
    bootStart,  // Lowest PC value giving boot section access
    bootflags,  // Where to find the boot sector control flags, if any
    EECR,       // EEPROM control register index. EEDR and EEARL always follow directly.
    EEARH: integer;      // EEPROM address high (doesn't exist on all devices)
  end;

  TLog = procedure (s: string) of object;
  { TDebugWire }

  TDebugWire = class
  private
    //FBaud: integer;
    FSer: TSerialObj;
    FAddrFlag: byte;
    FTimersDisabled: boolean;
    FTimersMask: byte;
    FDevice: TDeviceInfo;
    FPC: word;
    FBP: integer;  // Single hardware breakpoint
    //FFlashPageBuffer: TBytes;
    FCachedRegs: TBytes; // Cache of R28:R31 which gets changed in some debugwire transactions.
    FLogger: TLog;

    FOutBuffer: array[0..255] of byte;
    FOutBufCount: integer;

    procedure FLog(s: string);

    function FBaudScale(data: byte): integer;
    procedure FTimersDisabledProc(val: boolean);

    procedure FSetBaud(aBaud: integer);
    function FReadBaud: integer;

    // Reads data from hardware
    procedure FReadAddress(const start, count: word; out values: TBytes);

    // First load page from flash and check for differences
    // First, erase if a 0 -> 1 bit transition is required
    // then perform write
    // Requires startAddress to fall on page boundary
    procedure FWriteFlashPage(startAddress: word; const page: TBytes);
    procedure FEraseFlashPage(startAddress: word);

    procedure FReEnabelRWW;  // require follow-up FPushSerialBuffer
    procedure FLoadPageBuffer(address: word; values: TBytes);
    procedure FWritePageBuffer(address: word; values: TBytes);

    function FReadSPMCSR: byte;
    procedure SetBreakPoint(const addr: word);  // require follow-up FPushSerialBuffer

    // Buffer serial data that doesn't require an immediate response
    // to increase speed
    procedure FBufferedSerialWrite(var data; count: integer);
    procedure FPushSerialBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(portName: string; baud: integer): boolean;
    function Connect(portName: string): boolean;
    function ScanTargetBaud: boolean;
    function BreakResponse: byte;
    procedure BreakCmd;
    procedure SendData(const data: byte);
    procedure SendData(const data: TBytes);
    procedure ReadData(const count: word; out values: TBytes);
    function IdentifyTarget: Boolean;

    procedure SetPC(addr: word);   // require follow-up FPushSerialBuffer

    procedure SetZreg(const val: word); // require follow-up FPushSerialBuffer

    // Use with caution, may change R29...R31
    procedure ReadRegs(const start, count: byte; out values: TBytes);
    procedure WriteRegs(const start: byte; const values: TBytes);  // require follow-up FPushSerialBuffer

    // regs, IO & SRAM
    // Reads all data from hardware, except R28-R32 & DWDR registers
    procedure ReadAddress(const start, count: word; out values: TBytes);
    procedure WriteAddress(startAddress: word; const values: TBytes);

    procedure ReadFlash(const start, count: word; out values: TBytes);
    procedure WriteFlash(start: word; const values: TBytes);

    procedure SendInstruction16(const instr: word);
    // Generate op code for OUT instruction and execute
    procedure OutInstruction(const IORegAddr: byte; const sourceReg: byte);
    // Generate op code for IN instruction and execute
    procedure InInstruction(const destReg: byte; const IORegAddr: byte);
    procedure ReadConfig(const index: byte; out value: byte);

    procedure Reset;
    // Wait for break & $55, typically returned after hitting break point or waiting for slow operation such as SPM
    // Blocking until break is received
    procedure Sync;
    // Nonblocking check for break - doesn't work great, often misses break conditions
    function TrySync: boolean;

    // Read current PC, cache R28-R31 (these get clobbered in memory access)
    procedure Reconnect;

    // Run target and wait for a break signal
    procedure ContinueUntilBreak;
    // Run target and return immediately
    procedure Run;
    // Execute single step and wait for break signal
    procedure Step;

    property PortName: string read FSer.portName;
    property Device: TDeviceInfo read FDevice;
    property TimersDisabled: boolean read FTimersDisabled write FTimersDisabledProc;
    property BaudRate: integer read FReadBaud write FSetBaud;
    property PC: word read FPC write FPC;
    property BP: integer read FBP write FBP;
    property OnLog: TLog read FLogger write FLogger;

    // for debugging only
    property Serial: TSerialObj read FSer;
  end;

implementation

uses
  {$IFNDEF WINDOWS}
  baseunix, errors, unix, math;
  {$ELSE}
  windows;
  {$ENDIF}

const
  FRegCacheStart = 28;   // Start caching from R28
  FRegCacheLength = 4;   // Cache up to R31

  SPMCSR = $37;        // used for in/out instruction, so don't add $20 offset

  DeviceInfo: array[0..16] of TDeviceInfo =
    ((name: 'ATtiny13';   ID: $9007; ioregSize:  64; sramSize:  64; eepromSize:  64; EepromPageSize: 4; flashSize: 1024; FlashPageSize: 32; DWDR: $2E; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $00),
     (name: 'ATtiny2313'; ID: $910a; ioregSize:  64; sramSize: 128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $1f; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $00),
     (name: 'ATtiny24';   ID: $910B; ioregSize:  64; sramSize: 128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATtiny44';   ID: $9207; ioregSize:  64; sramSize: 256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATtiny84';   ID: $930C; ioregSize:  64; sramSize: 512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATtiny25';   ID: $9108; ioregSize:  64; sramSize: 128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATtiny45';   ID: $9206; ioregSize:  64; sramSize: 256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATtiny85';   ID: $930B; ioregSize:  64; sramSize: 512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F),
     (name: 'ATmega48A';  ID: $9205; ioregSize: 224; sramSize: 512; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $31; bootStart: $0000; bootflags: 0; EECR: $1F; EEARH: $22),
     (name: 'ATmega48PA'; ID: $920A; ioregSize: 224; sramSize: 512; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $31; bootStart: $0000; bootflags: 0; EECR: $1F; EEARH: $22),

     (name: 'ATmega88A';  ID: $930A; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $31; bootStart: $0F80; bootflags: 1; EECR: $1F; EEARH: $22),
     (name: 'ATmega88PA'; ID: $930F; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $31; bootStart: $0F80; bootflags: 1; EECR: $1F; EEARH: $22),

     (name: 'ATmega168A'; ID: $9406; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 16384;FlashPageSize: 128;DWDR: $31; bootStart: $1F80; bootflags: 1; EECR: $1F; EEARH: $22),
     (name: 'ATmega168PA';ID: $940B; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 16384;FlashPageSize: 128;DWDR: $31; bootStart: $1F80; bootflags: 1; EECR: $1F; EEARH: $22),
     (name: 'ATmega328P'; ID: $950F; ioregSize: 224; sramSize: 2048; eepromSize: 1024;EepromPageSize: 4; flashSize: 32768;FlashPageSize: 128;DWDR: $31; bootStart: $3F80; bootflags: 2; EECR: $1F; EEARH: $22),
     (name: 'ATmega328';  ID: $9514; ioregSize: 224; sramSize: 2048; eepromSize: 1024;EepromPageSize: 4; flashSize: 32768;FlashPageSize: 128;DWDR: $31; bootStart: $3F80; bootflags: 2; EECR: $1F; EEARH: $22),

     (name: 'ATtiny441';  ID: $9215; ioregSize: 224; sramSize: 256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 16; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEARH: $1F));

  // used as pointer for r/w operations
  REG_Z = 30;
  SREG = $5F;  // $20 offset already added

  // debugWIRE commands
  CMD_DISABLE = $06;  // disable debugwire/enable ISP - stay set until power cycle
  CMD_RESET = $07;    // reset MCU
  CMD_GO_RW = $20;    // read/write memory
  CMD_SS_IR = $23;    // execute single instruction loaded into instruction register with CMD_SET_IR
  CMD_RUN = $30;      // Normal code execution
  CMD_SS = $31;       // Single step code execution
  CMD_SS_SPM = $33;   // execute single step slow instruction (SPM)
  CMD_SS_SETUP = $64; // setup single step from loaded instruction
  CMD_RW_SETUP = $66; // setup read/write with repeated instructions
  CMD_RW_MODE = $C2;  // set byte size to instruction register
  CMD_SET_PC = $D0;   // set word size program counter
  CMD_SET_BP = $D1;   // set word size break point
  CMD_SET_IR= $D2;    // set word size to instruction register
  CMD_READ_PC = $F0;  // read word size program counter
  CMD_READ_SIG = $F3; // read word size signature

  CMD_GO_CONTEXT = $40;
  FLAG_TIMERS_DISABLE = $20;

  // CMD_RW_MODE modes
  RW_MODE_READ_SRAM = $00;
  RW_MODE_READ_REGS = $01;
  RW_MODE_READ_FLASH = $02;
  RW_MODE_WRITE_SRAM = $04;
  RW_MODE_WRITE_REGS = $05;

  // SPMCSR register bits (attiny/atmega nomenclature)
  SPMEN = $01;      // Store program memory enable (ATTINY) only?
  SELFPRGEN = $01;  // Self programming enable (ATMEGA) only?
  PGERS = $02;      // Page erase
  PGWRT = $04;      // Page write
  RFLB = $08;       // Read fuse and lock bits
  BLBSET = $08;     // Boot lock bit set  (ATMEGA)?
  CTPB = $10;       // Clear temporary buffer
  RWWSRE = $10;     // Read-While-Write Section Read Enable
  RSIG = $20;       // Read device signature imprint table - no equivalent bit for atmega?
  RWWWSB = $40;     // Read-While-Write Section Busy
  SPMIE = $80;      // SPM Interrupt Enable - for completeness only

  // Opcodes for commonly used instructions
  OpCode_Break = $9598;
  OpCode_SPM   = $95E8;

function ConCatArray(a1, a2: TBytes): TBytes;
begin
  SetLength(Result, Length(a1) + Length(a2));
  FillChar(Result[0], Length(Result), 0);
  if length(a1) > 0 then
    Move(a1[0], Result[0], Length(a1));
  if length(a2) > 0 then
  Move(a2[0], Result[Length(a1)], Length(a2));
end;


{ TDebugWire }

procedure TDebugWire.FLog(s: string);
begin
  if Assigned(FLogger) then
    Flogger(s);
end;

function TDebugWire.FBaudScale(data: byte): integer;
var
  i, j, currentbit: integer;
  runlength: array of integer;
begin
  if data = $55 then // Expected value, no need to scale
    Result := 100
  else if (data = 0) or (data = 255) then  // far off or bogus value, scale by large amount. Should not get these values
    Result := 50
  else  // check ContinueUntilBreak lengths of 1's and 0's
  begin
    i := 0;
    j := 0;
    SetLength(runlength, j+1);
    runlength[j] := 0;
    currentbit := (data and 1);
    data := data shr 1;

    repeat
      inc(i);
      if (data and 1) = currentbit then
        inc(runlength[j])
      else
      begin
        inc(j);
        SetLength(runlength, j+1);
        runlength[j] := 0;
        currentbit := (data and 1);
      end;

      data := data shr 1;
    until i = 7;

     //at least one change in bit pattern, since 0 and 255 are excluded here
     case length(runlength) of
       7: Result := 97;
       6: result := 90;
       5: result := 85;
       4: result := 80;
       3: result := 70;
       2: result := 60;
     end;
  end;
end;

procedure TDebugWire.FTimersDisabledProc(val: boolean);
begin
  FTimersDisabled := val;
  if val then
    FTimersMask := FLAG_TIMERS_DISABLE
  else
    FTimersMask := 0;
end;

procedure TDebugWire.FSetBaud(aBaud: integer);
begin
  FSer.BaudRate := aBaud;
end;

function TDebugWire.FReadBaud: integer;
begin
  Result := FSer.BaudRate;
end;

constructor TDebugWire.Create;
begin
  FSer := TSerialObj.Create;
  FTimersDisabled := false;
  FTimersMask := 0;
  FillChar(FDevice, sizeof(FDevice), 0);
  FPC := 0;
  FBP := -1;
  SetLength(FCachedRegs, FRegCacheLength);
end;

destructor TDebugWire.Destroy;
begin
  FreeAndNil(FSer);
  inherited Destroy;
end;

function TDebugWire.Connect(portName: string; baud: integer): boolean;
begin
  if baud > 0 then
    result := FSer.OpenPort(portName, baud)
  else
    result := Connect(portName);
end;

function TDebugWire.Connect(portName: string): boolean;
begin
  if Connect(portName, 200000) then
  begin
    result := ScanTargetBaud;
  end;
end;

function TDebugWire.ScanTargetBaud: boolean;
var
  BAUDscanDone: boolean;
  scale, oldscale, retries, data: integer;
  upperBaud, lowerBaud: integer;
begin
  BAUDscanDone := false;
  oldscale := 0;
  retries := 0;
  repeat
    if {$IFNDEF WINDOWS}FSer.SerialHandle < 0{$ELSE}not (FSer.SerialHandle > ERROR_INVALID_HANDLE){$ENDIF} then
    begin
      FLog('Invalid serial handle: ' + IntToStr(FSer.SerialHandle));
      system.Break;
    end;

    data := BreakResponse;
    if data > 0 then
    begin
      scale := FBAUDscale(data);
      if scale = 100 then
        BAUDscanDone := true
      else
      begin
        if scale < oldscale then
        begin
          FLog('Scale increase predicted, ignore');
          scale := 97;
        end
        else
          oldscale := scale;

        FLog('Scale = ' + IntToStr(scale) + '%');
        FSer.BaudRate := (FSer.BaudRate * scale) div 100;
      end;
    end
    else
    begin // invalid response
      inc(retries);
      FLog('Invalid response, retry #' + IntToStr(retries));
    end;

    FSer.FlushInput;
  until (retries > 2) or BAUDscanDone;

  if (BAUDscanDone) then
  begin
    FLog('Success, BAUD = ' + IntToStr(FSer.BaudRate));
    FLog('Scanning for upper bound...');
    upperBAUD := FSer.BaudRate;
    lowerBAUD := FSer.BaudRate;

    repeat
      FSer.BaudRate := (FSer.BaudRate * 102) div 100;
      data := BreakResponse;
      if data = $55 then
        upperBAUD := FSer.BaudRate;
      FSer.FlushInput;
    until data <> $55;

    FLog('Scanning for lower bound...');
    FSer.BaudRate := lowerBAUD;
    repeat
      FSer.BaudRate := (FSer.BaudRate * 100) div 102;
      data := BreakResponse;
      if data = $55 then
        lowerBAUD := FSer.BaudRate;
      FSer.FlushInput;
    until data <> $55;
    FSer.BaudRate := (lowerBAUD + upperBAUD) div 2;
    FLog('BAUD estimate = ' + IntToStr(FSer.BaudRate));
    result := true;
  end
  else
  begin
    FLog('Giving up...');
    result := false;
  end;
end;

function TDebugWire.BreakResponse: byte;
var
  buf: array[0..1] of byte;
  status: integer;
begin
  FillChar(buf[0], length(buf), 0);

  FSer.Break;

  status := 1;
  while (status > 0) and ((buf[0] = 0) or (buf[0] = 255)) do
  begin
    status := FSer.ReadTimeout(buf[0], 1, 100);
  end;

  if (buf[0] > 0) and (buf[0] < 255) then
  begin
    FLog('BAUD: ' + IntToStr(FSer.BaudRate) + '. Data: ' + IntToStr(buf[0]) + '. Bin: ' + binStr(buf[0], 8));
    Result := buf[0];
  end
  else
    result := 0;
end;

procedure TDebugWire.BreakCmd;
var
  buf: array[0..9] of byte;
  status: integer;
begin
  FillChar(buf[0], length(buf)*sizeof(buf[0]), 0);

  FSer.Break;

  // Should receive echo of 0, followed by $55
  repeat
    status := FSer.ReadTimeout(buf[0], 1, 50);
  until (buf[0] <> 0) or (status = 0);

  if buf[0] <> 85 then
    FLog('Expected 85 but recv: ' + IntToStr(buf[0]))
  else if status = 0 then
    FLog('Didn''t receive expected response for BREAK signal.');
end;

procedure TDebugWire.SendData(const data: byte);
var
  echo: byte;
begin
  echo := data; // hack to convert const to memory so that a reference can be passed to FBufferedSerialWrite
  FBufferedSerialWrite(echo, 1);
end;

procedure TDebugWire.SendData(const data: TBytes);
begin
  FBufferedSerialWrite(data[0],length(data));
end;

procedure TDebugWire.ReadData(const count: word; out values: TBytes);
var
  len, l2: byte;
begin
  SetLength(values, count);
  FPushSerialBuffer; // empty last bit of buffered data

  len := FSer.ReadTimeout(values[0], count, 10*count);
  if len < count then
  begin
    FLog('ReadData: len < count.  OS error: ' + IntToStr(GetLastOSError) {+ ' - ' + StrError(GetLastOSError)});
    l2 := FSer.ReadTimeout(values[len], count - len, 10*(count - len));
    len := len + l2
  end;

  SetLength(values, len);
end;

function TDebugWire.IdentifyTarget: Boolean;
var
  r: byte;
  id: word;
  data: TBytes;
begin
  if FDevice.ID > 0 then
  begin
    Result := true;
  end
  else
  begin
    SendData(CMD_READ_SIG);
    ReadData(2, data);
    r := length(data);

    FillChar(FDevice, sizeof(FDevice), 0);

    Result := false;
    if r = 2 then
    begin
      id := data[0] shl 8 + data[1];

      r := 0;
      repeat
        inc(r);
      until (r = length(DeviceInfo)) or (DeviceInfo[r].ID = id);
      if r < length(DeviceInfo) then
      begin
        Result := true;
        FDevice := DeviceInfo[r];

        FLog('Device ID: $' + hexStr(id, 4) + ' ['+FDevice.name+']');
        if FDevice.flashSize < 8192 then
          FAddrFlag := $10
        else
          FAddrFlag := 0;
      end
      else
        FLog('Device not found in DeviceInfo list: $' + hexStr(id, 4));
    end
    else
    begin
      FLog('Unexpected response for DWGetID - bytes read = ' + IntToStr(r));
    end;
  end;
end;

procedure TDebugWire.SetBreakPoint(const addr: word);
var
  cmd: TBytes;
begin
  SetLength(cmd, 3);
  cmd[0] := CMD_SET_BP;
  cmd[1] := hi(addr) ;//or FAddrFlag;
  cmd[2] := lo(addr);
  SendData(cmd);
end;

procedure TDebugWire.FBufferedSerialWrite(var data; count: integer);
var
  dataIndex, bufferspace: integer;
  byteArray: TByteArray absolute data;
begin
  bufferspace := length(FOutBuffer) - FOutBufCount;
  dataIndex := 0;
  while FOutBufCount + count >= length(FOutBuffer) do  // enough data for a full buffer transfer?
  begin
    Move(byteArray[dataIndex], FOutBuffer[FOutBufCount], bufferspace);
    FOutBufCount := length(FOutBuffer);
    FPushSerialBuffer;  // this will set FOutBufCount to 0 after writing

    dataIndex := bufferspace;
    count := count - bufferspace;
    bufferspace := length(FOutBuffer) - FOutBufCount;
  end;

  Move(byteArray[dataIndex], FOutBuffer[FOutBufCount], count);
  FOutBufCount := FOutBufCount + count;
end;

procedure TDebugWire.FPushSerialBuffer;
var
  echo: TBytes;
  count, i: integer;
begin
  if FOutBufCount > 0 then
  begin
    SetLength(echo, FOutBufCount);
    FSer.Write(FOutBuffer[0], FOutBufCount);
    FSer.Flush;

    count := FSer.ReadTimeout(echo[0], FOutBufCount, 100);

    if (count < FOutBufCount) then
    begin
      count := count + FSer.ReadTimeout(echo[count], FOutBufCount - count, 100);
      if count < FOutBufCount then
      FLog('Didn''t receive full echo response');
    end;

    for i := 0 to FOutBufCount-1 do
    begin
      if echo[i] <> FOutBuffer[i] then
        Flog('Echo not same as data sent. Sent: ' + IntToStr(FOutBuffer[i]) + ' . Recv: ' + IntToStr(echo[i]));
    end;

    FOutBufCount := 0;
  end;
end;

// Program counter as word address
// So, byte address div 2
procedure TDebugWire.SetPC(addr: word);
var
  cmd: TBytes;
begin
  SetLength(cmd, 3);
  cmd[0] := CMD_SET_PC;
  cmd[1] := hi(addr) or FAddrFlag;
  cmd[2] := lo(addr);
  SendData(cmd);
end;

procedure TDebugWire.SetZreg(const val: word);
var
  data: TBytes;
begin
  SetLength(data, 2);
  data[0] := lo(val);
  data[1] := hi(val);
  WriteRegs(REG_Z, data);
end;

procedure TDebugWire.ReadRegs(const start, count: byte; out values: TBytes);
var
  cmds: TBytes;
begin
  SetPC(start);
  SetBreakPoint(start + count);

  SetLength(cmds, 4);
  cmds[0] := CMD_RW_SETUP;
  cmds[1] := CMD_RW_MODE;
  cmds[2] := RW_MODE_READ_REGS;
  cmds[3] := CMD_GO_RW;
  SendData(cmds);

  ReadData(count, values);
end;

procedure TDebugWire.WriteRegs(const start: byte; const values: TBytes);
var
  cmds: tbytes;
begin
  SetPC(start);
  SetBreakPoint(start + length(values));

  SetLength(cmds, 4);
  cmds[0] := CMD_RW_SETUP;
  cmds[1] := CMD_RW_MODE;
  cmds[2] := RW_MODE_WRITE_REGS;
  cmds[3] := CMD_GO_RW;
  SendData(cmds);

  SendData(values);
end;

procedure TDebugWire.ReadAddress(const start, count: word; out values: TBytes);
const
  MaxBufSize = 16;
var
  addr, totalremaining, len, i, ptr: integer;
  temp: TBytes;
begin
  SetLength(values, count);
  addr := start;
  totalremaining := count;
  ptr := 0;  // pointer to next value in values to be written

  len := min(FRegCacheStart - addr, totalremaining);
  if len > 0 then
  begin
    FReadAddress(addr, len, temp);
    for i := 0 to len-1 do
    begin
      values[ptr] := temp[i];
      inc(ptr);
    end;

    addr := addr + len;
    totalremaining := totalremaining - len;
  end;

  i := 0;
  while (addr >= FRegCacheStart) and (addr < FRegCacheStart + FRegCacheLength) and (totalremaining > 0) do
  begin
    values[ptr] := FCachedRegs[i];
    inc(addr);
    dec(totalremaining);
    inc(ptr);
    inc(i);
  end;

  // Read up to before DWDR
  len := min(totalremaining, $20 + FDevice.DWDR - addr); // Register address in DeviceInfo doesn't include register file
  if len > 0 then
  begin
    FReadAddress(addr, len, temp);
    for i := 0 to len-1 do
    begin
      values[ptr] := temp[i];
      inc(ptr);
    end;

    addr := addr + len;
    totalremaining := totalremaining - len;
  end;

  if (addr = FDevice.DWDR + $20) and (totalremaining > 0) then
  begin
    values[ptr] := 0; // dummy value
    inc(addr);
    inc(ptr);
    dec(totalremaining);
  end;

  // Read the rest in chunks
  while (totalremaining > MaxBufSize) do
  begin
    FReadAddress(addr, MaxBufSize, temp);
    for i := 0 to length(temp)-1 do
    begin
      values[ptr] := temp[i];
      inc(ptr);
    end;

    addr := addr + length(temp);
    totalremaining := totalremaining - Length(temp);
    sleep(10);
  end;

  if totalremaining > 0 then
  begin
    FReadAddress(addr, totalremaining, temp);
    for i := 0 to length(temp)-1 do
    begin
      values[ptr] := temp[i];
      inc(ptr);
    end;
  end;
end;

procedure TDebugWire.WriteAddress(startAddress: word; const values: TBytes);
var
  data: TBytes;
  limit, i: byte;
begin
  SetLength(data, 3);
  SetZreg(startAddress);
  SetBreakPoint(3);
  data[0] := CMD_RW_SETUP;
  data[1] := CMD_RW_MODE;
  data[2] := RW_MODE_WRITE_SRAM;
  SendData(data);

  limit := startAddress + Length(values);
  i := 0;
  while startAddress < limit do
  begin
    if (startAddress < 28) or ((startAddress > 31) and (startAddress <> FDevice.DWDR + $20)) then
    begin
      SetPC(1);
      SendData(byte(CMD_GO_RW));
      SendData(values[i]);
    end
    else if (startAddress >= FRegCacheStart) and (startAddress <= FRegCacheStart + FRegCacheLength) then
    begin
      FCachedRegs[startAddress - FRegCacheStart] := values[i];
      SetZreg(startAddress + 1);
    end;

    inc(i);
    inc(startAddress);
  end;
  FPushSerialBuffer;
end;

procedure TDebugWire.FReadAddress(const start, count: word; out values: TBytes);
var
  cmd: TBytes;
begin
  SetZreg(start);
  SetPC(0);
  SetBreakPoint(2*count);

  SetLength(cmd, 4);
  cmd[0] := CMD_RW_SETUP;
  cmd[1] := CMD_RW_MODE;
  cmd[2] := RW_MODE_READ_SRAM;
  cmd[3] := CMD_GO_RW;
  SendData(cmd);

  ReadData(count, values);
end;

procedure TDebugWire.FWriteFlashPage(startAddress: word; const page: TBytes);
var
  oldPage: TBytes;
  i: integer;
  doWrite, doErase: boolean;
begin
  // In case of ATMega?
  FReEnabelRWW;

  ReadFlash(startAddress, FDevice.FlashPageSize, oldPage);

  doWrite := false;
  doErase := false;
  i := 0;
  while (i < length(oldPage)) and not(doErase) do
  begin
    doWrite := doWrite or (oldPage[i] <> page[i]);  // Flag any difference
    doErase := (not(oldPage[i]) and page[i]) > 0;   // Flag any 0 -> 1 transition
    inc(i);
  end;

  if doErase then
  begin
    FLog('Erasing flash page starting at: $' + hexStr(startAddress, 4));
    FEraseFlashPage(startAddress);
  end;

  if doWrite then
  begin
    FLog('Loading flash page buffer.');
    FLoadPageBuffer(startAddress, page);

    FLog('Write flash page buffer');
    FWritePageBuffer(startAddress, page);
  end;

  FReEnabelRWW;
end;

procedure TDebugWire.FEraseFlashPage(startAddress: word);
var
  data: TBytes;
begin
  SetLength(data, 3);
  // Write %0000011 to SPMCSR, Page address in Z
  data[0] := PGERS or SPMEN;     // PGERS = page erase bit in SPMCSR
  data[1] := lo(startAddress);
  data[2] := hi(startAddress);
  WriteRegs(29, data);           // R29 = PGERS, R30-R31 = startAddress
  SetPC(FDevice.bootStart);      // To enable SPM instruction
  SendData(byte(CMD_SS_SETUP));
  OutInstruction(SPMCSR, 29);    // Write content of R29 to SPMCSR (PGERS and SPMEN)

  // Execute SPM:
  SetLength(data, 4);
  data[0] := CMD_SET_IR;
  data[1] := hi(OpCode_SPM);
  data[2] := lo(OpCode_SPM);
  data[3] := CMD_SS_SPM;
  SendData(data);

  Sync;
end;

procedure TDebugWire.FReEnabelRWW;
var
   data: TBytes;
begin
  if (FDevice.bootStart > 0) then
  begin
    // TODO: Consider need to first check if RWWSRE or SELFPRGEN is set?
    SetPC(FDevice.bootStart);  // Set PC to boot loader section to execute SPM instruction
    SetLength(data, 1);
    data[0] := RWWSRE and SPMEN;
    WriteRegs(29, data); // r29 := RWWSRE
    OutInstruction(SPMCSR, 29);  // out SPMCSR,r29
    SendInstruction16(OpCode_SPM);       // spm
    //FPushSerialBuffer;
  end;
end;

procedure TDebugWire.FLoadPageBuffer(address: word; values: TBytes);
var
  data: TBytes;
  i: byte;
  //tv: TTimeVal;
begin
  SetLength(data, 3);

  // Write %0000001 to SPMCSR, data word address in Z
  data[0] := SPMEN;
  data[1] := lo(address);        // Note that Z is interpreted as PCPAGE + PCWORD
  data[2] := hi(address);        // To write to temp page the page address (PCPAGE) is masked out, so effectively only the data address from 0 to pagesize is used
  WriteRegs(29, data);           // r29 := SPEN, Z = word address of first data word

  SendData(Byte(CMD_SS_SETUP));  // Set up for single step mode
  i := 0;
  SetLength(data, 2);
  SetPC(FDevice.bootStart);      // Set PC that allows access to all of flash
  while (i < FDevice.FlashPageSize) do
  begin
    data[0] := values[i];          // R0
    data[1] := values[i+1];        // R1
    i := i + 2;
    WriteRegs(0, data);            // r0 := low byte, r1 := high byte
    OutInstruction(SPMCSR, 29);    // out SPMCSR,r29 (SPMEN)
    SendInstruction16(OpCode_SPM); // spm
    SendInstruction16($9632);      // adiw Z,2  TODO: Really needed, Z is auto inceremented by SPM?
  end;
  FPushSerialBuffer;
end;

procedure TDebugWire.FWritePageBuffer(address: word; values: TBytes);
var
  data: TBytes;
begin
  SetLength(data, 3);
  data[0] := PGWRT or SPMEN;            // Page write
  data[1] := lo(address);
  data[2] := hi(address);
  WriteRegs(29, data);
  SetPC(FDevice.bootStart);             // move PC into boot section to enable execution of SPM instruction
  OutInstruction(SPMCSR, 29);

  if (FDevice.bootStart > 0) then       // mega device?
  begin
    SendInstruction16(OpCode_SPM);      // Do spm
    FPushSerialBuffer;
    FLog('Waiting for SPM to complete');
    while ((FReadSPMCSR and SPMEN) = SPMEN) do   // block until SPMEN clears
    begin
      Sleep(1);   // 3.7 - 4.5 ms per Flash write operation
      FLog('.');
    end;
    FLog('SPM of page completed');
  end
  else
  begin
    FLog('Start SPM of page write');
    SetLength(data, 4);
    data[0] := CMD_SET_IR;
    data[1] := hi(OpCode_SPM);
    data[2] := lo(OpCode_SPM);
    data[3] := CMD_SS_SPM;
    SendData(data);
    // Wait for break signal
    Sync;
  end;
end;

function TDebugWire.FReadSPMCSR: byte;
var
  val: TBytes;
begin
  SetLength(val, 1);
  SendData(Byte(CMD_SS_SETUP));        // Set up for single step mode
  InInstruction(30, SPMCSR);
  ReadRegs(30, 1, val);
  Result := val[0];
end;

procedure TDebugWire.WriteFlash(start: word; const values: TBytes);
var
  R0R1: TBytes;
  page: TBytes;
  partLength, pageMask, PageStart, remainingLength: word;
  len, i: byte;
begin
  if start + length(values) > FDevice.flashSize then
  begin
    FLog('ERROR - WriteFlash starting address + length of data exceeds flash size!');
    exit;
  end
  else if (length(values) > 0) then
  begin
    pageMask := FDevice.flashSize - FDevice.FlashPageSize;
    SetLength(page, FDevice.FlashPageSize);
    remainingLength := length(values);
    // Cache R0 & R1
    ReadRegs(0, 2, R0R1);

    // Check if start falls inside a page
    // If so, fill first part of page with existing content
    if (start and (FDevice.FlashPageSize - 1) > 0) then
    begin
      PageStart := start and pageMask;
      ReadFlash(PageStart, FDevice.FlashPageSize, page);
      len := min(FDevice.FlashPageSize, length(values));
      for i := 0 to len-1 do
        page[start - PageStart + i] := values[i];

      FWriteFlashPage(PageStart, page);
      partLength := FDevice.FlashPageSize - ((FDevice.FlashPageSize - 1) and start);
      start := start + partLength;
      if partLength < remainingLength then
        remainingLength := remainingLength - partLength
      else
        remainingLength := 0;;
    end;

    // Whole pages
    while remainingLength > FDevice.FlashPageSize do
    begin
      page := copy(values, start, FDevice.FlashPageSize);
      FWriteFlashPage(start, page);
      start := start + FDevice.FlashPageSize;
      remainingLength := remainingLength - FDevice.FlashPageSize;
    end;

    if remainingLength > 0 then
    begin
      ReadFlash(start, FDevice.FlashPageSize, page);

      // Copy new data on top of existing data
      for i := 0 to remainingLength-1 do
        page[i] := values[i];

      FWriteFlashPage(start, page);
    end;

    // Restore R0-R1
    WriteRegs(0, R0R1);
    FPushSerialBuffer;
  end;
end;

procedure TDebugWire.ReadFlash(const start, count: word; out values: TBytes);
var
  cmd, temp: TBytes;
  addr: word;
  addrEnd, len: integer;
begin
  addrEnd := start + count;
  addr := start;

  SetLength(cmd, 4);
  cmd[0] := CMD_RW_SETUP;
  cmd[1] := CMD_RW_MODE;
  cmd[2] := RW_MODE_READ_FLASH;
  cmd[3] := CMD_GO_RW;

  // Read 128 byte chunks
  while (addr < addrEnd) do
  begin
    len := min(addrEnd - addr, 128);

    SetZreg(addr);
    SetPC(FDevice.bootStart);    // to execute LPM?
    SetBreakPoint(FDevice.bootStart + 2*len);

    SendData(cmd);

    ReadData(len, temp);
    values := ConCatArray(values, temp);
    addr := addr + len;
  end;
end;

procedure TDebugWire.SendInstruction16(const instr: word);
var
  data: TBytes;
  //data: array[0..4] of byte;
begin
  SetLength(data, 5);
  data[0] := CMD_SS_SETUP;       // Single step
  data[1] := CMD_SET_IR;         // Set instruction register
  data[2] := hi(instr);
  data[3] := lo(instr);
  data[4] := CMD_SS_IR;          // Single step instrunction in register
  SendData(data);
end;

procedure TDebugWire.OutInstruction(const IORegAddr: byte; const sourceReg: byte);
var
  instr: word;
begin
   instr := $B800 or ((IORegAddr shl 5) and $0600) or
            ((sourceReg shl 4) and $01F0) or (IORegAddr and $000F);
   SendInstruction16(instr);
end;

procedure TDebugWire.InInstruction(const destReg: byte; const IORegAddr: byte);
var
  instr: word;
begin
   instr := $B000 or ((IORegAddr shl 5) and $0600) or
            ((destReg shl 4) and $01F0) or (IORegAddr and $000F);
   SendInstruction16(instr);
end;

procedure TDebugWire.ReadConfig(const index: byte; out value: byte);
var
  R0: TBytes;
  data: TBytes;
begin
  // Backup R0
  ReadRegs(0, 1, R0);
  SetLength(data, 3);
  data[0] := RFLB or SPMEN;  // R29
  data[1] := index;          // R30
  data[2] := 0;              // R31
  WriteRegs(29, data);
  SendData(byte(CMD_SS_SETUP));
  OutInstruction(SPMCSR, 29);
  SendInstruction16($95C8);      // Load register from Program Memory (from Z) and store result in R0
  ReadRegs(0, 1, data);
  value := data[0];
  WriteRegs(0, R0);   // restore R0
  FPushSerialBuffer;
end;

procedure TDebugWire.Reset;
begin
  SendData(Byte(CMD_RESET));
  Sync;
  Reconnect;
end;

procedure TDebugWire.Sync;
var
  data, res: byte;
begin
  FPushSerialBuffer;
  repeat
    res := FSer.ReadTimeout(data, 1, 200);
    if res = 0 then
      Sleep(10);
  until (res > 0);

  repeat
    res := FSer.ReadTimeout(data, 1, 200);
    if res = 0 then
      Sleep(10);
  until (res > 0) and (data < 255);

  if data = 85 then
    //FLog('Received $55')
  else
    FLog('Unexpected reply: ' + IntToStr(data));
end;

function TDebugWire.TrySync: boolean;
var
  data, res: byte;
begin
  FPushSerialBuffer;
  res := FSer.ReadTimeout(data, 1, 100);
  Result := false;

  if (res > 0) then
    if (data = 0) then
    begin
      //FLog('Received break');

      res := FSer.ReadTimeout(data, 1, 100);

      // Seem to get bogus $FF value before $55
      if data = $FF then
        res := FSer.ReadTimeout(data, 1, 100);

      if data = $55 then
      begin
        //FLog('Received $55');
        Result := true;
      end
      else
      begin
        FLog('Expected $55 after BREAK, but got: $'+ hexStr(data, 2));
        FLog('Accept as break response anyway...');
        Result := true;
      end;
    end
    else if (data = $55) then
    begin
      FLog('Received late $55?');
      Result := true;
    end
    else
      FLog('Unexpected reply: $' + hexStr(data, 2));
end;

procedure TDebugWire.Reconnect;
var
  buf: TBytes;
begin
  SendData(CMD_READ_PC);
  ReadData(2, buf);
  FPC := 2*((buf[0] shl 8 + buf[1]) - 1);   // PC points to next instruction word
  FLog('PC: $' + hexStr(FPC, 4));

  // Cache R28-R31
  ReadRegs(FRegCacheStart, FRegCacheLength, FCachedRegs);
  FLog('R28: $' + hexStr(FCachedRegs[0], 2) + ', ' +
          'R29: $' + hexStr(FCachedRegs[1], 2) + ', ' +
          'R30: $' + hexStr(FCachedRegs[2], 2) + ', ' +
          'R31: $' + hexStr(FCachedRegs[3], 2));
end;

procedure TDebugWire.ContinueUntilBreak;
begin
  // Restore potentially dirty regs
  WriteRegs(FRegCacheStart, FCachedRegs);
  SetPC(FPC div 2);
  if FBP >= 0 then
  begin
    SetBreakPoint(FBP div 2);
    SendData(byte(CMD_GO_CONTEXT or FTimersMask or 1));  // seems like timers ContinueUntilBreak anyway????
  end
  else
    SendData(byte(CMD_GO_CONTEXT or FTimersMask));  // seems like timers ContinueUntilBreak anyway????

  SendData(byte(CMD_RUN));

  // Blocking ContinueUntilBreak
  Sync;
  Reconnect;
end;

procedure TDebugWire.Run;
begin
  // Restore potentially dirty regs
  WriteRegs(FRegCacheStart, FCachedRegs);
  SetPC(FPC div 2);
  if FBP >= 0 then
  begin
    SetBreakPoint(FBP div 2);
    SendData(byte(CMD_GO_CONTEXT or FTimersMask or 1));  // seems like timers ContinueUntilBreak anyway????
  end
  else
    SendData(byte(CMD_GO_CONTEXT or FTimersMask));  // seems like timers ContinueUntilBreak anyway????

  SendData(byte(CMD_RUN));
  FPushSerialBuffer;
  // Optionally poll for break
end;

procedure TDebugWire.Step;
begin
  // Restore potentially dirty regs
  WriteRegs(FRegCacheStart, FCachedRegs);
  SetPC(FPC div 2);
  SendData(byte(CMD_GO_CONTEXT or FLAG_TIMERS_DISABLE));
  SendData(byte(CMD_SS));

  Sync;
  Reconnect;
end;

end.

