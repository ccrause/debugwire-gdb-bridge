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

{$define debug}

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
    flashSize,           // In bytes
    FlashPageSize,       // In bytes
    DWDR,                // DebugWIRE data register, aka MONDR - Monitor data register
    bootStart,           // Lowest PC value giving boot section access - used to set PC when executing SPM instruction to get write access to flash
    bootflags,           // Where to find the boot sector control flags, if any (0=not present, 1=ext fuse, 2=high fuse)
    EECR,                // EEPROM control register index.
    EEDR,                // EEPROM data register
    EEARL,               // EEPROM address low
    EEARH: integer;      // EEPROM address high (doesn't exist on all devices)
    NRRW: integer;       // Start of no read while write section. Before this the CPU can read from NRRW section & write to RWW section.  After this the CPU is halted during a flash write operation.
  end;

  TLog = procedure (s: string) of object;
  { TDebugWire }

  TDebugWire = class
  private
    FSer: TSerialObj;
    FAddrFlag: byte;
    FTimersDisabled: boolean;
    FTimersMask: byte;
    FDevice: TDeviceInfo;
    FPC: word;
    FBP: integer;  // Single hardware breakpoint
    FCachedRegs: TBytes; // Cache of R28:R31 which gets changed in some debugwire transactions.
    FLogger: TLog;

    FOutBuffer: array[0..255] of byte;
    FOutBufCount: integer;

    FLoggingEnabled: boolean;

    procedure FLogFile(data: TBytes; count: integer); overload;
    procedure FLogFile(s: string);  overload;
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
    procedure FWriteFlashPage(startAddress: word; const newPage, oldPage: TBytes);
    procedure FEraseFlashPage(startAddress: word);

    procedure FReEnableRWW;  // require follow-up FPushSerialBuffer
    procedure FLoadPageBuffer(address: word; values: TBytes);
    procedure FWritePageBuffer(address: word);

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
    // Call ReadRegs, then substitute cached registers in values
    procedure safeReadRegs(const start, count: byte; out values: TBytes);
    procedure WriteRegs(const start: byte; const values: TBytes);  // require follow-up FPushSerialBuffer

    // regs, IO & SRAM
    // Reads all data from hardware, except R28-R32 & DWDR registers
    procedure ReadAddress(const start, count: word; out values: TBytes);
    procedure WriteAddress(startAddress: word; const values: TBytes);

    procedure ReadFlash(const start, count: word; out values: TBytes);
    procedure WriteFlash(start: word; const values: TBytes);

    procedure ReadEEPROM(const start, count: word; out values: TBytes);
    procedure WriteEEPROM(start: word; const values: TBytes);

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

    procedure DisableDWEN;
    property Device: TDeviceInfo read FDevice;
    property TimersDisabled: boolean read FTimersDisabled write FTimersDisabledProc;
    property BaudRate: integer read FReadBaud write FSetBaud;
    property PC: word read FPC write FPC;
    property BP: integer read FBP write FBP;
    property OnLog: TLog read FLogger write FLogger;
    property LoggingEnabled: boolean read FLoggingEnabled write FLoggingEnabled;

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
  DWDR = $31;          // debug-wire register, used to push data in/out

  DeviceInfo: array[0..20] of TDeviceInfo =
    ((name: 'ATtiny13';   ID: $9007; ioregSize:  64; sramSize:   64; eepromSize:  64; EepromPageSize: 4; flashSize: 1024; FlashPageSize: 32; DWDR: $2E; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $00; NRRW: 0),
     (name: 'ATtiny2313'; ID: $910a; ioregSize:  64; sramSize:  128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $1f; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $00; NRRW: 0),
     (name: 'ATtiny24';   ID: $910B; ioregSize:  64; sramSize:  128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATtiny44';   ID: $9207; ioregSize:  64; sramSize:  256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATtiny84';   ID: $930C; ioregSize:  64; sramSize:  512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATtiny25';   ID: $9108; ioregSize:  64; sramSize:  128; eepromSize: 128; EepromPageSize: 4; flashSize: 2048; FlashPageSize: 32; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATtiny45';   ID: $9206; ioregSize:  64; sramSize:  256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATtiny85';   ID: $930B; ioregSize:  64; sramSize:  512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $22; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0),
     (name: 'ATmega48A';  ID: $9205; ioregSize: 224; sramSize:  512; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $31; bootStart: $0000; bootflags: 0; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: 0),
     (name: 'ATmega48PA'; ID: $920A; ioregSize: 224; sramSize:  512; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 64; DWDR: $31; bootStart: $0000; bootflags: 0; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: 0),

     (name: 'ATmega88A';  ID: $930A; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $31; bootStart: $0F80; bootflags: 1; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $C00),
     (name: 'ATmega88PA'; ID: $930F; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $31; bootStart: $0F80; bootflags: 1; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $C00),

     (name: 'ATmega168A'; ID: $9406; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize:16384; FlashPageSize:128; DWDR: $31; bootStart: $1F80; bootflags: 1; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $1C00),
     (name: 'ATmega168PA';ID: $940B; ioregSize: 224; sramSize: 1024; eepromSize: 512; EepromPageSize: 4; flashSize:16384; FlashPageSize:128; DWDR: $31; bootStart: $1F80; bootflags: 1; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $1C00),
     (name: 'ATmega328P'; ID: $950F; ioregSize: 224; sramSize: 2048; eepromSize:1024; EepromPageSize: 4; flashSize:32768; FlashPageSize:128; DWDR: $31; bootStart: $3F00; bootflags: 2; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $3800),
     (name: 'ATmega328';  ID: $9514; ioregSize: 224; sramSize: 2048; eepromSize:1024; EepromPageSize: 4; flashSize:32768; FlashPageSize:128; DWDR: $31; bootStart: $3F00; bootflags: 2; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $3800),

     (name: 'ATmega8U2';  ID: $9389; ioregSize: 224; sramSize:  512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 64; DWDR: $31; bootStart: $0F00; bootflags: 2; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $0800),
     (name: 'ATmega16U2'; ID: $9489; ioregSize: 224; sramSize:  512; eepromSize: 512; EepromPageSize: 4; flashSize:16384; FlashPageSize:128; DWDR: $31; bootStart: $1F00; bootflags: 2; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $1800),
     (name: 'ATmega32U2'; ID: $958A; ioregSize: 224; sramSize: 1024; eepromSize:1024; EepromPageSize: 4; flashSize:32768; FlashPageSize:128; DWDR: $31; bootStart: $3F00; bootflags: 2; EECR: $1F; EEDR: $20; EEARL: $21; EEARH: $22; NRRW: $3800),

     (name: 'ATtiny441';  ID: $9215; ioregSize: 224; sramSize:  256; eepromSize: 256; EepromPageSize: 4; flashSize: 4096; FlashPageSize: 16; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $00; NRRW: 0),
     (name: 'ATtiny481';  ID: $9315; ioregSize: 224; sramSize:  512; eepromSize: 512; EepromPageSize: 4; flashSize: 8192; FlashPageSize: 16; DWDR: $27; bootStart: $0000; bootflags: 0; EECR: $1C; EEDR: $1D; EEARL: $1E; EEARH: $1F; NRRW: 0));

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
  CMD_SET_IR= $D2;    // set word size instruction to instruction register, execute with CMD_SS_IR
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
  OpCode_Break        = $9598;
  OpCode_SPM          = $95E8;
  OpCode_Subi_R30_255 = $5FEF;
  OpCode_Adiw_Z_1     = $9631;
  OpCode_Subi_R30_254 = $5FEE;
  OpCode_Adiw_Z_2     = $9632;
  OpCode_LPM          = $95C8;   // Load address in Z from program space, store in R0

  // EEPROM control register bit settings
  EEMP0 = 1 shl 4;
  EEMPROM_ERASE_WRITE_MODE     = 0 shl EEMP0;
  EEMPROM_ERASE_ONLY_MODE      = 1 shl EEMP0;
  EEMPROM_WRITE_ONLY_MODE      = 2 shl EEMP0;
  EEPROM_MASTER_PROGRAM_ENABLE = 1 shl 2;
  EEPROM_PROGRAM_ENABLE        = 1 shl 1;
  EEPROM_READ_ENABLE           = 1 shl 0;

  // Config options

function ConCatArray(a1, a2: TBytes): TBytes;
begin
  Result := nil;
  SetLength(Result, Length(a1) + Length(a2));
  FillChar(Result[0], Length(Result), 0);
  if length(a1) > 0 then
    Move(a1[0], Result[0], Length(a1));
  if length(a2) > 0 then
  Move(a2[0], Result[Length(a1)], Length(a2));
end;


{ TDebugWire }

procedure TDebugWire.FLogFile(data: TBytes; count: integer);
const
  fname = 'debug.txt';
var
  f: TextFile;
  i: integer;
begin
  AssignFile(f, fname);
  if FileExists(fname) then
    Append(f)
  else
    Rewrite(f);

  for i := 0 to count-1 do
    Write(f, HexStr(data[i], 2), ' ');
  WriteLn(f);
  Flush(f);
  CloseFile(f);
end;

procedure TDebugWire.FLogFile(s: string);
const
  fname = 'debug.txt';
var
  f: TextFile;
begin
  AssignFile(f, fname);
  if FileExists(fname) then
    Append(f)
  else
    Rewrite(f);

  WriteLn(f, s);
  Flush(f);
  CloseFile(f);
end;

procedure TDebugWire.FLog(s: string);
begin
  if FLoggingEnabled and Assigned(FLogger) then
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
       7: Result := 98;
       6: result := 95;
       5: result := 90;
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
  FLoggingEnabled := false;
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
    Result := FSer.OpenPort(portName, baud)
  else
    Result := Connect(portName);
end;

function TDebugWire.Connect(portName: string): boolean;
begin
  Result := Connect(portName, 200000);
  if Result then
    Result := ScanTargetBaud;
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
      inc(retries);
    scale := FBAUDscale(data);
    if scale = 100 then
      BAUDscanDone := true
    else
    begin
      if scale < oldscale then
      begin
        // To get here the bit pattern would have deteriorated,
        // keep making small steps until the situation improves...
        FLog('Scale increase predicted, make small adjustment.');
        scale := 99;
      end
      else
        oldscale := scale;

      FLog('Scale = ' + IntToStr(scale) + '%');
      FSer.BaudRate := (FSer.BaudRate * scale) div 100;
    end;

    FSer.FlushInput;
  until (retries > 25) or BAUDscanDone;

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

  FLog('BAUD: ' + IntToStr(FSer.BaudRate) + '. Data: ' + IntToStr(buf[0]) + '. Bin: ' + binStr(buf[0], 8));
  Result := buf[0];
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

  len := FSer.ReadTimeout(values[0], count, 100 + 10*count);
  if len < count then
  begin
    FLog('ReadData: len < count.  OS error: ' + IntToStr(GetLastOSError) + ' - '
    {$ifndef windows}+ StrError(GetLastOSError){$endif});
    l2 := FSer.ReadTimeout(values[len], count - len, 100 + 10*(count - len));
    len := len + l2;
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
  cmd[1] := hi(addr) or FAddrFlag;
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
    {$ifdef debug} FLogFile(FOutBuffer, FOutBufCount);{$endif}
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
  //if count = 1 then // shorter to just execute an OUT instruction to push the register over debugwire
  //begin
  //  OutInstruction(DWDR, start);
  //end
  //else
  begin
    SetPC(start);
    SetBreakPoint(start + count);

    SetLength(cmds, 4);
    cmds[0] := CMD_RW_SETUP;
    cmds[1] := CMD_RW_MODE;
    cmds[2] := RW_MODE_READ_REGS;
    cmds[3] := CMD_GO_RW;
    SendData(cmds);
  end;
  ReadData(count, values);
end;

procedure TDebugWire.safeReadRegs(const start, count: byte; out values: TBytes);
var
  i: integer;
begin
  ReadRegs(start, count, values);
  for i := start to count-1 do
    if (i >= FRegCacheStart) and (i < (FRegCacheStart + FRegCacheLength)) then
      values[i] := FCachedRegs[i - FRegCacheStart];
end;

procedure TDebugWire.WriteRegs(const start: byte; const values: TBytes);
var
  cmds: tbytes;
  //i: integer;
begin
  //if length(values) = 1 then // shorter to just execute an OUT instruction to push the register over debugwire
  //begin
  //  InInstruction(start, DWDR);
  //  SendData(values[0]);
  //end
  //else
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
  limit, i: word;
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
    if (startAddress < FRegCacheStart) or ((startAddress > FRegCacheStart + FRegCacheLength) and (startAddress <> FDevice.DWDR + $20)) then
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

procedure TDebugWire.FWriteFlashPage(startAddress: word; const newPage, oldPage: TBytes);
var
  //oldPage: TBytes;
  i: integer;
  doWrite, doErase: boolean;
begin
  // In case of ATMega?
  //FReEnableRWW;

  //ReadFlash(startAddress, FDevice.FlashPageSize, oldPage);

  doWrite := false;
  doErase := false;
  i := 0;
  while (i < length(oldPage)) and not(doErase) do
  begin
    doWrite := doWrite or (oldPage[i] <> newPage[i]);  // Flag any difference
    doErase := (not(oldPage[i]) and newPage[i]) > 0;   // Flag any 0 -> 1 transition
    inc(i);
  end;

  if doErase then
  begin
    FLog('Erasing flash page starting at: $' + hexStr(startAddress, 4));
    FEraseFlashPage(startAddress);
    FReEnableRWW;  // re-enable RWW section
  end;

  if doWrite then
  begin
    FLog('Loading flash page buffer.');
    FLoadPageBuffer(startAddress, newPage);

    FLog('Write flash page buffer');
    FWritePageBuffer(startAddress);
    FReEnableRWW;  // re-enable RWW section
  end;
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

procedure TDebugWire.FReEnableRWW;
var
   data: TBytes;
begin
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}
  if (FDevice.bootStart > 0) then
  begin
    // TODO: Consider need to first check if RWWSRE or SELFPRGEN is set?
    SetLength(data, 1);
    data[0] := RWWSRE or SPMEN;
    WriteRegs(29, data); // r29 := RWWSRE
    OutInstruction(SPMCSR, 29);  // out SPMCSR,r29
    SetPC(FDevice.bootStart);  // Set PC to boot loader section to execute SPM instruction
    SendInstruction16(OpCode_SPM);       // spm
  end;
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
end;

procedure TDebugWire.FLoadPageBuffer(address: word; values: TBytes);
var
  data: TBytes;
  i: byte;
begin
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}
  SetLength(data, 3);

  // Write %0000001 to SPMCSR, data word address in Z
  data[0] := SPMEN;
  data[1] := lo(address);        // Note that Z is interpreted as PCPAGE + PCWORD
  data[2] := hi(address);        // To write to temp page the page address (PCPAGE) is masked out, so effectively only the data address from 0 to pagesize is used
  WriteRegs(29, data);           // r29 := SPEN, Z = word address of first data word

  i := 0;
  SetLength(data, 2);
  while (i < FDevice.FlashPageSize) do
  begin
    data[0] := values[i];                // R0
    data[1] := values[i+1];              // R1
    i := i + 2;
    WriteRegs(0, data);                  // r0 := low byte, r1 := high byte
    SetPC(FDevice.bootStart);            // Set PC that allows access to all of flash
    OutInstruction(SPMCSR, 29);          // out SPMCSR,r29 (SPMEN)
    SendInstruction16(OpCode_SPM);       // spm
    if Device.FlashPageSize > 256 then
      SendInstruction16(OpCode_Adiw_Z_2) // adiw Z,2
    else
      SendInstruction16(OpCode_Subi_R30_254);  // subi R30, 254 = add R30, 2
  end;
  FPushSerialBuffer;
  {$ifdef debug} FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
end;

procedure TDebugWire.FWritePageBuffer(address: word);
var
  data: TBytes;
begin
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}
  SetLength(data, 3);
  data[0] := PGWRT or SPMEN;            // Page write
  data[1] := lo(address);               // Put address in Z
  data[2] := hi(address);
  WriteRegs(29, data);
  SetPC(FDevice.bootStart);             // move PC into boot section to enable execution of SPM instruction
  OutInstruction(SPMCSR, 29);

  if (FDevice.bootStart > 0) and (address < FDevice.NRRW) then       // mega device?
  begin
    SendInstruction16(OpCode_SPM);      // Do spm
    FPushSerialBuffer;
    Sleep(1);
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
  {$ifdef debug} FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
end;

function TDebugWire.FReadSPMCSR: byte;
var
  val: TBytes = nil;
begin
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}
  SetLength(val, 1);
  InInstruction(30, SPMCSR);
  ReadRegs(30, 1, val);
  Result := val[0];
  {$ifdef debug} FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
end;

procedure TDebugWire.WriteFlash(start: word; const values: TBytes);
var
  R0R1: TBytes;
  page, oldPage: TBytes;
  partLength, pageMask, PageStart, remainingLength, currentIndex: word;
  len, i: word;
begin
  if start + length(values) > FDevice.flashSize then
  begin
    FLog('ERROR - WriteFlash starting address + length of data exceeds flash size!');
    exit;
  end
  else if (length(values) > 0) then
  begin
    {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}
    // Cache R0 & R1
    ReadRegs(0, 2, R0R1);
    pageMask := FDevice.flashSize - FDevice.FlashPageSize;
    SetLength(page, FDevice.FlashPageSize);
    remainingLength := length(values);
    currentIndex := 0;  // start at starting point of values

    while remainingLength > 0 do
    begin
      PageStart := start and pageMask;
      ReadFlash(PageStart, FDevice.FlashPageSize, oldPage);

      if (start > PageStart) then  // start falls inside page boundary
      begin
        page := copy(oldPage);
        len := length(values);
        if ((start + len - 1) and pageMask) > PageStart then // end falls in next page
          len :=  FDevice.FlashPageSize - (start and (FDevice.FlashPageSize - 1));

        len := min(FDevice.FlashPageSize, len);
        for i := 0 to len-1 do
          page[start - PageStart + i] := values[i];

        partLength := len;
      end
      else if remainingLength >= FDevice.FlashPageSize then  // enough data to fill whole flash page
      begin
        page := copy(values, currentIndex, FDevice.FlashPageSize);
        partLength := FDevice.FlashPageSize;
      end
      else // last bit of data not enough to fill whole page
      begin
        page := copy(oldPage);
        for i := 0 to remainingLength-1 do
          page[i] := values[currentIndex + i];

        partLength := remainingLength;
      end;

      FWriteFlashPage(PageStart, page, oldPage);
      start := start + partLength;
      currentIndex := currentIndex + partLength;
      if partLength < remainingLength then
        remainingLength := remainingLength - partLength
      else
        remainingLength := 0;
    end;

    // Restore R0-R1
    WriteRegs(0, R0R1);
    FPushSerialBuffer;
    {$ifdef debug} FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
  end;
end;

procedure TDebugWire.ReadEEPROM(const start, count: word; out values: TBytes);
var
  R0, data, status: TBytes;
  i, timeout: integer;
begin
  // Backup R0
  ReadRegs(0, 1, R0);

  // Wait for previous EEPROM write to finish
  timeout := 10;
  repeat
    // Read EECR and check that EEPE is clear
    InInstruction(0, Device.EECR);
    OutInstruction(Device.DWDR, 0);
    ReadData(1, status);
    dec(timeout);
  until ((status[0] and EEPROM_PROGRAM_ENABLE) = 0) or (timeout = 0);

  // Configure R29, R30, R31
  SetLength(data, 3);
  data[0] := EEPROM_READ_ENABLE;
  data[1] := lo(start);
  data[2] := hi(start);
  WriteRegs(29, data);

  i := 0;
  SetLength(data, 1);
  SetLength(values, count);
  repeat
    // Configure EEPROM registers to read address from R30:R31.
    // EEPROM_READ_ENABLE already set in R29, above.
    if Device.EEARH > 0 then
      OutInstruction(Device.EEARH, 31);

    OutInstruction(Device.EEARL, 30);
    OutInstruction(Device.EECR, 29);
    InInstruction(0, Device.EEDR);
    OutInstruction(Device.DWDR, 0);

    ReadData(1, data);
    values[i] := data[0];
    inc(i);
    // Increment Z register to read next byte in EEPROM
    if i < count then
    begin
      if Device.EEARH > 0 then
        SendInstruction16(OpCode_Adiw_Z_1)
      else
        SendInstruction16(OpCode_Subi_R30_255);   // subi r30, -1
    end;
  until i = count;

  // restore R0
  WriteRegs(0, R0);
end;

// Byte address based
procedure TDebugWire.ReadFlash(const start, count: word; out values: TBytes);
var
  cmd, temp: TBytes;
  addr: word;
  addrEnd, len: integer;
begin
  addrEnd := start + count;
  addr := start;

  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%}); {$endif}

  SetLength(cmd, 4);
  cmd[0] := CMD_RW_SETUP;
  cmd[1] := CMD_RW_MODE;
  cmd[2] := RW_MODE_READ_FLASH;
  cmd[3] := CMD_GO_RW;

  values := nil;
  // Read 64 byte chunks
  while (addr < addrEnd) do
  begin
    len := min(addrEnd - addr, 64);
    SetZreg(addr);
    SetPC(Device.bootStart);
    SetBreakPoint(Device.bootStart + 2*len);
    SendData(cmd);

    ReadData(len, temp);
    values := ConCatArray(values, temp);
    addr := addr + len;
  end;
  {$ifdef debug} FPushSerialBuffer; FLogFile({$I %CURRENTROUTINE%} + ' - end'); {$endif}
end;

procedure TDebugWire.WriteEEPROM(start: word; const values: TBytes);
var
  R0, data, status: TBytes;
  i, count, timeout: integer;
begin
  // Backup R0
  ReadRegs(0, 1, R0);

  // Configure R28, R29, R30, R31
  SetLength(data, 4);
  data[0] := EEPROM_MASTER_PROGRAM_ENABLE;
  data[1] := EEPROM_PROGRAM_ENABLE;          // EEPM1:EEPM0 = 0, Erase & write mode
  data[2] := lo(start);
  data[3] := hi(start);
  WriteRegs(28, data);

  i := 0;
  count := length(values);
  repeat
    // Wait for previous EEPROM write to finish
    timeout := 10;
    repeat
      // Read EECR and check that EEPE is clear
      InInstruction(0, Device.EECR);
      OutInstruction(Device.DWDR, 0);
      ReadData(1, status);
      dec(timeout);
    until ((status[0] and EEPROM_PROGRAM_ENABLE) = 0) or (timeout = 0);

    // Configure EEPROM registers to read address from R30:R31.
    // EEPROM_READ_ENABLE already set in R29, above.
    if Device.EEARH > 0 then
      OutInstruction(Device.EEARH, 31);
    OutInstruction(Device.EEARL, 30);

    // Load byte into EEDR register via DWDR
    InInstruction(0, Device.DWDR);
    SendData(values[i]);
    OutInstruction(Device.EEDR, 0);

    // Write EEMWE / EEWE sequence to enable and write byte to EEPROM
    OutInstruction(Device.EECR, 28);
    OutInstruction(Device.EECR, 29);

    inc(i);
    // Increment Z register to write next byte in EEPROM
    if i < count then
    begin
      if Device.EEARH > 0 then
        SendInstruction16(OpCode_Adiw_Z_1)
      else
        SendInstruction16(OpCode_Subi_R30_255);   // subi r30, -1
    end;
  until i = count;

  // restore R0
  WriteRegs(0, R0);
end;

procedure TDebugWire.SendInstruction16(const instr: word);
var
  data: TBytes = nil;
begin
  SetLength(data, 5);
  data[0] := CMD_SS_SETUP;       // Single step
  data[1] := CMD_SET_IR;         // Set instruction register
  data[2] := hi(instr);
  data[3] := lo(instr);
  data[4] := CMD_SS_IR;          // Single step instruction in register
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
  data: TBytes = nil;
begin
  // Backup R0
  ReadRegs(0, 1, R0);
  SetLength(data, 3);
  data[0] := RFLB or SPMEN;  // R29
  data[1] := index;          // R30
  data[2] := 0;              // R31
  WriteRegs(29, data);
  //SendData(byte(CMD_SS_SETUP));
  OutInstruction(SPMCSR, 29);
  SendInstruction16(OpCode_LPM);      // Load register from Program Memory (from Z) and store result in R0
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

  // Catch break signal
  repeat
    res := FSer.ReadTimeout(data, 1, 200);
    if res = 0 then
      Sleep(10);
  until (res > 0);

  // Check response
  while (data = 0) or (data = 255) do
  begin
    res := FSer.ReadTimeout(data, 1, 200);
    if res = 0 then
      Sleep(10);
  end;

  if data <> 85 then
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
      res := FSer.ReadTimeout(data, 1, 100);

      // Seem to get bogus $FF value before $55
      if data = $FF then
        res := FSer.ReadTimeout(data, 1, 100);

      if data = $55 then
        Result := true
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

procedure TDebugWire.DisableDWEN;
begin
  SendData(byte(CMD_DISABLE));
  // Empty serial buffer
  FPushSerialBuffer;
end;

end.
