unit bpmanager;

interface

uses
  sysutils, debugwire;

type
  TBPType = (swbreak, hwbreak);
  TBP = record
    address: dword;   // little endian?
    bpsize: byte;     // number of bytes of code affected, so size of origCode
    origCode: TBytes; // straight memory byte sequence
    active: boolean;  // Mark active if BP should be set when stepping/continuing
    written: boolean; // Whether BP opcode was written to flash
  end;
  PBP = ^TBP;

  TFlashPageBPs = record
    PageAddress: dword;
    BreakPoints: array of TBP;
  end;

  THwBp = record
    address: integer;
    active: boolean;
  end;

  // Strategy: try to minimize flash rewrites

  { TBPManager }

  TBPManager = class
  private
    HWBP: THwBp;
    FlashPagesWithBPs: array of TFlashPageBPs; // BPs grouped per flash page
    FDebugWire: TDebugWire;
    FLogger: TLog;
    FLoggingEnabled: boolean;
    FPageMask: dword;
    FPageOffsetMask: dword;
    FlashStart: dword;
    procedure FLog(s: string);

    // Calc flash page address, then check number of BPs
    function FBPcountPerFlashPage(address: dword): integer;

    procedure FCopyBP(const source: TBP; var dest: TBP);
    procedure FCopyFlashPageWithBPs(const soure: TFlashPageBPs; var dest: TFlashPageBPs);
  public
    constructor Create(dw: TDebugWire; log:TLog);
    procedure AddBP(address: dword);

    procedure DeleteBP(address: dword);
    procedure DeleteAllBPs;
    // Call FinalizeBPs just before running code
    // and only rewrite sw BPs if necessary
    procedure FinalizeBPs;
    procedure PrintBPs;
    function findSWBPFromAddress(address: word): PBP;
    property LoggingEnabled: boolean read FLoggingEnabled write FLoggingEnabled;
  end;

implementation

{ TBPManager }

procedure TBPManager.FLog(s: string);
begin
  if FLoggingEnabled and Assigned(FLogger) then
    FLogger(s);
end;

procedure TBPManager.AddBP(address: dword);
var
  PageAddr: dword;
  i, j: integer;
  page: ^TFlashPageBPs;
  newBP: boolean = true;
begin
  if address >= FDebugWire.Device.flashSize then
  begin
    FLog('Error in AddBP - address exceeds flash size');
    exit;
  end;

  // First check hw BP
  // If not assigned yet, asign
  // if address matches previous address, enable
  // if disabled, will be re-assigned in FinalizeBPs
  if (HWBP.address = -1) or (HWBP.address = address) then
  begin
    HWBP.address := address;
    HWBP.active := true;
  end
  else
  begin
    PageAddr := address and FPageMask;
    // locate page or insert position
    if length(FlashPagesWithBPs) = 0 then
    begin
      SetLength(FlashPagesWithBPs, 1);
      i := 0;
      FlashPagesWithBPs[i].PageAddress := PageAddr;
    end
    else
    begin
      i := 0;
      while (i < length(FlashPagesWithBPs)) and (FlashPagesWithBPs[i].PageAddress < PageAddr) do
        inc(i);

      if i = length(FlashPagesWithBPs) then // add record
      begin
        SetLength(FlashPagesWithBPs, i+1);
        FlashPagesWithBPs[i].PageAddress := PageAddr;
        SetLength(FlashPagesWithBPs[i].BreakPoints, 0);
      end
      else if FlashPagesWithBPs[i].PageAddress > PageAddr then // insert record
      begin
        SetLength(FlashPagesWithBPs, length(FlashPagesWithBPs)+1);
        // now move BPs down to create space...
        for j := length(FlashPagesWithBPs)-1 to i+1 do
        begin
          FCopyFlashPageWithBPs(FlashPagesWithBPs[j-1], FlashPagesWithBPs[j]);
        end;
        FlashPagesWithBPs[i].PageAddress := PageAddr;
        SetLength(FlashPagesWithBPs[i].BreakPoints, 0);
      end;
    end;

    // Now i points to appropriate record
    page := @(FlashPagesWithBPs[i]);
    if length(page^.BreakPoints) = 0 then
    begin
      SetLength(page^.BreakPoints, 1);
      i := 0;
    end
    else
    begin
      i := 0;
      while (i < length(page^.BreakPoints)) and (page^.BreakPoints[i].address < address) do
        inc(i);

      if i = length(page^.BreakPoints) then // add record
        SetLength(page^.BreakPoints, i+1)
      else if page^.BreakPoints[i].address > address then // insert record
      begin
        SetLength(page^.BreakPoints, length(page^.BreakPoints)+1);
        // now move BPs down to create space...
        for j := length(page^.BreakPoints)-2 to i do
        begin
          page^.BreakPoints[j+1] := page^.BreakPoints[j];
          page^.BreakPoints[j+1].origCode := copy(page^.BreakPoints[j].origCode);
        end;
      end
      else
        newBP := false;
    end;

    if newBP then
    begin
      page^.BreakPoints[i].address := address;
      page^.BreakPoints[i].active := true;
      page^.BreakPoints[i].written := false;
      page^.BreakPoints[i].bpsize := 2;
    end
    else
      page^.BreakPoints[i].active := true;
  end;
end;

function TBPManager.FBPcountPerFlashPage(address: dword): integer;
var
  PageAddr: dword;
  i: integer;
begin
  PageAddr := address and FPageMask;
  i := 1;
  while (i < Length(FlashPagesWithBPs)) and
        (FlashPagesWithBPs[i].PageAddress <> PageAddr) do
  begin
    inc(i);
  end;

  if i < Length(FlashPagesWithBPs) then
    Result := length(FlashPagesWithBPs[i].BreakPoints)
  else
    Result := 0;
end;

procedure TBPManager.FCopyBP(const source: TBP; var dest: TBP);
begin
  dest.active := source.active;
  dest.address := source.address;
  dest.bpsize := source.bpsize;
  dest.written := source.written;
  dest.origCode := copy(source.origCode);
end;

procedure TBPManager.FCopyFlashPageWithBPs(const soure: TFlashPageBPs;
  var dest: TFlashPageBPs);
var
  i: integer;
begin
  dest.PageAddress := soure.PageAddress;
  SetLength(dest.BreakPoints, length(soure.BreakPoints));
  for i := 0 to high(soure.BreakPoints) do
    FCopyBP(soure.BreakPoints[i], dest.BreakPoints[i]);
end;

constructor TBPManager.Create(dw: TDebugWire; log: TLog);
begin
  FDebugWire := dw;
  FLogger := log;

  FPageOffsetMask := FDebugWire.Device.FlashPageSize - 1;
  FPageMask := $FFFFFFFF - FPageOffsetMask;
  FlashStart := 32 + FDebugWire.Device.ioregSize;
  HWBP.address := -1;
  HWBP.active := false;
end;

procedure TBPManager.DeleteBP(address: dword);
var
  BP: PBP;
begin
  // Out of range addresses rejected in AddBP, so just exit here, no need for further cleanup
  if address >= FDebugWire.Device.flashSize then
  begin
    FLog('Error in DeleteBP - address exceeds flash size');
    exit;
  end;

  if HWBP.address = address then
    HWBP.active := false
  else
  begin
    BP := findSWBPFromAddress(address);
    if BP <> nil then
      BP^.active := false
    else
      FLog('BP ERROR - breakpoint $' + hexStr(address, 4) + ' not found for deletion');
  end;
end;

procedure TBPManager.DeleteAllBPs;
var
  i, j: integer;
begin
  HWBP.active := false;

  i := 0;
  while (i < length(FlashPagesWithBPs)) do
  begin
    j := 0;
    while j < length(FlashPagesWithBPs[i].BreakPoints) do
    begin
      FlashPagesWithBPs[i].BreakPoints[j].active := false;
      inc(j);
    end;

    inc(i);
  end;
end;

procedure TBPManager.FinalizeBPs;
var
  i, j, k, index: integer;
  BreakOpcode: TBytes;
  hwbInUse, MustLoadPage, MustSavePage: boolean;
  tempPage: TBytes;
  current: PBP;
begin
  SetLength(BreakOpcode, 2);
  BreakOpcode[0] := $98; BreakOpcode[1] := $95;

  // Check HW BP
  if HWBP.active then
  begin
    FDebugWire.BP := HWBP.address;
    hwbInUse := true;
  end
  else
  begin
    FDebugWire.BP := -1;
    hwbInUse := false;
  end;

  // Update active and delete inactive BP
  i := 0;
  while (i < length(FlashPagesWithBPs)) do  // iterate over flash pages
  begin
    j := 0;
    MustLoadPage := true;   // set to false after first read
    MustSavePage := false;
    while (j < length(FlashPagesWithBPs[i].BreakPoints)) do  // iterate over BPs in current flash page
    begin
      current := @FlashPagesWithBPs[i].BreakPoints[j];

      // Check if active but SW break not written yet
      // Move to hw if not in use, else write to flash
      if current^.active and not(current^.written) then
      begin
        if not hwbInUse then // No penalty to move BP to HW because it wasn't written to flash yet
        begin
          HWBP.address := current^.address;
          HWBP.active := true;
          hwbInUse := true;
          FDebugWire.BP := current^.address;
          current^.active := false;
        end
        else
        begin
          if MustLoadPage then
          begin
            FDebugWire.ReadFlash(FlashPagesWithBPs[i].PageAddress, FDebugWire.Device.FlashPageSize, tempPage);
            MustLoadPage := false;
            MustSavePage := true;
          end;

          // Store opcode then write break opcode
          index := current^.address and FPageOffsetMask;
          if length(current^.origCode) < 2 then
            SetLength(current^.origCode, 2);

          current^.origCode[0] := tempPage[index];
          current^.origCode[1] := tempPage[index+1];
          tempPage[index] := BreakOpcode[0];
          tempPage[index+1] := BreakOpcode[1];

          current^.written := true;
        end;
      end;

      // sw break saved but now inactive
      // Restore original opcode and mark inactive
      if not (current^.active) and current^.written then
      begin
        if MustLoadPage then
        begin
          FDebugWire.ReadFlash(FlashPagesWithBPs[i].PageAddress, FDebugWire.Device.FlashPageSize, tempPage);
          MustLoadPage := false;
          MustSavePage := true;
        end;

        // Restore original opcode then deactivate BP
        index := current^.address and FPageOffsetMask;
        tempPage[index] := current^.origCode[0];
        tempPage[index+1] := current^.origCode[1];
        MustSavePage := true;
        current^.written := false;
      end;

      // sw inactive, delete from list
      if not (current^.active) and not (current^.written) then
      begin
        for k := j to high(FlashPagesWithBPs[i].BreakPoints)-1 do
        begin
          FCopyBP(FlashPagesWithBPs[i].BreakPoints[j+1], FlashPagesWithBPs[i].BreakPoints[j]);
        end;

        SetLength(FlashPagesWithBPs[i].BreakPoints, length(FlashPagesWithBPs[i].BreakPoints)-1);
        dec(j);  // hack to keep j pointing to the next record which now shifted up 1 position
      end;

      inc(j);
    end;

    if MustSavePage then
    begin
      FLog('Update breaks & opcodes for page: ' + hexStr(FlashPagesWithBPs[i].PageAddress, 4));
      FDebugWire.WriteFlash(FlashPagesWithBPs[i].PageAddress, tempPage);
    end;

    // check if there are BPs left, delete this page item if not
    if length(FlashPagesWithBPs[i].BreakPoints) = 0 then
    begin
      for k := i to length(FlashPagesWithBPs)-2 do
      begin
        FCopyFlashPageWithBPs(FlashPagesWithBPs[k+1], FlashPagesWithBPs[k]);
      end;
      SetLength(FlashPagesWithBPs, length(FlashPagesWithBPs)-1);
      dec(i);  // hack to keep i pointing to the next record which now shifted up 1 position
    end;

    inc(i);
  end;
end;

procedure TBPManager.PrintBPs;
var
  i, j: integer;
  bp: ^TBP;
begin
  FLog('HW break point address: $' + hexStr(FDebugWire.BP, 6));
  FLog('SW break point map:');
  for i := 0 to high(FlashPagesWithBPs) do
  begin
    FLog(format('Page: %2d  Page address: $%.6x', [i, FlashPagesWithBPs[i].PageAddress]));
    for j := 0 to high(FlashPagesWithBPs[i].BreakPoints) do
    begin
      bp := @FlashPagesWithBPs[i].BreakPoints[j];
      if length(bp^.origCode) = 2 then
        FLog(format('Address: $%.6x Active: %s Written: %s Size: %d OpCode: $%.2x%.2x',
          [bp^.address, BoolToStr(bp^.active, true), BoolToStr(bp^.written, true),
           bp^.bpsize, bp^.origCode[1], bp^.origCode[0]]))
      else
      FLog(format('Address: $%.6x Active: %s Written: %s Size: %d OpCode: N/A',
        [bp^.address, BoolToStr(bp^.active, true), BoolToStr(bp^.written, true),
         bp^.bpsize]));
    end;
  end;
end;

function TBPManager.findSWBPFromAddress(address: word): PBP;
var
  i, j: integer;
  thisPage: word;
begin
  thisPage := address and FPageMask;
  i := 0;
  while (i < length(FlashPagesWithBPs)) and (FlashPagesWithBPs[i].PageAddress <> thisPage) do
  begin
    inc(i);
  end;

  if (i < length(FlashPagesWithBPs)) then
  begin
    j := 0;
    while (j < length(FlashPagesWithBPs[i].BreakPoints)) and
          (FlashPagesWithBPs[i].BreakPoints[j].address <> address) do
    begin
      inc(j);
    end;
    if (j < length(FlashPagesWithBPs[i].BreakPoints)) then
    begin
      Result := @FlashPagesWithBPs[i].BreakPoints[j]
    end
    else
      Result := nil;
  end
  else
    Result := nil;
end;

end.

