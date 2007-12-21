unit document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  constants, dbcomponents;

type

  { TDocument }

  TDocument = class(TObject)
  public
    { Persistent information of the user interface }
    CurrentTool: TCTool;
    NewComponentOrientation: TCComponentOrientation;
    { Selection fields }
    SelectedComponent: PTCComponent;
    SelectedWire: PTCWire;
    SelectedWirePart: TCWirePart;
    { Document information }
    SheetWidth, SheetHeight: Integer;
    Components: PTCComponent;
    Wires: PTCWire;
    { Base methods }
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AStream: TStream);
    { General document methods }
    function  GetDocumentPos(X, Y: Integer): TPoint;
    { Components methods }
    procedure ClearComponents;
    function  GetComponentCount: Cardinal;
    procedure InsertComponent(AComponent: PTCComponent);
    procedure MoveComponent(AComponent: PTCComponent; ADelta: TPoint);
    procedure RemoveComponent(AComponent: PTCComponent);
    procedure RotateNewComponentOrientation;
    function  SearchComponent(Pos: TPoint): PTCComponent;
    { Wires methods }
    procedure ClearWires;
    function  GetWireCount: Cardinal;
    procedure InsertWire(AWire: PTCWire);
    procedure MoveWire(AWire: PTCWire; APos: TPoint; APart: TCWirePart);
    procedure RemoveWire(AWire: PTCWire);
    function  SearchWire(Pos: TPoint): PTCWire;
    { Selection methods }
    procedure ClearSelection;
    function  IsSomethingSelected: Boolean;
  end;

var
  vDocument: TDocument;

implementation

{ TDocument }

constructor TDocument.Create;
begin
  inherited Create;

  CurrentTool := toolArrow;
  NewComponentOrientation := coEast;

  SheetWidth := INT_SHEET_DEFAULT_WIDTH;
  SheetHeight := INT_SHEET_DEFAULT_HEIGHT;
  Components := nil;
  Wires := nil;
end;

destructor TDocument.Destroy;
var
  AComponent, NextComponent: PTCComponent;
  AWire, NextWire: PTCWire;
begin
  { Cleans the memory of the components }
  ClearComponents;

  { Cleans the memory of the wires }
  ClearWires;

  inherited Destroy;
end;

procedure TDocument.LoadFromFile(AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AFileStream);
  finally
    AFileStream.Free;
  end;
end;

procedure TDocument.LoadFromStream(AStream: TStream);
var
  AComponent, LastComponent: PTCComponent;
  AWire, LastWire: PTCWire;
  i, NewComponentCount, NewWireCount: Cardinal;
  AStr: string;
begin
  { clears old data }
  ClearComponents;
  ClearWires;

  { First the identifier of any TurboCircuit schematics file }
  AStr := AStream.ReadAnsiString;

  { Persistent information of the user interface }
  CurrentTool := TCTool(AStream.ReadDWord);
  NewComponentOrientation := TCComponentOrientation(AStream.ReadDWord);
    { Selection fields }
//    SelectedComponent: PTCComponent;
//    SelectedWire: PTCWire;
//    SelectedWirePart: TCWirePart;
    { Document information }
  SheetWidth := AStream.ReadDWord;
  SheetHeight := AStream.ReadDWord;

  { Reads the components }
  NewComponentCount := AStream.ReadDWord();

  AComponent := GetMem(SizeOf(TCComponent));
  Components := AComponent;

  AStream.Read(AComponent^, SizeOf(TCComponent));

  for i := 1 to NewComponentCount - 1 do
  begin
    LastComponent := AComponent;
    AComponent := GetMem(SizeOf(TCComponent));
    LastComponent^.Next := AComponent;
    AComponent^.Previous := LastComponent;

    AStream.Read(AComponent^, SizeOf(TCComponent));
  end;

  { Reads the wires }
  NewWireCount := AStream.ReadDWord();

  AWire := GetMem(SizeOf(TCWire));
  Wires := AWire;

  AStream.Read(AWire^, SizeOf(TCWire));

  for i := 1 to NewWireCount - 1 do
  begin
    LastWire := AWire;
    AWire := GetMem(SizeOf(TCWire));
    LastWire^.Next := AWire;
    AWire^.Previous := LastWire;

    AStream.Read(AWire^, SizeOf(TCWire));
  end;
end;

procedure TDocument.SaveToFile(AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(AFileStream);
  finally
    AFileStream.Free;
  end;
end;

procedure TDocument.SaveToStream(AStream: TStream);
var
  AComponent: PTCComponent;
  AWire: PTCWire;
begin
  { First the identifier of any TurboCircuit schematics file }
  AStream.WriteAnsiString(STR_TCSCHEMATICS_IDENTIFIER);
  
  { Persistent information of the user interface }
  AStream.WriteDWord(LongWord(CurrentTool));
  AStream.WriteDWord(LongWord(NewComponentOrientation));
  { Selection fields }
//    SelectedComponent: PTCComponent;
//    SelectedWire: PTCWire;
//    SelectedWirePart: TCWirePart;
  { Document information }
  AStream.WriteDWord(SheetWidth);
  AStream.WriteDWord(SheetHeight);

  { Stores the components }
  AStream.WriteDWord(GetComponentCount);

  AComponent := Components;
  while AComponent <> nil do
  begin
    AStream.Write(AComponent^, SizeOf(TCComponent));
  
    AComponent := AComponent^.Next;
  end;
  
  { Stores the wires }
  AStream.WriteDWord(GetWireCount);

  AWire := Wires;
  while AWire <> nil do
  begin
    AStream.Write(AWire^, SizeOf(TCWire));

    AWire := AWire^.Next;
  end;
end;

function TDocument.GetDocumentPos(X, Y: Integer): TPoint;
begin
  Result.X := Round(X / INT_SHEET_GRID_SPACING);
  Result.Y := Round(Y / INT_SHEET_GRID_SPACING);
end;

procedure TDocument.ClearComponents;
var
  AComponent, NextComponent: PTCComponent;
begin
  { Cleans the memory of the components }
  AComponent := Components;
  while (AComponent <> nil) do
  begin
    NextComponent := AComponent^.Next;
    FreeMem(AComponent);
    AComponent := NextComponent;
  end;
  
  Components := nil;
end;

function TDocument.GetComponentCount: Cardinal;
var
  AComponent: PTCComponent;
begin
  Result := 0;
  
  AComponent := Components;
  while AComponent <> nil do
  begin
    Inc(Result);
    AComponent := AComponent^.Next;
  end;
end;

procedure TDocument.InsertComponent(AComponent: PTCComponent);
var
  LastComponent: PTCComponent;
begin
  AComponent^.Next := nil;
  AComponent^.Previous := nil;

  { Handles the case where we don't have any items yet }
  if (Components = nil) then
   Components := AComponent
  else
  begin
    { Finds the last item }
    LastComponent := Components;
    
    while LastComponent^.Next <> nil do
     LastComponent := LastComponent^.Next;
     
    { Stablishes the links between the items }
    LastComponent^.Next := AComponent;
    AComponent^.Previous := LastComponent;
  end;
end;

procedure TDocument.MoveComponent(AComponent: PTCComponent; ADelta: TPoint);
begin
  AComponent^.PosX := AComponent^.PosX + ADelta.X;
  AComponent^.PosY := AComponent^.PosY + ADelta.Y;
end;

procedure TDocument.RemoveComponent(AComponent: PTCComponent);
begin
  if Assigned(AComponent^.Previous) then AComponent^.Previous^.Next := AComponent^.Next;

  if Assigned(AComponent^.Next) then AComponent^.Next^.Previous := AComponent^.Previous;

  FreeMem(AComponent);
end;

procedure TDocument.RotateNewComponentOrientation;
begin
  case NewComponentOrientation of
    coEast:  NewComponentOrientation := coNorth;
    coNorth: NewComponentOrientation := coWest;
    coWest:  NewComponentOrientation := coSouth;
    coSouth: NewComponentOrientation := coEast;
  end;
end;

function TDocument.SearchComponent(Pos: TPoint): PTCComponent;
var
  AComponent: PTCComponent;
  ACompHeight, ACompWidth: Integer;
begin
  Result := nil;

  AComponent := Components;

  while AComponent <> nil do
  begin
    ACompHeight := vComponentsDatabase.GetHeight(AComponent^.TypeID);
    ACompWidth := vComponentsDatabase.GetWidth(AComponent^.TypeID);
  
    if (AComponent^.PosX < Pos.X) and (Pos.X < AComponent^.PosX + ACompWidth)
     and (AComponent^.PosY < Pos.Y) and (Pos.Y < AComponent^.PosY + ACompHeight) then
    begin
      Result := AComponent;
    
      Exit;
    end;

    AComponent := AComponent^.Next;
  end;
end;

procedure TDocument.ClearWires;
var
  AWire, NextWire: PTCWire;
begin
  { Cleans the memory of the wires }
  AWire := Wires;
  while (AWire <> nil) do
  begin
    NextWire := AWire^.Next;
    FreeMem(AWire);
    AWire := NextWire;
  end;

  Wires := nil;
end;

function TDocument.GetWireCount: Cardinal;
var
  AWire: PTCWire;
begin
  Result := 0;

  AWire := Wires;
  while AWire <> nil do
  begin
    Inc(Result);
    AWire := AWire^.Next;
  end;
end;

procedure TDocument.InsertWire(AWire: PTCWire);
var
  LastWire: PTCWire;
begin
  AWire^.Next := nil;
  AWire^.Previous := nil;

  { Handles the case where we don't have any items yet }
  if (Wires = nil) then
   Wires := AWire
  else
  begin
    { Finds the last item }
    LastWire := Wires;

    while LastWire^.Next <> nil do
     LastWire := LastWire^.Next;

    { Stablishes the links between the items }
    LastWire^.Next := AWire;
    AWire^.Previous := LastWire;
  end;
end;

procedure TDocument.MoveWire(AWire: PTCWire; APos: TPoint; APart: TCWirePart);
begin
  case APart of
    wpPtFrom: AWire^.PtFrom := APos;
    wpPtTo:   AWire^.PtTo := APos;
  end;
end;

procedure TDocument.RemoveWire(AWire: PTCWire);
begin
  if Assigned(AWire^.Previous) then AWire^.Previous^.Next := AWire^.Next;

  if Assigned(AWire^.Next) then AWire^.Next^.Previous := AWire^.Previous;

  FreeMem(AWire);
end;

function TDocument.SearchWire(Pos: TPoint): PTCWire;
var
  AWire: PTCWire;
begin
  Result := nil;

  AWire := Wires;

  while AWire <> nil do
  begin
    { Verifies PtFrom }
    if (AWire^.PtFrom.X = Pos.X) and (AWire^.PtFrom.Y = Pos.Y) then
    begin
      Result := AWire;
      SelectedWirePart := wpPtFrom;

      Exit;
    end;

    { Verifies PtTo }
    if (AWire^.PtTo.X = Pos.X) and (AWire^.PtTo.Y = Pos.Y) then
    begin
      Result := AWire;
      SelectedWirePart := wpPtTo;

      Exit;
    end;

    AWire := AWire^.Next;
  end;
end;

procedure TDocument.ClearSelection;
begin
  SelectedComponent := nil;
  SelectedWire := nil;
end;

function TDocument.IsSomethingSelected: Boolean;
begin
  Result := (SelectedComponent <> nil) or (SelectedWire <> nil);
end;

initialization

  vDocument := TDocument.Create;

finalization

  vDocument.Free;

end.

