unit tclists;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Graphics,
  constants, dbcomponents, tcutils;

type

  {@@
   @see TCElementList.ForEachDoPaint
  }
  TCElementListPaintCallback = procedure(ACanvas: TCanvas; AElement: PTCElement) of object;

  TCElementListWriteCallback = procedure(AStream: TStream; AElement: PTCElement) of object;

  { TCElementList }

  PTCElementList = ^TCElementList;

  TCElementList = class(TObject)
  protected
    { Element-specific utility methods }
    {@@
     @see #FindElement
    }
    function  DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord; virtual; abstract;
  public
    Elements: PTCElement;
  public
    constructor Create;
    destructor Destroy; override;
    { Common utility methods }
    procedure Clear;
    function  FindElement(Pos: TPoint; var AElement: PTCElement): DWord;
    procedure ForEachDoPaint(ACanvas: TCanvas; ACallback: TCElementListPaintCallback);
    procedure ForEachDoWrite(AStream: TStream; ACallback: TCElementListWriteCallback);
    function  GetCount: Cardinal;
    procedure Insert(AElement: PTCElement);
    procedure MoveElement(AElement: PTCElement; ADelta: TPoint);
    procedure Remove(AElement: PTCElement);
  end;

  { TCComponentList }

  TCComponentList = class(TCElementList)
  protected
    { Element-specific utility methods }
    function  DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord; override;
  public
    procedure WriteDebugInfo();
  end;

  { TCWireList }

  TCWireList = class(TCElementList)
  protected
    { Element-specific utility methods }
    function  DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord; override;
  public
    { Methods specific for wires }
    procedure MoveWire(AWire: PTCWire; APos: TPoint; APart: DWord);
  end;

  { TCTextList }

  TCTextList = class(TCElementList)
  protected
    { Element-specific utility methods }
    function  DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord; override;
  end;

  { TCPolylineList }

  TCPolylineList = class(TCElementList)
  protected
    { Element-specific utility methods }
    function  DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord; override;
  public
    { Methods specific for polylines }
    procedure AddPoint(APolyline: PTCPolyline; APoint: TPoint);
    //procedure RemovePoint(APolyline: PTCPolyline; AIndex: Integer);
    function  GetLastPoint(APolyline: PTCPolyline): TPoint;
  end;

implementation

{ TCElementList }

constructor TCElementList.Create;
begin
  inherited Create;
  
  Elements := nil;
end;

destructor TCElementList.Destroy;
begin
  Clear;

  inherited Destroy;
end;

{@@
  Clears the list of elements and frees the memory associated with them,
  efectively reseting the list.
}
procedure TCElementList.Clear;
var
  AElement, NextElement: PTCElement;
begin
  AElement := Elements;
  while (AElement <> nil) do
  begin
    NextElement := AElement^.Next;
    FreeMem(AElement);
    AElement := NextElement;
  end;

  Elements := nil;
end;

{@@
  Calls a paint callback for each element on the list
}
procedure TCElementList.ForEachDoPaint(ACanvas: TCanvas;
  ACallback: TCElementListPaintCallback);
var
  NextElement: PTCElement;
begin
  if Elements = nil then Exit;

  NextElement := Elements;

  while (NextElement <> nil) do
  begin
    ACallback(ACanvas, NextElement);

    NextElement := NextElement^.Next;
  end;
end;

procedure TCElementList.ForEachDoWrite(AStream: TStream;
  ACallback: TCElementListWriteCallback);
var
  NextElement: PTCElement;
begin
  if Elements = nil then Exit;

  NextElement := Elements;

  while (NextElement <> nil) do
  begin
    ACallback(AStream, NextElement);

    NextElement := NextElement^.Next;
  end;
end;

function TCElementList.GetCount: Cardinal;
var
  AElement: PTCElement;
begin
  Result := 0;

  AElement := Elements;
  while AElement <> nil do
  begin
    Inc(Result);
    AElement := AElement^.Next;
  end;
end;

procedure TCElementList.Insert(AElement: PTCElement);
var
  LastElement: PTCElement;
begin
  AElement^.Next := nil;
  AElement^.Previous := nil;

  { Handles the case where we don't have any items yet }
  if (Elements = nil) then
   Elements := AElement
  else
  begin
    { Finds the last item }
    LastElement := Elements;

    while LastElement^.Next <> nil do
     LastElement := LastElement^.Next;

    { Stablishes the links between the items }
    LastElement^.Next := AElement;
    AElement^.Previous := LastElement;
  end;
end;

procedure TCElementList.MoveElement(AElement: PTCElement; ADelta: TPoint);
begin
  AElement^.Pos.X := AElement^.Pos.X + ADelta.X;
  AElement^.Pos.Y := AElement^.Pos.Y + ADelta.Y;
end;

procedure TCElementList.Remove(AElement: PTCElement);
begin
  if AElement = nil then Exit;

  if Assigned(AElement^.Previous) then
   AElement^.Previous^.Next := AElement^.Next;

  if Assigned(AElement^.Next) then
   AElement^.Next^.Previous := AElement^.Previous;

  if (Elements = AElement) then
  begin
    Elements := AElement^.Next;
    if Assigned(AElement^.Next) then
      AElement^.Next^.Previous := nil;
  end;

  Dispose(AElement);
end;

function TCElementList.FindElement(Pos: TPoint; var AElement: PTCElement): DWord;
var
  NextElement: PTCElement;
begin
  Result := ELEMENT_DOES_NOT_MATCH;

  if Elements = nil then Exit;

  NextElement := Elements;

  while (NextElement <> nil) do
  begin
    Result := DoVerifyElementPos(Pos, NextElement);

    if Result <> ELEMENT_DOES_NOT_MATCH then
    begin
       AElement := NextElement;
       Exit;
    end;

    NextElement := NextElement^.Next;
  end;
end;

{ TCComponentList }

function TCComponentList.DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord;
var
  AComponent: PTCComponent absolute AElement;
  ACompHeight, ACompWidth, ABaseCompHeight, ABaseCompWidth: Integer;
begin
  Result := ELEMENT_DOES_NOT_MATCH;

  vComponentsDatabase.GoToRecByID(AComponent^.TypeID);
  ACompHeight := vComponentsDatabase.GetHeight();
  ACompWidth := vComponentsDatabase.GetWidth();

  case AComponent.Orientation of

  coEast:
  if (AComponent^.Pos.X < Pos.X) and (Pos.X < AComponent^.Pos.X + ACompWidth)
   and (AComponent^.Pos.Y < Pos.Y) and (Pos.Y < AComponent^.Pos.Y + ACompHeight) then
    Result := ELEMENT_MATCHES;

  coNorth:
  if (AComponent^.Pos.X < Pos.X) and (Pos.X < AComponent^.Pos.X + ACompHeight)
   and (AComponent^.Pos.Y - ACompWidth < Pos.Y) and (Pos.Y < AComponent^.Pos.Y) then
    Result := ELEMENT_MATCHES;

  coWest:
  if (AComponent^.Pos.X < Pos.X - ACompWidth) and (Pos.X < AComponent^.Pos.X)
   and (AComponent^.Pos.Y - ACompHeight < Pos.Y) and (Pos.Y < AComponent^.Pos.Y) then
    Result := ELEMENT_MATCHES;

  coSouth:
  if (AComponent^.Pos.X - ACompHeight< Pos.X) and (Pos.X < AComponent^.Pos.X)
   and (AComponent^.Pos.Y < Pos.Y) and (Pos.Y < AComponent^.Pos.Y + ACompWidth) then
    Result := ELEMENT_MATCHES;

  end;
end;

procedure TCComponentList.WriteDebugInfo();
var
  NextComponent: PTCComponent;
  i: Integer;
begin
  NextComponent := PTCComponent(Elements);
  i := 0;

  while (NextComponent <> nil) do
  begin
    WriteLn('#Component #' + IntToStr(i) + ' TypeID: ' + NextComponent^.TypeID);

    NextComponent := PTCComponent(NextComponent^.Next);
    Inc(i)
  end;
  WriteLn('');
end;

{ TCWireList }

function TCWireList.DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord;
var
  AWire: PTCWire absolute AElement;
begin
  Result := ELEMENT_DOES_NOT_MATCH;

  { Verifies PtFrom }
  if (AWire^.Pos.X = Pos.X) and (AWire^.Pos.Y = Pos.Y) then
  begin
    Result := ELEMENT_START_POINT;
  end
  { Verifies PtTo }
  else if (AWire^.PtTo.X = Pos.X) and (AWire^.PtTo.Y = Pos.Y) then
  begin
    Result := ELEMENT_END_POINT;
  end;
end;

procedure TCWireList.MoveWire(AWire: PTCWire; APos: TPoint; APart: DWord);
begin
  case APart of
    ELEMENT_START_POINT: AWire^.Pos := APos;
    ELEMENT_END_POINT:   AWire^.PtTo := APos;
  end;
end;

{ TCTextList }

function TCTextList.DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord;
var
  AElementHeight, AElementWidth: Integer;
begin
  Result := ELEMENT_DOES_NOT_MATCH;

  AElementHeight := 2;
  AElementWidth := 4;

  if (AElement^.Pos.X < Pos.X) and (Pos.X < AElement^.Pos.X + AElementWidth)
   and (AElement^.Pos.Y < Pos.Y) and (Pos.Y < AElement^.Pos.Y + AElementHeight) then
     Result := ELEMENT_MATCHES;
end;

{ TCPolylineList }

function TCPolylineList.DoVerifyElementPos(Pos: TPoint; AElement: PTCElement): DWord;
var
  APolyline: PTCPolyline absolute AElement;
  i: Integer;
begin
  Result := ELEMENT_DOES_NOT_MATCH;

  { Verifies Position }
  if (APolyline^.Pos.X = Pos.X) and (APolyline^.Pos.Y = Pos.Y) then
  begin
    Result := ELEMENT_START_POINT;
  end
  { Verifies all other points }
  else
  begin
    for i := 0 to Length(APolyline^.Points) do
     if APolyline^.Points[i] = Pos then Exit(ELEMENT_END_POINT + i);
  end;
end;

procedure TCPolylineList.AddPoint(APolyline: PTCPolyline; APoint: TPoint);
begin
  APolyline^.Points[APolyline^.NPoints] := APoint;
  Inc(APolyline^.NPoints);
end;

function TCPolylineList.GetLastPoint(APolyline: PTCPolyline): TPoint;
begin
  if APolyline^.NPoints > 0 then
    Result := APolyline^.Points[APolyline^.NPoints - 1]
  else
    Result := APolyline^.Pos;
end;

end.

