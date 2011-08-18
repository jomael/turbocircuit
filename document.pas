unit document;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLIntf,
  constants, dbcomponents, tcfileformat, tclists,
  // fpvectorial
  fpvectorial;

type

  { TDocument }

  TDocument = class(TvVectorialDocument)
  private
    { Helper routines }
    FDestroying: Boolean;
    procedure UpdateDocumentInfo(AIsSaved: Boolean);
    function  ReadBOF(AStream: TStream): Boolean;
    procedure WriteBOF(AStream: TStream);
    procedure WriteSCHEMATICS_GUI_DATA(AStream: TStream);
    procedure WriteSCHEMATICS_DOC_DATA(AStream: TStream);
    procedure WriteCOMPONENT(AStream: TStream; AElement: PTCElement);
    procedure WriteWIRE(AStream: TStream; AElement: PTCElement);
    procedure WriteTEXT(AStream: TStream; AElement: PTCElement);
    procedure WritePOLYLINE(AStream: TStream; AElement: PTCElement);
    procedure WriteEOF(AStream: TStream);
  public
    { Callback for changes in the UI info }
    UIChangeCallback: TNotifyEvent;
    { Non-Persistent information of the user interface }
    Modified: Boolean;
    Saved: Boolean;
    FileName: string;
    NewItemOrientation: TCComponentOrientation;
    CurrentTool: TCTool;
    PosX, PosY: Double; // in milimeters
    { Persistent information of the user interface }
    Title: string;
    SnapToGrid: Boolean;
    ShowGrid: Boolean;
    { Selection fields }
    SelectedElement: PTCElement;
    SelectedElementType: TCTool;
    SelectionInfo: DWord;
    SelectionvInfo: TvFindEntityResult;
    { Document information }
    Components: TCComponentList;
    Wires: TCWireList;
    TextList: TCTextList;
    Polylines: TCPolylineList;
    RasterImages: TCRasterImageList;
    Ellipses: TCEllipseList;
    { Base methods }
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    //procedure LoadFromFile(AFileName: string);
    //procedure LoadFromStream(AStream: TStream);
    //procedure SaveToFile(AFileName: string);
    //procedure SaveToStream(AStream: TStream);
    { General document methods }
    function  GetDocumentPos(X, Y: Integer): TPoint;
    function  GetVectorialDocumentPos(X, Y: Integer): TPoint;
    { Components methods }
    procedure RotateOrientation(var AOrientation: TCComponentOrientation);
    function  GetComponentTopLeft(AComponent: PTCComponent): TPoint;
    { Selection methods }
    procedure ClearSelection;
    function  IsSelected(AType: TCTool): Boolean;
    function  IsSomethingSelected: Boolean;
    function  GetListForElement(AType: TCTool): PTCElementList;
    function  GetSelectedComponent: PTCComponent;
    function  GetSelectedWire: PTCWire;
    function  GetSelectedText: PTCText;
    function  GetSelectedPolyline: PTCPolyline;
    function  GetSelectedRasterImage: PTCRasterImage;
  end;

var
  vDocument: TDocument;

implementation

{ TDocument }

procedure TDocument.UpdateDocumentInfo(AIsSaved: Boolean);
begin
  if AIsSaved then
  begin
    { Non-Persistent information of the user interface }
    Modified := False;
    Saved := True;
  end
  else
  begin
    { Non-Persistent information of the user interface }
    Modified := False;
    Saved := False;
  end;

  { Update the UI with the changes }
  if Assigned(UIChangeCallback) then UIChangeCallback(Self);
end;

function TDocument.ReadBOF(AStream: TStream): Boolean;
var
  vID: array[0..INT_TCFILE_IDENTIFIER_SIZE-1] of Char;
begin
  Result := False;

  AStream.ReadBuffer(vID, INT_TCFILE_IDENTIFIER_SIZE);

  if not CompareMem(@vID, @STR_TCFILE_IDENTIFIER[1], INT_TCFILE_IDENTIFIER_SIZE) then Exit;

  if AStream.ReadByte <> TCRECORD_BOF then Exit;
  
  AStream.ReadByte;
  AStream.ReadWord;
  
  Result := True;
end;

procedure TDocument.WriteBOF(AStream: TStream);
begin
  AStream.WriteBuffer(STR_TCFILE_IDENTIFIER[1], INT_TCFILE_IDENTIFIER_SIZE);
  AStream.WriteByte(TCRECORD_BOF);
  AStream.WriteByte(TCRECORD_BOF_VER);
  AStream.WriteWord($0);
end;

procedure TDocument.WriteSCHEMATICS_GUI_DATA(AStream: TStream);
begin

end;

procedure TDocument.WriteSCHEMATICS_DOC_DATA(AStream: TStream);
begin

end;

procedure TDocument.WriteCOMPONENT(AStream: TStream; AElement: PTCElement);
var
  AComponent: PTCComponent absolute AElement;
begin
  AStream.WriteByte(TCRECORD_COMPONENT);
  AStream.WriteByte(TCRECORD_COMPONENT_VER);
  AStream.WriteWord(TCRECORD_COMPONENT_SIZE);
  AStream.WriteBuffer(AComponent^, SizeOf(TCComponent));
end;

procedure TDocument.WriteWIRE(AStream: TStream; AElement: PTCElement);
var
  AWire: PTCWire absolute AElement;
begin
  AStream.WriteByte(TCRECORD_WIRE);
  AStream.WriteByte(TCRECORD_WIRE_VER);
  AStream.WriteWord(TCRECORD_WIRE_SIZE);
  AStream.WriteBuffer(AWire^, SizeOf(TCWire));
end;

procedure TDocument.WriteTEXT(AStream: TStream; AElement: PTCElement);
var
  AText: PTCText absolute AElement;
begin
  AStream.WriteByte(TCRECORD_TEXT);
  AStream.WriteByte(TCRECORD_TEXT_VER);
  AStream.WriteWord(TCRECORD_TEXT_SIZE);
  AStream.WriteBuffer(AText^, SizeOf(TCText));
end;

procedure TDocument.WritePOLYLINE(AStream: TStream; AElement: PTCElement);
var
  APolyline: PTCPolyline absolute AElement;
begin
  AStream.WriteByte(TCRECORD_POLYLINE);
  AStream.WriteByte(TCRECORD_POLYLINE_VER);
  AStream.WriteWord(TCRECORD_POLYLINE_SIZE);
  AStream.WriteBuffer(APolyline^, SizeOf(APolyline));
end;

procedure TDocument.WriteEOF(AStream: TStream);
begin

end;

constructor TDocument.Create;
begin
  inherited Create;

  { Creates the lists of items }
  Components := TCComponentList.Create;
  Wires := TCWireList.Create;
  TextList := TCTextList.Create;
  Polylines := TCPolylineList.Create;
  RasterImages := TCRasterImageList.Create;
  Ellipses := TCEllipseList.Create;

  { Initialization of various fields }
  Clear;
end;

destructor TDocument.Destroy;
begin
  { Cleans the memory of the lists of items }
  FreeAndNil(Components);
  FreeAndNil(Wires);
  FreeAndNil(TextList);
  Polylines.Free;
  RasterImages.Free;
  Ellipses.Free;

  FDestroying := True;

  inherited Destroy;
end;

{
  Creates a new document, by completeling clearing all data in ti
}
procedure TDocument.Clear;
begin
  inherited Clear;

  if FDestroying then Exit; // important, otherwise crashes

  { Non-Persistent information of the user interface }
  Modified := False;
  Saved := False;
  FileName := '';
  ZoomLevel := 1.0;
  PosX := 0.0;
  PosY := 0.0;
  { Persistent information of the user interface }
  CurrentTool := toolArrow;
  NewItemOrientation := coEast;
  Title := '';
  SnapToGrid := True;
  ShowGrid := True;
  { Selection fields }
  SelectedElement := nil;
  SelectedElementType := toolArrow;
  SelectionInfo := 0;
  { Document information }
  Width := INT_SHEET_DEFAULT_WIDTH;
  Height := INT_SHEET_DEFAULT_HEIGHT;

  if Components <> nil then Components.Clear;
  if Wires <> nil then Wires.Clear;
  if TextList <> nil then TextList.Clear;

  { Update the UI with the changes }
  if Assigned(UIChangeCallback) then UIChangeCallback(Self);
end;

(*procedure TDocument.LoadFromFile(AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AFileStream);
  finally
    AFileStream.Free;
  end;

  { Update fields }
  Title := ExtractFileName(AFileName);
  FileName := AFileName;
  UpdateDocumentInfo(True);
end;

procedure TDocument.LoadFromStream(AStream: TStream);
var
  ARecID, ARecVer: Byte;
  ARecSize: Word;
  AComponent: PTCComponent;
  AWire: PTCWire;
  AText: PTCText;
begin
  { First try to verify if the file is valid }
  if not ReadBOF(AStream) then raise Exception.Create('Invalid Turbo Circuit BOF');

  { clears old data }
  Clear();

  { Reads all records }
  while AStream.Position < AStream.Size do
  begin
    ARecID := AStream.ReadByte;
    ARecVer := AStream.ReadByte;
    ARecSize := AStream.ReadWord;
    
    case ARecID of
    
    TCRECORD_BOF: Exit; // Shouldn't be here

    TCRECORD_GUI_DATA:
    begin
      { Persistent information of the user interface }
//  CurrentTool := TCTool(AStream.ReadDWord);
//  NewComponentOrientation := TCComponentOrientation(AStream.ReadDWord);
    { Selection fields }
//    SelectedComponent: PTCComponent;
//    SelectedWire: PTCWire;
//    SelectedWirePart: TCWirePart;
    { Document information }
//  SheetWidth := AStream.ReadDWord;
//  SheetHeight := AStream.ReadDWord;
    end;

    TCRECORD_DOC_DATA:
    begin
    end;

    TCRECORD_COMPONENT:
    begin
      New(AComponent);
      AStream.Read(AComponent^, SizeOf(TCComponent));
    
      Components.Insert(AComponent);
    end;

    TCRECORD_WIRE:
    begin
      New(AWire);
      AStream.Read(AWire^, SizeOf(TCWire));

      Wires.Insert(AWire);
    end;

    TCRECORD_TEXT:
    begin
      New(AText);
      AStream.Read(AText^, SizeOf(TCText));

      TextList.Insert(AText);
    end;

    TCRECORD_EOF: Exit;
    
    end;
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

  { Update fields }
  Title := ExtractFileName(AFileName);
  FileName := AFileName;
  UpdateDocumentInfo(True);
end;

procedure TDocument.SaveToStream(AStream: TStream);
begin
  { First the identifier of any TurboCircuit schematics file }
  WriteBOF(AStream);

  { Persistent information of the user interface }
//  AStream.WriteDWord(LongWord(CurrentTool));
//  AStream.WriteDWord(LongWord(NewComponentOrientation));
  { Selection fields }
//    SelectedComponent: PTCComponent;
//    SelectedWire: PTCWire;
//    SelectedWirePart: TCWirePart;
  { Document information }
//  AStream.WriteDWord(SheetWidth);
//  AStream.WriteDWord(SheetHeight);

  { Stores the components }
  Components.ForEachDoWrite(AStream, WriteCOMPONENT);

  { Stores the wires }
  Wires.ForEachDoWrite(AStream, WriteWIRE);

  { Stores the text elements }
  TextList.ForEachDoWrite(AStream, WriteTEXT);

  { Stores the polylines }
  Polylines.ForEachDoWrite(AStream, WritePOLYLINE);
end;*)

function TDocument.GetDocumentPos(X, Y: Integer): TPoint;
begin
  Result.X := Round(X / INT_SHEET_GRID_SPACING);
  Result.Y := Round(Y / INT_SHEET_GRID_SPACING);
end;

// Receives control mouse coordinates
// Returns coordenates for FPVectorial
function TDocument.GetVectorialDocumentPos(X, Y: Integer): TPoint;
begin
  Result.X := Round(X / ZoomLevel);
  Result.Y := Round((Height - Y) / ZoomLevel);
end;

procedure TDocument.RotateOrientation(var AOrientation: TCComponentOrientation);
begin
  case AOrientation of
    coEast:  AOrientation := coNorth;
    coNorth: AOrientation := coWest;
    coWest:  AOrientation := coSouth;
    coSouth: AOrientation := coEast;
  end;
end;

function TDocument.GetComponentTopLeft(AComponent: PTCComponent): TPoint;
var
  ComponentWidth, ComponentHeight: Integer;
begin
  vComponentsDatabase.GoToRecByID(AComponent^.TypeID);
  ComponentWidth := vComponentsDatabase.GetWidth();
  ComponentHeight := vComponentsDatabase.GetHeight();

  case AComponent^.Orientation of

    coEast:
    begin
      Result.X := AComponent^.Pos.X;
      Result.Y := AComponent^.Pos.Y;
    end;

    coNorth:
    begin
      Result.X := AComponent^.Pos.X;
      Result.Y := AComponent^.Pos.Y - ComponentWidth;
    end;

    coWest:
    begin
      Result.X := AComponent^.Pos.X - ComponentWidth;
      Result.Y := AComponent^.Pos.Y - ComponentHeight;
    end;

    coSouth:
    begin
      Result.X := AComponent^.Pos.X - ComponentHeight;
      Result.Y := AComponent^.Pos.Y;
    end;
  end;
end;

procedure TDocument.ClearSelection;
begin
  SelectedElement := nil;
  SelectedvElement := nil;
  SelectionInfo := ELEMENT_DOES_NOT_MATCH;
end;

function TDocument.IsSelected(AType: TCTool): Boolean;
begin
  Result := IsSomethingSelected and (SelectedElementType = AType);
end;

function TDocument.IsSomethingSelected: Boolean;
begin
  Result := (SelectedElement <> nil) or (SelectedvElement <> nil);
end;

function TDocument.GetListForElement(AType: TCTool): PTCElementList;
begin
  case Atype of
    toolComponent: Result := @Components;
    toolWire: Result := @Wires;
    toolText: Result := @TextList;
    toolPolyline: Result := @Polylines;
  else
    Result := nil;
  end;
end;

function TDocument.GetSelectedComponent: PTCComponent;
begin
  Result := PTCComponent(SelectedElement);
end;

function TDocument.GetSelectedWire: PTCWire;
begin
  Result := PTCWire(SelectedElement);
end;

function TDocument.GetSelectedText: PTCText;
begin
  Result := PTCText(SelectedElement);
end;

function TDocument.GetSelectedPolyline: PTCPolyline;
begin
  Result := PTCPolyline(SelectedElement);
end;

function TDocument.GetSelectedRasterImage: PTCRasterImage;
begin
  Result := PTCRasterImage(SelectedElement);
end;

initialization

  vDocument := TDocument.Create;

finalization

  vDocument.Free;

end.

