unit toolscode; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType,
  // TurboCircuit
  constants, schematics, document, tclists,
  // fpvectorial
  fpvectorial, fpvelectricalelements;

type

  { TToolsDelegate }

  TToolsDelegate = class(TSchematicsDelegate)
  public
    Owner: TSchematics;
    constructor Create;
    destructor Destroy; override;
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure HandleKeyPress(Sender: TObject; var Key: char); override;
    procedure HandleMouseDown(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure HandleMouseUp(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure HandleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char); override;
  end;

var
  vToolsDelegate: TToolsDelegate;

implementation

{ TToolsDelegate }

constructor TToolsDelegate.Create;
begin
  inherited Create;
end;

destructor TToolsDelegate.Destroy;
begin
  inherited Destroy;
end;

procedure TToolsDelegate.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lList: PTCElementList;
begin
  case Key of
   VK_DELETE:
   begin
     if vDocument.IsSomethingSelected then
     begin
       lList := vDocument.GetListForElement(vDocument.SelectedElementType);
       lList^.Remove(vDocument.SelectedElement);

       // It is fundamental to clear the selection,
       // because otherwise the drawing code will try
       // to draw the selected component, which no longer exists
       vDocument.ClearSelection;

       // Also mark the modified flag
       vDocument.Modified := True;

       Owner.UpdateAndRepaint(nil);
     end;
   end;
  end;
end;

procedure TToolsDelegate.HandleKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
   ^R:
   begin
     { If a component is selected, rotate it }
     if (vDocument.SelectedElementType = toolComponent) then
     begin
       vDocument.RotateOrientation(vDocument.GetSelectedComponent()^.Orientation);
       vDocument.Modified := True;
       Owner.UpdateAndRepaint(nil);
     end
     { If a component is selected to be added, then rotate it }
     else
     begin
       if (vDocument.CurrentTool in [toolComponent, toolRasterImage]) then
       begin
         vDocument.RotateOrientation(vDocument.NewItemOrientation);
         Owner.UpdateAndRepaint(nil);
       end;
     end;
   end;
  end; // case
end;

procedure TToolsDelegate.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos, FPVDocPos: TPoint;
  segment: T2DSegment;
begin
  Owner.SetFocus;

  DocPos := vDocument.GetDocumentPos(X, Y);
  FPVDocPos := vDocument.GetVectorialDocumentPos(X, Y);
  DragStartPos := DocPos;

  case vDocument.CurrentTool of

  toolArrow:
  begin
    { Clear selection }
    vDocument.ClearSelection;

    { Attempts to select a component }
    vDocument.SelectionInfo := vDocument.Components.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      vDocument.SelectedElementType := toolComponent;
      DragDropStarted := True;
      vDocument.NewItemOrientation := PTCComponent(vDocument.SelectedElement)^.Orientation;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

    { Attempts to select a wire }
    vDocument.SelectionInfo := vDocument.Wires.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      vDocument.SelectedElementType := toolWire;
      DragDropStarted := True;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

    { Attempts to select a fpvectorial element }
    vDocument.SelectionvInfo := vDocument.GetPage(0).FindAndSelectEntity(FPVDocPos);

    if vDocument.SelectionvInfo <> vfrNotFound then
    begin
      //vDocument.SelectedElementType := toolText;
      DragDropStarted := True;
      DragStartPos := FPVDocPos;// remove when all elements are based on fpvectorial
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

    { Attempts to select a polyline }
    vDocument.SelectionInfo := vDocument.Polylines.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      vDocument.SelectedElementType := toolPolyline;
      DragDropStarted := True;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

     { Attempts to select a raster image }
    vDocument.SelectionInfo := vDocument.RasterImages.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      vDocument.SelectedElementType := toolRasterImage;
      DragDropStarted := True;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;
  end;

  toolWire:
  begin
    DragDropStarted := True;

    NewWire := TvWire.Create;
    NewWire.X := FPVDocPos.X;
    NewWire.Y := FPVDocPos.Y;
  end;

  { Places a new text element on the document and selects it }
{  toolText:
  begin
    vDocument.ClearSelection;

    New(NewText);
    FillChar(NewText^, SizeOf(TCText), #0);
    NewText^.Pos := DocPos;
  end;    }

  toolPolyline:
  begin
    DragDropStarted := True;

    New(NewPolyline);

    NewPolyline^.NPoints := 1;
    NewPolyline^.Width := 1;
//    NewPolyline^.Color := clBlack;
//    PenStyle: TPenStyle;
//    PenEndCap: TPenEndCap;
//    PenJoinStyle: TPenJoinStyle;
    NewPolyline^.Points[0] := DocPos;
  end;

  toolEllipse:
  begin
    DragDropStarted := True;
    NewEllipse := TvEllipse.Create;
    NewEllipse.X := FPVDocPos.X;
    NewEllipse.Y := FPVDocPos.Y;
  end;

  toolRectangle:
  begin
    DragDropStarted := True;

    NewRectangle := TPath.Create;
    segment := T2DSegment.Create;
    segment.SegmentType := stMoveTo;
    segment.X := FPVDocPos.X;
    segment.Y := FPVDocPos.Y;
    NewRectangle.AppendSegment(segment);
  end;

  end;
end;

procedure TToolsDelegate.HandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseMoveDocPos := vDocument.GetDocumentPos(X, Y);

  if Assigned(OnUpdateMousePos) then OnUpdateMousePos(Sender, Shift, X, Y);

  case vDocument.CurrentTool of
    { Help to move items }
    toolArrow: if DragDropStarted then Owner.UpdateAndRepaint(nil);
    { Help to place components }
    toolComponent: Owner.UpdateAndRepaint(nil);
    { Help to place line elements }
    toolWire, toolPolyline: if DragDropStarted then Owner.UpdateAndRepaint(nil);
  end;
end;

procedure TToolsDelegate.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos, FPVDocPos: TPoint;
  lList: PTCElementList;
  lPolyline: PTCPolyline;
  lChanged: Boolean;
  segment: T2DSegment;
  StartX: Double;
  StartY: Double;
  lEntityIndex: Integer;
begin
  DocPos := vDocument.GetDocumentPos(X, Y);
  FPVDocPos := vDocument.GetVectorialDocumentPos(X, Y);

  DragDropStarted := False;
  lChanged := False;

  case vDocument.CurrentTool of

  toolArrow:
  begin
    { Verify if something is being moved }
    if vDocument.SelectedvElement <> nil then
    begin
      vDocument.SelectedvElement.Move(FPVDocPos.X - DragStartPos.X, FPVDocPos.Y - DragStartPos.Y);
    end
    else if vDocument.IsSomethingSelected then
    begin
      lList := vDocument.GetListForElement(vDocument.SelectedElementType);
      if lList <> nil then
      begin
        lList^.MoveElement(vDocument.SelectedElement,
         Point(DocPos.X - DragStartPos.X, DocPos.Y - DragStartPos.Y));
        vDocument.Modified := True;
      end;

      vDocument.UIChangeCallback(Self);
    end;

    Owner.UpdateAndRepaint(nil);
  end;

  toolComponent:
  begin
    New(NewComponent);

    NewComponent^.Pos.X := DocPos.X;
    NewComponent^.Pos.Y := DocPos.Y;
    NewComponent^.TypeID := NewComponentType;
    NewComponent^.Orientation := vDocument.NewItemOrientation;

    vDocument.Components.Insert(NewComponent);

    lChanged := True;
  end;

  toolWire:
  begin
    NewWire.DestPos.X := FPVDocPos.X;
    NewWire.DestPos.Y := FPVDocPos.Y;

    vDocument.GetPage(0).AddEntity(NewWire);

    lChanged := True;
  end;

  toolText:
  begin
    NewText := TvText.Create;
    NewText.X := FPVDocPos.X;
    NewText.Y := FPVDocPos.Y;
    lEntityIndex := vDocument.GetPage(0).AddEntity(NewText);
    vDocument.SelectedvElement := NewText;
    if Assigned(OnUpdateAppInterface) then OnUpdateAppInterface(Self);

    lChanged := True;
  end;

  toolPolyline:
  begin
    // Placing the start of a polyline
    if DragDropStarted and (NewPolyline^.NPoints = 1) then
    begin
      // Add the new point
      NewPolyline^.Points[NewPolyline^.NPoints] := DocPos;
      NewPolyline^.NPoints += 1;
      vDocument.Polylines.Insert(NewPolyline);

      // Select the polyline
      vDocument.SelectedElementType := toolPolyline;
      vDocument.SelectedElement := NewPolyline;

      lChanged := True;
    end
    // Placing more lines to a polyline
    else if (vDocument.SelectedElementType = toolPolyline) then
    begin
      lPolyline := vDocument.GetSelectedPolyline();
      lPolyline^.Points[lPolyline^.NPoints] := DocPos;
      lPolyline^.NPoints := lPolyline^.NPoints + 1;

      lChanged := True;
    end;
  end;

  toolEllipse:
  begin
    // NewEllipse here comes with the starting X,Y positions stored,
    // with that we need to build its real informations
    NewEllipse.HorzHalfAxis := Abs(NewEllipse.X - FPVDocPos.X) / 2;
    NewEllipse.VertHalfAxis := Abs(NewEllipse.Y - FPVDocPos.Y) / 2;
    if NewEllipse.X > FPVDocPos.X then NewEllipse.X := NewEllipse.X - NewEllipse.HorzHalfAxis
    else NewEllipse.X := NewEllipse.X + NewEllipse.HorzHalfAxis;
    if NewEllipse.Y > FPVDocPos.Y then NewEllipse.Y := NewEllipse.Y - NewEllipse.VertHalfAxis
    else NewEllipse.Y := NewEllipse.Y + NewEllipse.VertHalfAxis;

    vDocument.GetPage(0).AddEntity(NewEllipse);
    lChanged := True;
  end;

  toolRectangle:
  begin
    StartX := T2DSegment(NewRectangle.Points).X;
    StartY := T2DSegment(NewRectangle.Points).Y;
    //
    segment := T2DSegment.Create;
    segment.SegmentType := st2DLine;
    segment.X := StartX;
    segment.Y := FPVDocPos.Y;
    NewRectangle.AppendSegment(segment);
    //
    segment := T2DSegment.Create;
    segment.SegmentType := st2DLine;
    segment.X := FPVDocPos.X;
    segment.Y := FPVDocPos.Y;
    NewRectangle.AppendSegment(segment);
    //
    segment := T2DSegment.Create;
    segment.SegmentType := st2DLine;
    segment.X := FPVDocPos.X;
    segment.Y := StartY;
    NewRectangle.AppendSegment(segment);
    //
    segment := T2DSegment.Create;
    segment.SegmentType := st2DLine;
    segment.X := StartX;
    segment.Y := StartY;
    NewRectangle.AppendSegment(segment);

    vDocument.GetPage(0).AddEntity(NewRectangle);

    lChanged := True;
  end;

  end;

  if lChanged then
  begin
    vDocument.Modified := True;
    vDocument.UIChangeCallback(Self);
    Owner.UpdateAndRepaint(nil);
  end;
end;

procedure TToolsDelegate.HandleUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  lText: PTCText;
begin
{  case vDocument.CurrentTool of

  toolText:
  begin
    if (vDocument.SelectedElementType = toolText) then
    begin
      lText := vDocument.GetSelectedText();
      lText^.Text += UTF8Key;
      vDocument.Modified := True;
      vDocument.UIChangeCallback(Self);
      Owner.UpdateAndRepaint(nil);
    end;
  end;

  end;    }
end;

initialization

vToolsDelegate := TToolsDelegate.Create;

finalization

vToolsDelegate.Free;

end.
