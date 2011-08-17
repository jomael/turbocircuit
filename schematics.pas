{

}
unit schematics;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCLType,
  document, constants, dbcomponents, drawer,
  // fpvectorial
  fpvtocanvas, fpvutils
  ;

type
  TSchematicsDelegate = class
  public
    { Fields to be used by both the delegate and the schematics class }
    NewComponentType: TCDataString;
    OnUpdateMousePos: TMouseMoveEvent;
    NewComponent: PTCComponent;
    NewWire: PTCWire;
    NewText: PTCText;
    NewPolyline: PTCPolyline;
    NewRasterImage: PTCRasterImage;
    NewEllipse: PTCEllipse;
    //
    MouseMoveDocPos: TPoint;
    MulticlickPlacementStarted: Boolean;
    DragDropStarted: Boolean;
    DragStartPos: TPoint;
    procedure   HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure   HandleKeyPress(Sender: TObject; var Key: char); virtual; abstract;
    procedure   HandleMouseDown(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure   HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure   HandleMouseUp(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure   HandleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char); virtual; abstract;
  end;

{ TSchematics }

  TSchematics = class(TCustomControl)
  private
    // Temporary data while drawing
    ADestX, ADestY: Integer;
    AMulX, AMulY: Double;
  public
    { Fields accessible to external classes }
    bmpOutput: TBitmap;
    Delegate: TSchematicsDelegate;
    { Base methods }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { Event handling methods }
    procedure   HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   HandleKeyPress(Sender: TObject; var Key: char);
    procedure   HandleMouseDown(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure   HandleMouseUp(Sender: TOBject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   HandleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    { Paint methods }
    procedure   DrawBackground(ACanvas: TCanvas);
    procedure   DrawComponents(ACanvas: TCanvas);
    procedure   DrawGrid(ACanvas: TCanvas);
    procedure   DrawPolylinePreview(ACanvas: TCanvas);
    procedure   DrawToCanvas(ACanvas: TCanvas; AEditMode: Boolean);
    procedure   EraseBackground(DC: HDC); override;
    procedure   Paint; override;
    procedure   UpdateAndRepaint(Sender: TObject);
  end;

var
  vSchematics: TSchematics;

implementation

{@@
  Allocates an instance of the TScrematics class
}
constructor TSchematics.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Set form events }

  OnKeyDown := HandleKeyDown;
  OnKeyPress := HandleKeyPress;
  OnUTF8KeyPress := HandleUTF8KeyPress;
  OnResize := UpdateAndRepaint; // Necessary, otherwise when resizing not everything is painted
  OnMouseDown := HandleMouseDown;
  OnMouseMove := HandleMouseMove;
  OnMouseUp := HandleMouseUp;

  { Create the output bitmap }
  bmpOutput := TBitmap.Create;
  bmpOutput.Width := INT_SHEET_MAX_WIDTH;
  bmpOutput.Height := INT_SHEET_MAX_HEIGHT;

  { Properties of the control }
  TabStop := True;
end;

{@@
  Releases an instance of the TSchematics class
}
destructor TSchematics.Destroy;
begin
  bmpOutput.Free;

  inherited Destroy;
end;

{@@
  Handles KeyDown events
}
procedure TSchematics.HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(Delegate) then Delegate.HandleKeyDown(Sender, Key, Shift);
end;

{@@
  Handles ASCII KeyPress events
}
procedure TSchematics.HandleKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(Delegate) then Delegate.HandleKeyPress(Sender, Key);
end;

{@@
  Handles MouseDown events
}
procedure TSchematics.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Delegate) then Delegate.HandleMouseDown(Sender, Button, Shift, X, Y);
end;

{@@
  Handles MouseMove events
}
procedure TSchematics.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Delegate) then Delegate.HandleMouseMove(Sender, Shift, X, Y);
end;

{@@
  Handles MouseUp events
}
procedure TSchematics.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Delegate) then Delegate.HandleMouseUp(Sender, Button, Shift, X, Y);
end;

{@@
  Handles text input in utf-8 format
}
procedure TSchematics.HandleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Assigned(Delegate) then Delegate.HandleUTF8KeyPress(Sender, UTF8Key);
end;

{@@
  Fills the background of the schematics screen
}
procedure TSchematics.DrawBackground(ACanvas: TCanvas);
var
  J, X1, Y1, X2, Y2: Integer;

  // Special version for the background
  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ACoord * vDocument.ZoomLevel);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ACoord * vDocument.ZoomLevel);
  end;

begin
  { Area outside the document }
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, Width, Height);

  { Document area delimiter, which has the following size:
   0, 0, vDocument.SheetWidth, vDocument.SheetHeight }
  ACanvas.Pen.Color := RGBToColor(50, 50, 50);
  J := 1;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(127, 127, 127);
  J := 2;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(151, 151, 151);
  J := 3;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(181, 181, 181);
  J := 4;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(206, 206, 206);
  J := 5;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(226, 226, 226);
  J := 6;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);

  ACanvas.Pen.Color := RGBToColor(240, 240, 240);
  J := 7;
  ACanvas.Line(CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(0), CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J + 1);
  ACanvas.Line(CoordToCanvasX(0), CoordToCanvasY(vDocument.Height) + J, CoordToCanvasX(vDocument.Width) + J, CoordToCanvasY(vDocument.Height) + J);
end;

{@@
  Draws all components on the document
}
procedure TSchematics.DrawComponents(ACanvas: TCanvas);
var
  NextComponent: PTCComponent;
  TmpString: string;
begin
  if vDocument.Components = nil then Exit;

  {.$ifdef DEBUG}
  vDocument.Components.WriteDebugInfo();
  {.$endif}

  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clBlack;

  NextComponent := PTCComponent(vDocument.Components.Elements);

  while (NextComponent <> nil) do
  begin
    vItemsDrawer.DeltaX := NextComponent^.Pos.X;
    vItemsDrawer.DeltaY := NextComponent^.Pos.Y;
    vItemsDrawer.Orientation := NextComponent^.Orientation;

    vComponentsDatabase.GoToRecByID(NextComponent^.TypeID);
    TmpString := vComponentsDatabase.GetDrawingCode();
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);

    NextComponent := PTCComponent(NextComponent^.Next);
  end;
end;

{@@
  Draws the default grid of the schematics
}
procedure TSchematics.DrawGrid(ACanvas: TCanvas);
var
  x, y: Integer;
  OldColor: TColor;
begin
  // Don't draw the grid if it is too small
  if INT_SHEET_GRID_SPACING <= 1 then Exit;

  ACanvas.Brush.Color := clBlack;
  for x := 0 to Round(vDocument.Width / INT_SHEET_GRID_SPACING / vDocument.ZoomLevel) do
   for y := 0 to Round(vDocument.Height / INT_SHEET_GRID_SPACING / vDocument.ZoomLevel) do
    ACanvas.FillRect(
      Round(x * INT_SHEET_GRID_SPACING * vDocument.ZoomLevel),
      Round(y * INT_SHEET_GRID_SPACING * vDocument.ZoomLevel),
      Round(x * INT_SHEET_GRID_SPACING * vDocument.ZoomLevel + 1),
      Round(y * INT_SHEET_GRID_SPACING * vDocument.ZoomLevel + 1));
end;

procedure TSchematics.DrawPolylinePreview(ACanvas: TCanvas);
var
  LocalPolyline: TCPolyline;
begin
//  ACanvas.Pen.Color := clRed;

  { Help to place polyline }
  if ((vDocument.CurrentTool = toolPolyline) and Delegate.MulticlickPlacementStarted) then
  begin
    // It's simples to copy the whole polyline and modify it
    // then to temporarely add the extra point for the
    // current mouse position and later remove it
    Move(Delegate.NewPolyline^, LocalPolyline, SizeOf(TCPolyline));
    vDocument.Polylines.AddPoint(@LocalPolyline, Delegate.MouseMoveDocPos);
    vItemsDrawer.DrawPolyline(ACanvas, @LocalPolyline);
  end;
end;

{@@
  Draws all schmatics elements on the document
}
procedure TSchematics.DrawToCanvas(ACanvas: TCanvas; AEditMode: Boolean);
var
  TmpString: string;
  TmpWire: TCWire;
  TmpText: TCText;
  TmpPolyline: TCPolyline;
begin
  ADestX := Round(vDocument.PosX * vDocument.ZoomLevel);
  ADestY := Round(ACanvas.Height + vDocument.PosY * vDocument.ZoomLevel);
  AMulX := vDocument.ZoomLevel;
  AMulY := -1 * vDocument.ZoomLevel;

  ACanvas.Font.Height := 12;

  { Background }
  DrawBackground(ACanvas);

  { Sheet Background dots showing the grid }
  if AEditMode and vDocument.ShowGrid then DrawGrid(ACanvas);

  { FPVectorial }
  DrawFPVectorialToCanvas(vDocument, ACanvas, 0, ACanvas.Height, 1, -1);

  { RasterImages }
  vDocument.RasterImages.ForEachDoPaint(ACanvas, vItemsDrawer.DrawRasterImage);

  { Components }
  DrawComponents(ACanvas);

  { Wires }
  ACanvas.Pen.Color := clGreen;
  vDocument.Wires.ForEachDoPaint(ACanvas, vItemsDrawer.DrawWire);
  
  { Text elements }
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := clWhite;
  vDocument.TextList.ForEachDoPaint(ACanvas, vItemsDrawer.DrawText);

  { Polylines }
  vDocument.Polylines.ForEachDoPaint(ACanvas, vItemsDrawer.DrawPolyline);

  { Raster Images }
  vDocument.RasterImages.ForEachDoPaint(ACanvas, vItemsDrawer.DrawRasterImage);

  { Ellipses }
  vDocument.Ellipses.ForEachDoPaint(ACanvas, vItemsDrawer.DrawEllipse);

  // Extra visual effects in edit mode
  if AEditMode then
  begin
    { Preview when placing an element }
    ACanvas.Pen.Color := clRed;

    case vDocument.CurrentTool of
    toolComponent:
    begin
      vItemsDrawer.DeltaX := Delegate.MouseMoveDocPos.X;
      vItemsDrawer.DeltaY := Delegate.MouseMoveDocPos.Y;
      vItemsDrawer.Orientation := vDocument.NewItemOrientation;

      vComponentsDatabase.GoToRecByID(Delegate.NewComponentType);
      TmpString := vComponentsDatabase.GetDrawingCode();
      vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
    end;
    toolWire:
    begin
      if (Delegate.DragDropStarted) then
      begin
        Delegate.NewWire^.PtTo := Delegate.MouseMoveDocPos;

        vItemsDrawer.DrawWire(ACanvas, Delegate.NewWire);
      end;
    end;
    toolPolyline:
    begin
      if Delegate.MulticlickPlacementStarted then
      begin
        TmpPolyline := Delegate.NewPolyline^;
        vDocument.Polylines.AddPoint(@TmpPolyline, Delegate.MouseMoveDocPos);
        vItemsDrawer.DrawPolyline(ACanvas, @TmpPolyline);
      end;
    end;
    end;

    { Preview when moving an element }
    if (vDocument.CurrentTool = toolArrow) then
    begin
    case vDocument.SelectedElementType of
    toolComponent:
    begin
      if Delegate.DragDropStarted then
      begin
        vItemsDrawer.DeltaX := vDocument.GetSelectedComponent^.Pos.X + Delegate.MouseMoveDocPos.X - Delegate.DragStartPos.X;
        vItemsDrawer.DeltaY := vDocument.GetSelectedComponent^.Pos.Y + Delegate.MouseMoveDocPos.Y - Delegate.DragStartPos.Y;
        vItemsDrawer.Orientation := vDocument.NewItemOrientation;

        vComponentsDatabase.GoToRecByID(vDocument.GetSelectedComponent^.TypeID);
        TmpString := vComponentsDatabase.GetDrawingCode();
        vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
      end;
    end;
    toolWire:
    begin
      if Delegate.DragDropStarted then
      begin
        case vDocument.SelectionInfo of
         ELEMENT_START_POINT:
         begin
           TmpWire.Pos := Delegate.MouseMoveDocPos;
           TmpWire.PtTo := vDocument.GetSelectedWire^.PtTo;
         end;

         ELEMENT_END_POINT:
         begin
           TmpWire.Pos := vDocument.GetSelectedWire^.Pos;
           TmpWire.PtTo := Delegate.MouseMoveDocPos;
         end;
        end;

        vItemsDrawer.DrawWire(ACanvas, @TmpWire);
      end;
    end;
    toolText:
    begin
      TmpText := vDocument.GetSelectedText^;
      TmpText.Pos := Delegate.MouseMoveDocPos;
      vItemsDrawer.DrawText(ACanvas, @TmpText);
    end;
    end;// case
    end;// if

    { element selection }
    ACanvas.Brush.Color := clWhite;
    ACanvas.Brush.Style := bsSolid;
    case vDocument.SelectedElementType of
     toolComponent: vItemsDrawer.DrawComponentSelection(ACanvas, vDocument.GetSelectedComponent);
     toolWire: vItemsDrawer.DrawWireSelection(ACanvas, vDocument.GetSelectedWire, vDocument.SelectionInfo);
     toolText: vItemsDrawer.DrawTextSelection(ACanvas, vDocument.GetSelectedText);
    end;
  end;
end;

{@@
  Handles the EraseBackground message by doing nothing

  This prevents flickering on Windows
}
procedure TSchematics.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

{@@
  Processes Paint messages for the TDrawer control
}
procedure TSchematics.Paint;
begin
  { Copies the buffer bitmap to the canvas }
  Canvas.Draw(0, 0, bmpOutput);

  inherited Paint;
end;

{@@
  Updates the schematics screen with the current data
  and repaints it
}
procedure TSchematics.UpdateAndRepaint(Sender: TObject);
begin
  { Ask for update of the whole window }
  DrawToCanvas(bmpOutput.Canvas, True);

  Invalidate;
end;

end.

