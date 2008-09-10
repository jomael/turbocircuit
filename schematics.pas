unit schematics;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCLType,
  document, constants, dbcomponents, drawer;

type

  { TSchematics }

  TSchematics = class(TCustomControl)
  private
    NewComponent: PTCComponent;
    NewWire: PTCWire;
    NewText: PTCText;
    MouseMoveDocPos: TPoint;
    DragDropStarted: Boolean;
    DragStartPos: TPoint;
  public
    { Fields accessible to external classes }
    bmpOutput: TBitmap;
    NewComponentType: Integer;
    OnUpdateMousePos: TMouseMoveEvent;
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
    procedure   DrawComponentPreview(ACanvas: TCanvas);
    procedure   DrawGrid(ACanvas: TCanvas);
    procedure   DrawToCanvas(ACanvas: TCanvas; ADrawGrid: Boolean);
    procedure   DrawWirePreview(ACanvas: TCanvas);
    procedure   EraseBackground(DC: HDC); override;
    procedure   Paint; override;
    procedure   UpdateAndRepaint;
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

  OnKeyDown := HandleKeyDown;
  OnKeyPress := HandleKeyPress;
  OnUTF8KeyPress := HandleUTF8KeyPress;

  OnMouseDown := HandleMouseDown;
  OnMouseMove := HandleMouseMove;
  OnMouseUp := HandleMouseUp;

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
  case Key of
   VK_DELETE:
   begin
     if Assigned(vDocument.SelectedComponent) then vDocument.Components.Remove(vDocument.SelectedComponent);
     if Assigned(vDocument.SelectedWire) then vDocument.Wires.Remove(vDocument.SelectedWire);
     if vDocument.IsSomethingSelected then UpdateAndRepaint;
   end;
  end;
end;

{@@
  Handles ASCII KeyPress events
}
procedure TSchematics.HandleKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
   ^R:
   begin
     { If a component is selected, rotate it }
     if vDocument.SelectedComponent <> nil then
     begin
       vDocument.RotateOrientation(vDocument.SelectedComponent.Orientation);
       UpdateAndRepaint;
     end
     { Otherwise rotate the new item to be added or moved }
     else
     begin
       vDocument.RotateOrientation(vDocument.NewItemOrientation);
       UpdateAndRepaint;
     end;
   end;
  end; // case
end;

{@@
  Handles MouseDown events
}
procedure TSchematics.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos: TPoint;
begin
  Self.SetFocus;

  DocPos := vDocument.GetDocumentPos(X, Y);
  DragStartPos := DocPos;
  
  case vDocument.CurrentTool of

  toolArrow:
  begin
    { Clear selection }
    vDocument.ClearSelection;

    { Attempts to select a component }
    vDocument.SelectionInfo := vDocument.Components.FindElement(DocPos, vDocument.SelectedComponent);
    
    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      DragDropStarted := True;
      vDocument.NewItemOrientation := vDocument.SelectedComponent^.Orientation;
      UpdateAndRepaint;
      Exit;
    end;
      
    { Attempts to select a wire }
    vDocument.SelectionInfo := vDocument.Wires.FindElement(DocPos, vDocument.SelectedWire);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      DragDropStarted := True;
      UpdateAndRepaint;
      Exit;
    end;

    { Attempts to select an existing text }
    vDocument.SelectionInfo := vDocument.TextList.FindElement(DocPos, vDocument.SelectedText);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      DragDropStarted := True;
      UpdateAndRepaint;
      Exit;
    end;

  end;

  toolWire:
  begin
    DragDropStarted := True;

    New(NewWire);
      
    NewWire^.Pos := DocPos;
  end;

  { Places a new text element on the document and selects it }
  toolText:
  begin
    vDocument.ClearSelection;

    New(NewText);
    FillChar(NewText^, SizeOf(TCText), #0);
    NewText^.Pos := DocPos;
  end;

  end;
end;

{@@
  Handles MouseMove events
}
procedure TSchematics.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  MouseMoveDocPos := vDocument.GetDocumentPos(X, Y);
  
  if Assigned(OnUpdateMousePos) then OnUpdateMousePos(Sender, Shift, X, Y);

  case vDocument.CurrentTool of
    { Help to move items }
    toolArrow: if DragDropStarted then UpdateAndRepaint;
    { Help to place components }
    toolComponent: UpdateAndRepaint;
    { Help to place wires }
    toolWire: if DragDropStarted then UpdateAndRepaint;
  end;
end;

{@@
  Handles MouseUp events
}
procedure TSchematics.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos: TPoint;
begin
  DocPos := vDocument.GetDocumentPos(X, Y);

  DragDropStarted := False;

  case vDocument.CurrentTool of

  toolArrow:
  begin
    { Verify if something is being moved }
    if vDocument.IsSomethingSelected then
    begin
      if vDocument.SelectedComponent <> nil then
      begin
        vDocument.Components.MoveElement(vDocument.SelectedComponent,
         Point(DocPos.X - DragStartPos.X, DocPos.Y - DragStartPos.Y));
      end
      else if vDocument.SelectedWire <> nil then
      begin
        vDocument.Wires.MoveWire(vDocument.SelectedWire, DocPos, vDocument.SelectionInfo);
      end
      else if vDocument.SelectedText <> nil then
      begin
        vDocument.TextList.MoveElement(vDocument.SelectedText,
         Point(DocPos.X - DragStartPos.X, DocPos.Y - DragStartPos.Y));
      end;
    end;

    UpdateAndRepaint;
  end;

  toolComponent:
  begin
    New(NewComponent);
      
    NewComponent^.Pos.X := DocPos.X;
    NewComponent^.Pos.Y := DocPos.Y;
    NewComponent^.TypeID := NewComponentType;
    NewComponent^.Orientation := vDocument.NewItemOrientation;

    vDocument.Components.Insert(NewComponent);
      
    UpdateAndRepaint;
  end;

  toolWire:
  begin
    NewWire^.PtTo := DocPos;

    vDocument.Wires.Insert(NewWire);

    UpdateAndRepaint;
  end;

  toolText:
  begin
    vDocument.TextList.Insert(NewText);
    vDocument.SelectedText := NewText;
  end;

  end;
end;

{@@
  Handles text input in utf-8 format
}
procedure TSchematics.HandleUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  case vDocument.CurrentTool of

  toolText:
  begin
    if vDocument.SelectedText <> nil then
    begin
      vDocument.SelectedText.Text += UTF8Key;
      UpdateAndRepaint;
    end;
  end;

  end;
end;

{@@
  Fills the background of the schematics screen
}
procedure TSchematics.DrawBackground(ACanvas: TCanvas);
begin
  { Area outside the document }
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(0, 0, Width, Height);

  { Document area }
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, vDocument.SheetWidth, vDocument.SheetHeight);
end;

{@@
  Draws all components on the document
}
procedure TSchematics.DrawComponents(ACanvas: TCanvas);
var
  NextComponent: PTCComponent;
  TmpString: string;
begin
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clBlack;

  if vDocument.Components = nil then Exit;

  NextComponent := PTCComponent(vDocument.Components.Elements);

  while (NextComponent <> nil) do
  begin
    vItemsDrawer.DeltaX := NextComponent^.Pos.X;
    vItemsDrawer.DeltaY := NextComponent^.Pos.Y;
    vItemsDrawer.Orientation := NextComponent^.Orientation;

    TmpString := vComponentsDatabase.GetDrawingCode(NextComponent^.TypeID);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);

    NextComponent := PTCComponent(NextComponent^.Next);
  end;
end;

{@@
  Draws the preview to help place and move components
}
procedure TSchematics.DrawComponentPreview(ACanvas: TCanvas);
var
  TmpString: string;
begin
  ACanvas.Pen.Color := clRed;

  { Help to place components }
  if (vDocument.CurrentTool = toolComponent) then
  begin
    vItemsDrawer.DeltaX := MouseMoveDocPos.X;
    vItemsDrawer.DeltaY := MouseMoveDocPos.Y;
    vItemsDrawer.Orientation := vDocument.NewItemOrientation;

    TmpString := vComponentsDatabase.GetDrawingCode(NewComponentType);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
  end;

  { Help to move components }
  if (vDocument.CurrentTool = toolArrow) and (vDocument.SelectedComponent <> nil) and DragDropStarted then
  begin
    vItemsDrawer.DeltaX := vDocument.SelectedComponent^.Pos.X + MouseMoveDocPos.X - DragStartPos.X;
    vItemsDrawer.DeltaY := vDocument.SelectedComponent^.Pos.Y + MouseMoveDocPos.Y - DragStartPos.Y;
    vItemsDrawer.Orientation := vDocument.NewItemOrientation;

    TmpString := vComponentsDatabase.GetDrawingCode(vDocument.SelectedComponent^.TypeID);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
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
  ACanvas.Brush.Color := clBlack;
  for x := 0 to (vDocument.SheetWidth div INT_SHEET_GRID_SPACING) do
   for y := 0 to (vDocument.SheetHeight div INT_SHEET_GRID_SPACING) do
    ACanvas.FillRect(x * INT_SHEET_GRID_SPACING, y * INT_SHEET_GRID_SPACING,
     x * INT_SHEET_GRID_SPACING + 1, y * INT_SHEET_GRID_SPACING + 1);
end;

{@@
  Draws all schmatics elements on the document
}
procedure TSchematics.DrawToCanvas(ACanvas: TCanvas; ADrawGrid: Boolean);
begin
  ACanvas.Font.Height := 12;

  { Background }
  DrawBackground(ACanvas);

  { Sheet Background dots showing the grid }
  if ADrawGrid then DrawGrid(ACanvas);
  
  { Components }
  DrawComponents(ACanvas);

  { Wires }
  ACanvas.Pen.Color := clGreen;
  vDocument.Wires.ForEachDoPaint(ACanvas, vItemsDrawer.DrawWire);
  
  { Text elements }
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := clWhite;
  vDocument.TextList.ForEachDoPaint(ACanvas, vItemsDrawer.DrawText);

  { Preview when placing/moving a component }
  DrawComponentPreview(ACanvas);
  
  { Preview when placing/moving a wire }
  DrawWirePreview(ACanvas);

  { Component selection }
  if vDocument.SelectedComponent <> nil then
   vItemsDrawer.DrawComponentSelection(ACanvas, vDocument.SelectedComponent);

  { Wire selection }
  if vDocument.SelectedWire <> nil then
   vItemsDrawer.DrawWireSelection(ACanvas, vDocument.SelectedWire, vDocument.SelectionInfo);
end;

{@@
  Draws the preview to help place and move wires
}
procedure TSchematics.DrawWirePreview(ACanvas: TCanvas);
var
  TmpWire: TCWire;
begin
  ACanvas.Pen.Color := clRed;

  { Help to place wires }
  if ((vDocument.CurrentTool = toolWire) and DragDropStarted) then
  begin
    NewWire^.PtTo := MouseMoveDocPos;

    vItemsDrawer.DrawWire(ACanvas, NewWire);
  end;

  { Help to move wires }
  if (vDocument.CurrentTool = toolArrow) and (vDocument.SelectedWire <> nil) and DragDropStarted then
  begin
    case vDocument.SelectionInfo of
     ELEMENT_START_POINT:
     begin
       TmpWire.Pos := MouseMoveDocPos;
       TmpWire.PtTo := vDocument.SelectedWire^.PtTo;
     end;

     ELEMENT_END_POINT:
     begin
       TmpWire.Pos := vDocument.SelectedWire^.Pos;
       TmpWire.PtTo := MouseMoveDocPos;
     end;
    end;

    vItemsDrawer.DrawWire(ACanvas, @TmpWire);
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
procedure TSchematics.UpdateAndRepaint;
begin
  { Ask for update of the whole window }
  DrawToCanvas(bmpOutput.Canvas, True);

  Invalidate;
end;

end.

