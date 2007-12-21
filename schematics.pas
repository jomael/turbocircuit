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
    { Paint methods }
    procedure   UpdateAndRepaint;
    procedure   DrawToCanvas(ACanvas: TCanvas);
    procedure   Paint; override;
    procedure   EraseBackground(DC: HDC); override;
  end;

var
  vSchematics: TSchematics;

implementation

constructor TSchematics.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnKeyDown := HandleKeyDown;
  OnKeyPress := HandleKeyPress;

  OnMouseDown := HandleMouseDown;
  OnMouseMove := HandleMouseMove;
  OnMouseUp := HandleMouseUp;

  bmpOutput := TBitmap.Create;
  bmpOutput.Width := INT_SHEET_MAX_WIDTH;
  bmpOutput.Height := INT_SHEET_MAX_HEIGHT;

  NewComponentType := 1;
end;

destructor TSchematics.Destroy;
begin
  bmpOutput.Free;

  inherited Destroy;
end;

procedure TSchematics.HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
   VK_DELETE:
   begin
     if Assigned(vDocument.SelectedComponent) then vDocument.RemoveComponent(vDocument.SelectedComponent);
     if Assigned(vDocument.SelectedWire) then vDocument.RemoveWire(vDocument.SelectedWire);
     if vDocument.IsSomethingSelected then UpdateAndRepaint;
   end;
  end;
end;

procedure TSchematics.HandleKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
   ^R:
   begin
     vDocument.RotateNewComponentOrientation;
     UpdateAndRepaint;
   end;
  end;
end;

procedure TSchematics.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos: TPoint;
begin
  DocPos := vDocument.GetDocumentPos(X, Y);
  DragStartPos := DocPos;
  
  case vDocument.CurrentTool of

  toolArrow:
  begin
    { Clear selection }
    vDocument.ClearSelection;

    { Attempts to select a component }
    vDocument.SelectedComponent := vDocument.SearchComponent(DocPos);

    if vDocument.SelectedComponent <> nil then
    begin
      DragDropStarted := True;
      vDocument.NewComponentOrientation := vDocument.SelectedComponent^.Orientation;
      UpdateAndRepaint;
      Exit;
    end;
      
    { Attempts to select a wire }
    vDocument.SelectedWire := vDocument.SearchWire(DocPos);

    if vDocument.SelectedWire <> nil then
    begin
      DragDropStarted := True;
      UpdateAndRepaint;
      Exit;
    end;

  end;

  toolWire:
  begin
    DragDropStarted := True;

    NewWire := GetMem(SizeOf(TCWire));
      
    NewWire^.PtFrom := DocPos;
  end;

  end;
end;

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
        vDocument.MoveComponent(vDocument.SelectedComponent,
         Point(DocPos.X - DragStartPos.X, DocPos.Y - DragStartPos.Y));
      end
      else if vDocument.SelectedWire <> nil then
      begin
        vDocument.MoveWire(vDocument.SelectedWire, DocPos, vDocument.SelectedWirePart);
      end;
    end;

    UpdateAndRepaint;
  end;

  toolComponent:
  begin
    NewComponent := GetMem(SizeOf(TCComponent));
      
    NewComponent^.PosX := DocPos.X;
    NewComponent^.PosY := DocPos.Y;
    NewComponent^.TypeID := NewComponentType;
    NewComponent^.Orientation := vDocument.NewComponentOrientation;

    vDocument.InsertComponent(NewComponent);
      
    UpdateAndRepaint;
  end;

  toolWire:
  begin
    NewWire^.PtTo := DocPos;

    vDocument.InsertWire(NewWire);

    UpdateAndRepaint;
  end;

  end;
end;

procedure TSchematics.UpdateAndRepaint;
begin
  { Ask for update of the whole window }
  DrawToCanvas(bmpOutput.Canvas);

  Invalidate;
end;

procedure TSchematics.DrawToCanvas(ACanvas: TCanvas);
var
  x, y: Integer;
  NextComponent: PTCComponent;
  NextWire: PTCWire;
  TmpString: string;
  TmpWire: TCWire;
begin
  NextComponent := nil;

  { Background }
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(0, 0, Width, Height);

  { Sheet Background with dots showing the grid }
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, vDocument.SheetWidth, vDocument.SheetHeight);

  ACanvas.Brush.Color := clBlack;
  for x := 0 to (vDocument.SheetWidth div INT_SHEET_GRID_SPACING) do
   for y := 0 to (vDocument.SheetHeight div INT_SHEET_GRID_SPACING) do
    ACanvas.FillRect(x * INT_SHEET_GRID_SPACING, y * INT_SHEET_GRID_SPACING,
     x * INT_SHEET_GRID_SPACING + 1, y * INT_SHEET_GRID_SPACING + 1);

  { Components }
  NextComponent := vDocument.Components;
  
  while (NextComponent <> nil) do
  begin
    vItemsDrawer.DeltaX := NextComponent^.PosX;
    vItemsDrawer.DeltaY := NextComponent^.PosY;
    vItemsDrawer.Orientation := NextComponent^.Orientation;

    TmpString := vComponentsDatabase.GetDrawingCode(NextComponent^.TypeID);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
  
    NextComponent := NextComponent^.Next;
  end;

  { Wires }
  NextWire := vDocument.Wires;

  while (NextWire <> nil) do
  begin
    vItemsDrawer.DrawWire(ACanvas, NextWire);

    NextWire := NextWire^.Next;
  end;

  { Help to place components }
  if (vDocument.CurrentTool = toolComponent) then
  begin
    vItemsDrawer.DeltaX := MouseMoveDocPos.X;
    vItemsDrawer.DeltaY := MouseMoveDocPos.Y;
    vItemsDrawer.Orientation := vDocument.NewComponentOrientation;

    TmpString := vComponentsDatabase.GetDrawingCode(NewComponentType);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
  end;

  { Help to move components }
  if (vDocument.CurrentTool = toolArrow) and (vDocument.SelectedComponent <> nil) and DragDropStarted then
  begin
    vItemsDrawer.DeltaX := vDocument.SelectedComponent^.PosX + MouseMoveDocPos.X - DragStartPos.X;
    vItemsDrawer.DeltaY := vDocument.SelectedComponent^.PosY + MouseMoveDocPos.Y - DragStartPos.Y;
    vItemsDrawer.Orientation := vDocument.NewComponentOrientation;

    TmpString := vComponentsDatabase.GetDrawingCode(vDocument.SelectedComponent^.TypeID);
    vItemsDrawer.DrawComponentFromString(ACanvas, TmpString);
  end;


  { Help to place wires }
  if ((vDocument.CurrentTool = toolWire) and DragDropStarted) then
  begin
    NewWire^.PtTo := MouseMoveDocPos;

    vItemsDrawer.DrawWire(ACanvas, NewWire);
  end;
  
  { Help to move wires }
  if (vDocument.CurrentTool = toolArrow) and (vDocument.SelectedWire <> nil) and DragDropStarted then
  begin
    case vDocument.SelectedWirePart of
     wpPtFrom:
     begin
       TmpWire.PtFrom := MouseMoveDocPos;
       TmpWire.PtTo := vDocument.SelectedWire^.PtTo;
     end;

     wpPtTo:
     begin
       TmpWire.PtFrom := vDocument.SelectedWire^.PtFrom;
       TmpWire.PtTo := MouseMoveDocPos;
     end;
    end;
    
    vItemsDrawer.DrawWire(ACanvas, @TmpWire);
  end;

  { Component selection }
  if vDocument.SelectedComponent <> nil then
   vItemsDrawer.DrawComponentSelection(ACanvas, vDocument.SelectedComponent);

  { Wire selection }
  if vDocument.SelectedWire <> nil then
   vItemsDrawer.DrawWireSelection(ACanvas, vDocument.SelectedWire, vDocument.SelectedWirePart);
end;

{*******************************************************************
*  TSchematics.Paint ()
*
*  DESCRIPTION:    Processes Paint messages for the TDrawer control
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TSchematics.Paint;
begin
  { Copies the buffer bitmap to the canvas }
  Canvas.Draw(0, 0, bmpOutput);

  inherited Paint;
end;

procedure TSchematics.EraseBackground(DC: HDC);
begin
  // Uncomment this to enable default background erasing
  //inherited EraseBackground(DC);
end;

end.

