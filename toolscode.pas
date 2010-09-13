unit toolscode; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType,
  // TurboCircuit
  constants, schematics, document;

type

  { TToolsDelegate }

  TToolsDelegate = class(TSchematicsDelegate)
  public
    Owner: TSchematics;
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

procedure TToolsDelegate.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
   VK_DELETE:
   begin
     if (vDocument.SelectedElementType = toolComponent) then vDocument.Components.Remove(vDocument.SelectedElement);
     if (vDocument.SelectedElementType = toolWire) then vDocument.Wires.Remove(vDocument.SelectedElement);
     if vDocument.IsSomethingSelected then
     begin
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
       vDocument.RotateOrientation(PTCComponent(vDocument.SelectedElement)^.Orientation);
       vDocument.Modified := True;
       Owner.UpdateAndRepaint(nil);
     end
     { Otherwise rotate the new item to be added or moved }
     else
     begin
       vDocument.RotateOrientation(vDocument.NewItemOrientation);
       Owner.UpdateAndRepaint(nil);
     end;
   end;
  end; // case
end;

procedure TToolsDelegate.HandleMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DocPos: TPoint;
begin
  Owner.SetFocus;

  DocPos := vDocument.GetDocumentPos(X, Y);
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
      DragDropStarted := True;
      vDocument.NewItemOrientation := PTCComponent(vDocument.SelectedElement)^.Orientation;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

    { Attempts to select a wire }
    vDocument.SelectionInfo := vDocument.Wires.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      DragDropStarted := True;
      Owner.UpdateAndRepaint(nil);
      Exit;
    end;

    { Attempts to select an existing text }
    vDocument.SelectionInfo := vDocument.TextList.FindElement(DocPos, vDocument.SelectedElement);

    if vDocument.SelectionInfo <> ELEMENT_DOES_NOT_MATCH then
    begin
      DragDropStarted := True;
      Owner.UpdateAndRepaint(nil);
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
    { Help to place wires }
    toolWire: if DragDropStarted then Owner.UpdateAndRepaint(nil);
  end;
end;

procedure TToolsDelegate.HandleMouseUp(Sender: TOBject; Button: TMouseButton;
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
      if (vDocument.SelectedElementType = toolComponent) then
      begin
        vDocument.Components.MoveElement(vDocument.SelectedElement,
         Point(DocPos.X - DragStartPos.X, DocPos.Y - DragStartPos.Y));
        vDocument.Modified := True;
      end
      else if (vDocument.SelectedElementType = toolWire) then
      begin
        vDocument.Wires.MoveWire(PTCWire(vDocument.SelectedElement), DocPos, vDocument.SelectionInfo);
        vDocument.Modified := True;
      end
      else if (vDocument.SelectedElementType = toolText) then
      begin
        vDocument.TextList.MoveElement(vDocument.SelectedElement,
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

    vDocument.Modified := True;
    vDocument.UIChangeCallback(Self);
    Owner.UpdateAndRepaint(nil);
  end;

  toolWire:
  begin
    NewWire^.PtTo := DocPos;

    vDocument.Wires.Insert(NewWire);

    vDocument.Modified := True;
    vDocument.UIChangeCallback(Self);
    Owner.UpdateAndRepaint(nil);
  end;

  toolText:
  begin
    vDocument.TextList.Insert(NewText);
    vDocument.SelectedElement := NewText;
    vDocument.SelectedElementType := toolText;

    vDocument.Modified := True;
    vDocument.UIChangeCallback(Self);
    Owner.UpdateAndRepaint(nil);
  end;

  end;
end;

procedure TToolsDelegate.HandleUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  case vDocument.CurrentTool of

  toolText:
  begin
    if (vDocument.SelectedElementType = toolText) then
    begin
      PTCText(vDocument.SelectedElement)^.Text += UTF8Key;
      vDocument.Modified := True;
      vDocument.UIChangeCallback(Self);
      Owner.UpdateAndRepaint(nil);
    end;
  end;

  end;
end;

initialization

vToolsDelegate := TToolsDelegate.Create;

finalization

vToolsDelegate.Free;

end.

