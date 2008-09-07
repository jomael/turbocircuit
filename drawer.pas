{
drawer.pas

Drawer class for components, wires and other elements

Copyright (C) 2007 Felipe Monteiro de Carvalho

This file is part of Turbo Circuit.

Turbo Circuit is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Turbo Circuit is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating Turbo Circuit into proprietary programs.

AUTHORS: Felipe Monteiro de Carvalho
}
unit drawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  constants, tcutils, dbcomponents, document;

type

  { TItemsDrawer }

  TItemsDrawer = class(TObject)
  private
  public
    DeltaX, DeltaY: Integer;
    Orientation: TCComponentOrientation;
    { Component drawing methods  }
    procedure DrawComponentFromString(ACanvas: TCanvas; AString: string);
    procedure DrawComponentFromStringList(ACanvas: TCanvas; AStringList: TStrings);
    procedure DrawComponentSelection(ACanvas: TCanvas; AComponent: PTCComponent); overload;
    procedure DrawComponentSelection(ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawFromDrawingCodeLine(ACanvas: TCanvas; Cmds: T10Strings);
    { Wire drawing methods }
    procedure DrawWire(ACanvas: TCanvas; AElement: PTCElement);
    procedure DrawWireSelection(ACanvas: TCanvas; AWire: PTCWire; AWirePart: DWord);
    { Text element drawing methods }
    procedure DrawText(ACanvas: TCanvas; AElement: PTCElement);
    procedure DrawTextSelection(ACanvas: TCanvas; AText: PTCText);
    { General methods }
    function  FixCoordinates(APoint: TPoint): TPoint;
  end;

var
  vItemsDrawer: TItemsDrawer;
  
implementation

{ TItemsDrawer }

{@@
  Convenience method. Receives a multiline string
  and passes it to DrawComponentFromStringList

  @see    #DrawComponentFromStringList
}
procedure TItemsDrawer.DrawComponentFromString(ACanvas: TCanvas; AString: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := vComponentsDatabase.DBDrawingCodeToMemoString(AString);
    DrawComponentFromStringList(ACanvas, StringList);
  finally
    StringList.Free;
  end;
end;

{@@
  Draws a component given a string list with the
  instructions to draw it, one in each line.

  The format of such instructions is:

  [Command], [Param1], [Param2], etc ...

  Possible commands are:

  LINE     - Draws a line.

             (Param1, Param2) forms a (x, y) point
             indicating the inital point of the line.

             (Param3, Param4) forms a (x, y) point
             indicating the final point of the line.

  TEXT     - Draws a text.

             (Param1, Param2) forms a (left, top) point
             indicating where the text will be placed

             Param3 is the UTF-8 text to be written

             Param4 is the height of the font
}
procedure TItemsDrawer.DrawComponentFromStringList(ACanvas: TCanvas;
 AStringList: TStrings);
var
  i: Integer;
  Cmds: T10Strings;
begin
  for i := 0 to AStringList.Count - 1 do
  begin
    Cmds := SeparateString(AStringList.Strings[i], lpComma);

    DrawFromDrawingCodeLine(ACanvas, Cmds);
  end;
end;

{@@
  Draws the screen indication that a given component
  is selected
}
procedure TItemsDrawer.DrawComponentSelection(ACanvas: TCanvas;
  AComponent: PTCComponent);
var
  TargetRect: TRect;
begin
  TargetRect := Bounds(
    vDocument.GetComponentTopLeft(AComponent).X,
    vDocument.GetComponentTopLeft(AComponent).Y,
    vComponentsDatabase.GetWidth(AComponent^.TypeID),
    vComponentsDatabase.GetHeight(AComponent^.TypeID));

  TargetRect.Left := TargetRect.Left * INT_SHEET_GRID_SPACING;
  TargetRect.Top := TargetRect.Top * INT_SHEET_GRID_SPACING;
  TargetRect.Right := TargetRect.Right * INT_SHEET_GRID_SPACING;
  TargetRect.Bottom := TargetRect.Bottom * INT_SHEET_GRID_SPACING;

  DrawComponentSelection(ACanvas, TargetRect);
end;

{@@
  Draws the screen indication that a given component
  is selected
}
procedure TItemsDrawer.DrawComponentSelection(ACanvas: TCanvas; ARect: TRect);
begin
  { Upper-left corner }

  ACanvas.Line(
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Top - INT_SHEET_GRID_HALFSPACING,
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Top);

  ACanvas.Line(
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Top - INT_SHEET_GRID_HALFSPACING,
   ARect.Left,
   ARect.Top - INT_SHEET_GRID_HALFSPACING);

  { Upper-right corner }

  ACanvas.Line(
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Top - INT_SHEET_GRID_HALFSPACING,
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Top);

  ACanvas.Line(
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Top - INT_SHEET_GRID_HALFSPACING,
   ARect.Right,
   ARect.Top - INT_SHEET_GRID_HALFSPACING);

  { Lower-left corner }

  ACanvas.Line(
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING,
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom);

  ACanvas.Line(
   ARect.Left - INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING,
   ARect.Left,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING);

  { Lower-right corner }

  ACanvas.Line(
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING,
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom);

  ACanvas.Line(
   ARect.Right + INT_SHEET_GRID_HALFSPACING,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING,
   ARect.Right,
   ARect.Bottom + INT_SHEET_GRID_HALFSPACING);
end;

procedure TItemsDrawer.DrawFromDrawingCodeLine(ACanvas: TCanvas; Cmds: T10Strings);
var
  PtFrom, PtTo: TPoint;
begin
  if Cmds[0] = STR_DRAWINGCODE_LINE then
  begin
    PtFrom.X := Round((DeltaX + StrToFloat(Cmds[1])) * INT_SHEET_GRID_SPACING);
    PtFrom.Y := Round((DeltaY + StrToFloat(Cmds[2])) * INT_SHEET_GRID_SPACING);
    PtTo.X := Round((DeltaX + StrToFloat(Cmds[3])) * INT_SHEET_GRID_SPACING);
    PtTo.Y := Round((DeltaY + StrToFloat(Cmds[4])) * INT_SHEET_GRID_SPACING);

    PtFrom := FixCoordinates(PtFrom);
    PtTo := FixCoordinates(PtTo);

    ACanvas.Line(PtFrom, PtTo);
  end
  else if Cmds[0] = STR_DRAWINGCODE_TEXT then
  begin
    PtTo.X := Round((DeltaX + StrToFloat(Cmds[1])) * INT_SHEET_GRID_SPACING);
    PtTo.Y := Round((DeltaY + StrToFloat(Cmds[2])) * INT_SHEET_GRID_SPACING);

    PtTo := FixCoordinates(PtTo);

    if Cmds[4] <> '' then ACanvas.Font.Height := StrToInt(Cmds[4]);

    ACanvas.Brush.Color := clWhite;
    ACanvas.Pen.Color := clBlack;

    ACanvas.TextOut(PtTo.X, PtTo.Y, Cmds[3]);
  end;
end;

{@@
  Draws a wire
}
procedure TItemsDrawer.DrawWire(ACanvas: TCanvas; AElement: PTCElement);
var
  AWire: PTCWire;
  PtFrom, PtTo: TPoint;
begin
  AWire := PTCWire(AElement);

  PtFrom.X := AWire^.Pos.X * INT_SHEET_GRID_SPACING;
  PtFrom.Y := AWire^.Pos.Y * INT_SHEET_GRID_SPACING;
  PtTo.X := AWire^.PtTo.X * INT_SHEET_GRID_SPACING;
  PtTo.Y := AWire^.PtTo.Y * INT_SHEET_GRID_SPACING;

  ACanvas.Line(PtFrom, PtTo);
end;

{@@
  Draws the screen indication that a given wire
  is selected
}
procedure TItemsDrawer.DrawWireSelection(ACanvas: TCanvas; AWire: PTCWire;
  AWirePart: DWord);
begin
  case AWirePart of
  
  ELEMENT_START_POINT: ACanvas.Rectangle(
             AWire^.Pos.X * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.Pos.Y * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.Pos.X * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING,
             AWire^.Pos.Y * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING);
  
  ELEMENT_END_POINT:   ACanvas.Rectangle(
             AWire^.PtTo.X * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.Y * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.X * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.Y * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING);

  end;
end;

procedure TItemsDrawer.DrawText(ACanvas: TCanvas; AElement: PTCElement);
var
  AText: PTCText;
begin
  if AElement = nil then Exit;
  
  AText := PTCText(AElement);

  ACanvas.TextOut(
    AText^.Pos.X * INT_SHEET_GRID_SPACING,
    AText^.Pos.Y * INT_SHEET_GRID_SPACING,
    AText^.Text);
end;

procedure TItemsDrawer.DrawTextSelection(ACanvas: TCanvas; AText: PTCText);
begin

end;

{@@
  Given the inputs:

  APoint - A point, in pixels, which represents
  an absolute position on the schematics screen,
  with the suposition that the component has coEast
  orientation.

  (DeltaX, DeltaY) - The origin of the component in
  grid units.

  This method calculates a new absolute position on
  the schematics screen, fixed to correctly observe
  the orientation of the component.

  This method was introduced to help other methods
  which produce an absolute screen position and then
  need to adequate it to the orientation of the
  component.
}
function TItemsDrawer.FixCoordinates(APoint: TPoint): TPoint;
var
  BigDeltaX, BigDeltaY: Integer;
begin
  BigDeltaX := DeltaX * INT_SHEET_GRID_SPACING;
  BigDeltaY := DeltaY * INT_SHEET_GRID_SPACING;

  case Orientation of

    coEast:  Result := APoint;

    coNorth:
    begin
      Result.X := BigDeltaX + (APoint.Y - BigDeltaY);
      Result.Y := BigDeltaY - (APoint.X - BigDeltaX);
    end;

    coWest:
    begin
      Result.X := BigDeltaX - (APoint.X - BigDeltaX);
      Result.Y := BigDeltaY - (APoint.Y - BigDeltaY);
    end;

    coSouth:
    begin
      Result.X := BigDeltaX - (APoint.Y - BigDeltaY);
      Result.Y := BigDeltaY + (APoint.X - BigDeltaX);
    end;
  end;
end;

initialization

  vItemsDrawer := TItemsDrawer.Create;

finalization

  vItemsDrawer.Free;

end.

