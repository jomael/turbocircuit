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
    Points: array[0..2] of TPoint;
    procedure DrawPosCross(ACanvas: TCanvas; APos: TPoint);
  public
    DeltaX, DeltaY: Integer;
    Orientation: TCComponentOrientation;
    IC_Width, IC_Pins, IC_Half_Pins: Cardinal;
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
    procedure DrawTextSelection(ACanvas: TCanvas; AElement: PTCElement);
    { Polyline drawing methods }
    procedure DrawPolyline(ACanvas: TCanvas; AElement: PTCElement);
    procedure DrawPolylineSelection(ACanvas: TCanvas; AElement: PTCElement; APointNr: Integer);
    { Raster Image drawing methods }
    procedure DrawRasterImage(ACanvas: TCanvas; AElement: PTCElement);
    { Ellipse drawing methods }
    procedure DrawEllipse(ACanvas: TCanvas; AElement: PTCElement);
    { General methods }
    function  FixCoordinates(APoint: TPoint): TPoint;
    function  GridCoordsToSheet(AX, AY: Single): TPoint;
  end;

var
  vItemsDrawer: TItemsDrawer;
  
implementation

{ TItemsDrawer }

{@@
  Convenience method. Receives a multiline string
  and passes it to DrawComponentFromStringList

  @param AString The string containing the drawing instructions.
                 It should be in the internal database format, i.e. with #
                 instead of new lines.

  @see    #DrawComponentFromStringList
}
procedure TItemsDrawer.DrawComponentFromString(ACanvas: TCanvas; AString: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := TComponentsDatabase.DBDrawingCodeToMemoString(AString);
    DrawComponentFromStringList(ACanvas, StringList);
  finally
    StringList.Free;
  end;
end;

{@@
  Draws a component given a string list with the
  instructions to draw it, one in each line.
}
procedure TItemsDrawer.DrawComponentFromStringList(ACanvas: TCanvas;
 AStringList: TStrings);
var
  i: Integer;
  Cmds: T10Strings;
begin
  for i := 0 to AStringList.Count - 1 do
  begin
    Cmds := SeparateString(AStringList.Strings[i], lpSpace);

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
  Orientation := AComponent^.Orientation;

  vComponentsDatabase.GoToRecByID(AComponent^.TypeID);
  TargetRect := Bounds(
    vDocument.GetComponentTopLeft(AComponent).X,
    vDocument.GetComponentTopLeft(AComponent).Y,
    vComponentsDatabase.GetWidth(),
    vComponentsDatabase.GetHeight());

  TargetRect.Left := TargetRect.Left * INT_SHEET_GRID_SPACING;
  TargetRect.Top := TargetRect.Top * INT_SHEET_GRID_SPACING;
  TargetRect.Right := TargetRect.Right * INT_SHEET_GRID_SPACING;
  TargetRect.Bottom := TargetRect.Bottom * INT_SHEET_GRID_SPACING;

  FixCoordinates(TargetRect.BottomRight);

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

{@@
  Draws one line of instruction for drawing a component

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

  ARC      - Draws an arc

             (Param1, Param2) forms the (left, top) position of the arc

             (Param3, Param4) forms the (right, bottom) position of the arc

             Param5 is the UTF-8 text to be written

             Param6 is the height of the font

  TRIANGLE - Draws a triangle


  IC       - Draws an integrated circuit


  IC_TEXT_LEFT_1 - Draws the texts for the pins in an IC


  IC_TEXT_LEFT_2 -


  IC_TEXT_RIGHT_1 -


  IC_TEXT_RIGHT_2 -

}
procedure TItemsDrawer.DrawFromDrawingCodeLine(ACanvas: TCanvas; Cmds: T10Strings);
var
  i: Integer;

  procedure GetPoint_1;
  begin
    Points[0].X := Round((DeltaX + StrToFloat(Cmds[1])) * INT_SHEET_GRID_SPACING);
    Points[0].Y := Round((DeltaY + StrToFloat(Cmds[2])) * INT_SHEET_GRID_SPACING);

    Points[0] := FixCoordinates(Points[0]);
  end;

  procedure GetPoint_2;
  begin
    Points[1].X := Round((DeltaX + StrToFloat(Cmds[3])) * INT_SHEET_GRID_SPACING);
    Points[1].Y := Round((DeltaY + StrToFloat(Cmds[4])) * INT_SHEET_GRID_SPACING);

    Points[1] := FixCoordinates(Points[1]);
  end;

  procedure GetPoint_3;
  begin
    Points[2].X := Round((DeltaX + StrToFloat(Cmds[5])) * INT_SHEET_GRID_SPACING);
    Points[2].Y := Round((DeltaY + StrToFloat(Cmds[6])) * INT_SHEET_GRID_SPACING);

    Points[2] := FixCoordinates(Points[2]);
  end;

begin
  try
    if Cmds[0] = STR_DRAWINGCODE_LINE then
    begin
      GetPoint_1;
      GetPoint_2;

      ACanvas.Line(Points[0], Points[1]);
    end
    else if Cmds[0] = STR_DRAWINGCODE_TEXT then
    begin
      GetPoint_1;

      if Cmds[4] <> '' then ACanvas.Font.Height := StrToInt(Cmds[4]);

      ACanvas.Brush.Color := clWhite;
      ACanvas.Pen.Color := clBlack;

      ACanvas.TextOut(Points[0].X, Points[0].Y, Cmds[3]);
    end
    else if Cmds[0] = STR_DRAWINGCODE_ARC then
    begin
      GetPoint_1;
      GetPoint_2;

      ACanvas.Arc(
        Points[0].X, Points[0].Y,
        Points[1].X, Points[1].Y,
        StrToInt(Cmds[5]), StrToInt(Cmds[6]));
    end
    else if Cmds[0] = STR_DRAWINGCODE_TRIANGLE then
    begin
      GetPoint_1;
      GetPoint_2;
      GetPoint_3;

      ACanvas.Polygon(Points);
    end
    else if Cmds[0] = STR_DRAWINGCODE_IC then
    begin
      { Get Width and number of pins }
      IC_Width := StrToInt(Cmds[1]);
      IC_Pins := StrToInt(Cmds[2]);
      IC_Half_Pins := IC_Pins div 2;
      Points[0] := GridCoordsToSheet(IC_Width + 1, IC_Half_Pins + 1);
      Points[1] := GridCoordsToSheet(1, 0);

      { IC Body }
      ACanvas.Rectangle(Points[1].X, Points[1].Y, Points[0].X + 1, Points[0].Y + 1);

      { Pins on the left }
      for i := 0 to IC_Half_Pins - 1 do
        ACanvas.Line(GridCoordsToSheet(0, 1 + i), GridCoordsToSheet(1, 1 + i));

      { Pins on the right }
      for i := 0 to IC_Half_Pins - 1 do
        ACanvas.Line(GridCoordsToSheet(IC_Width + 1, 1 + i), GridCoordsToSheet(IC_Width + 2, 1 + i));

      { IC Name on the top}
      Points[2] := GridCoordsToSheet(1, 0);
      ACanvas.TextOut(Points[2].X, Points[2].Y - ACanvas.Font.Height, Cmds[3]);
    end
    else if Cmds[0] = STR_DRAWINGCODE_IC_TEXT_LEFT_1 then
    begin
      for i := 0 to 7 do
       if Cmds[i + 1] <> '' then
       begin
         Points[0] := GridCoordsToSheet(1, i);
         ACanvas.TextOut(Points[0].X + 1, Points[0].Y, Cmds[i + 1]);
       end;
    end
    else if Cmds[0] = STR_DRAWINGCODE_IC_TEXT_LEFT_2 then
    begin
      for i := 0 to 7 do
       if Cmds[i + 1] <> '' then
       begin
         Points[0] := GridCoordsToSheet(1, i + 8);
         ACanvas.TextOut(Points[0].X, Points[0].Y, Cmds[i + 1]);
       end;
    end
    else if Cmds[0] = STR_DRAWINGCODE_IC_TEXT_RIGHT_1 then
    begin
      for i := 0 to 7 do
       if Cmds[i + 1] <> '' then
       begin
         Points[0] := GridCoordsToSheet(1 + IC_Width - 2, i);
         ACanvas.TextOut(Points[0].X, Points[0].Y, Cmds[i + 1]);
       end;
    end
    else if Cmds[0] = STR_DRAWINGCODE_IC_TEXT_RIGHT_2 then
    begin
      for i := 0 to 7 do
       if Cmds[i + 1] <> '' then
       begin
         Points[0] := GridCoordsToSheet(1 + IC_Width - 2, i + 8);
         ACanvas.TextOut(Points[0].X, Points[0].Y, Cmds[i + 1]);
       end;
    end;
  except
    // Exit silently in floating-point conversion exceptions
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

  DrawPosCross(ACanvas, AText^.Pos);

  ACanvas.TextOut(
    AText^.Pos.X * INT_SHEET_GRID_SPACING,
    AText^.Pos.Y * INT_SHEET_GRID_SPACING,
    AText^.Text);
end;

procedure TItemsDrawer.DrawTextSelection(ACanvas: TCanvas; AElement: PTCElement);
var
  AText: PTCText;
begin
  if AElement = nil then Exit;

  AText := PTCText(AElement);

  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;

  ACanvas.Rectangle(
    AText^.Pos.X * INT_SHEET_GRID_SPACING,
    AText^.Pos.Y * INT_SHEET_GRID_SPACING,
    AText^.Pos.X * INT_SHEET_GRID_SPACING + ACanvas.TextWidth(AText^.Text),
    AText^.Pos.Y * INT_SHEET_GRID_SPACING + ACanvas.TextHeight(AText^.Text)
    );

  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsSolid;
end;

procedure TItemsDrawer.DrawPolyline(ACanvas: TCanvas; AElement: PTCElement);
var
  APolyline: PTCPolyline absolute AElement;
  i: Integer;
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := APolyline^.Width;
  ACanvas.Brush.Color := clBlack;

  if AElement = nil then Exit;

  for i := 1 to APolyline^.NPoints - 1 do
    ACanvas.Line(
      APolyline^.Points[i - 1].X * INT_SHEET_GRID_SPACING,
      APolyline^.Points[i - 1].Y * INT_SHEET_GRID_SPACING,
      APolyline^.Points[i].X * INT_SHEET_GRID_SPACING,
      APolyline^.Points[i].Y * INT_SHEET_GRID_SPACING
    );

  ACanvas.Pen.Width := 1;
end;

procedure TItemsDrawer.DrawPolylineSelection(ACanvas: TCanvas;
  AElement: PTCElement; APointNr: Integer);
begin

end;

procedure TItemsDrawer.DrawRasterImage(ACanvas: TCanvas; AElement: PTCElement);
var
  ARasterImage: PTCRasterImage absolute AElement;
  i: Integer;
begin
  if AElement = nil then Exit;

  DrawPosCross(ACanvas, AElement^.Pos);

  if ARasterImage^.ImageData <> nil then
  begin
    ACanvas.StretchDraw(
      Bounds(
      ARasterImage^.Pos.X * INT_SHEET_GRID_SPACING,
      ARasterImage^.Pos.Y * INT_SHEET_GRID_SPACING,
      Round(ARasterImage^.ImageData.Graphic.Width * ARasterImage^.Proportion * FLOAT_SHEET_GRID_PROPORTION),
      Round(ARasterImage^.ImageData.Graphic.Height * ARasterImage^.Proportion * FLOAT_SHEET_GRID_PROPORTION)
      ),
      ARasterImage^.ImageData.Graphic);
  end;
end;

procedure TItemsDrawer.DrawEllipse(ACanvas: TCanvas; AElement: PTCElement);
var
  AEllipse: PTCEllipse absolute AElement;
begin
  if AElement = nil then Exit;

  ACanvas.Ellipse(AEllipse^.Pos.X * INT_SHEET_GRID_SPACING, AEllipse^.Pos.Y * INT_SHEET_GRID_SPACING,
    AEllipse^.BottomRight.X * INT_SHEET_GRID_SPACING, AEllipse^.BottomRight.Y * INT_SHEET_GRID_SPACING);
end;

procedure TItemsDrawer.DrawPosCross(ACanvas: TCanvas; APos: TPoint);
begin
  ACanvas.Pen.Style := psDash;

  ACanvas.Line(
    APos.X * INT_SHEET_GRID_SPACING - INT_SHEET_DEFAULT_GRID_SPACING,
    APos.Y * INT_SHEET_GRID_SPACING,
    APos.X * INT_SHEET_GRID_SPACING + INT_SHEET_DEFAULT_GRID_SPACING,
    APos.Y * INT_SHEET_GRID_SPACING
    );

  ACanvas.Line(
    APos.X * INT_SHEET_GRID_SPACING,
    APos.Y * INT_SHEET_GRID_SPACING - INT_SHEET_DEFAULT_GRID_SPACING,
    APos.X * INT_SHEET_GRID_SPACING,
    APos.Y * INT_SHEET_GRID_SPACING + INT_SHEET_DEFAULT_GRID_SPACING
    );

  ACanvas.Pen.Style := psSolid;
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

{@@
  Converts simple grid coordinates to sheet coordinates already
  taking the rotation into account.
}
function TItemsDrawer.GridCoordsToSheet(AX, AY: Single): TPoint;
begin
  Result := FixCoordinates(Point(
    Round((DeltaX + AX) * INT_SHEET_GRID_SPACING),
    Round((DeltaY + AY) * INT_SHEET_GRID_SPACING)
  ));
end;

initialization

  vItemsDrawer := TItemsDrawer.Create;

finalization

  vItemsDrawer.Free;

end.

