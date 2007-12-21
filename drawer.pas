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
  constants, tcutils;

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
    procedure DrawComponentSelection(ACanvas: TCanvas; AComponent: PTCComponent);
    { Wire drawing methods }
    procedure DrawWire(ACanvas: TCanvas; AWire: PTCWire);
    procedure DrawWireSelection(ACanvas: TCanvas; AWire: PTCWire; AWirePart: TCWirePart);
    { General methods }
    function  FixCoordinates(APoint: TPoint): TPoint;
  end;

var
  vItemsDrawer: TItemsDrawer;
  
implementation

{ TItemsDrawer }

procedure TItemsDrawer.DrawComponentFromString(ACanvas: TCanvas; AString: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Text := AString;
    DrawComponentFromStringList(ACanvas, StringList);
  finally
    StringList.Free;
  end;
end;

procedure TItemsDrawer.DrawComponentFromStringList(ACanvas: TCanvas;
 AStringList: TStrings);
var
  i: Integer;
  Cmds: T10Strings;
  PtFrom, PtTo: TPoint;
begin
  for i := 0 to AStringList.Count - 1 do
  begin
    Cmds := SeparateString(AStringList.Strings[i], lpComma);

    if Cmds[0] = STR_DRAWINGCODE_LINE then
    begin
      PtFrom.X := Round((DeltaX + StrToFloat(Cmds[1])) * INT_SHEET_GRID_SPACING);
      PtFrom.Y := Round((DeltaY + StrToFloat(Cmds[2])) * INT_SHEET_GRID_SPACING);
      PtTo.X := Round((DeltaX + StrToFloat(Cmds[3])) * INT_SHEET_GRID_SPACING);
      PtTo.Y := Round((DeltaY + StrToFloat(Cmds[4])) * INT_SHEET_GRID_SPACING);

      PtFrom := FixCoordinates(PtFrom);
      PtTo := FixCoordinates(PtTo);

      ACanvas.Line(PtFrom, PtTo);
    end;
  end;
end;

procedure TItemsDrawer.DrawComponentSelection(ACanvas: TCanvas;
  AComponent: PTCComponent);
begin
  ACanvas.Line(
   AComponent^.PosX * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
   AComponent^.PosY * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
   AComponent^.PosX * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
   AComponent^.PosY * INT_SHEET_GRID_SPACING);

  ACanvas.Line(
   AComponent^.PosX * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
   AComponent^.PosY * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
   AComponent^.PosX * INT_SHEET_GRID_SPACING,
   AComponent^.PosY * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING);
end;

procedure TItemsDrawer.DrawWire(ACanvas: TCanvas; AWire: PTCWire);
var
  PtFrom, PtTo: TPoint;
begin
  PtFrom.X := AWire^.PtFrom.X * INT_SHEET_GRID_SPACING;
  PtFrom.Y := AWire^.PtFrom.Y * INT_SHEET_GRID_SPACING;
  PtTo.X := AWire^.PtTo.X * INT_SHEET_GRID_SPACING;
  PtTo.Y := AWire^.PtTo.Y * INT_SHEET_GRID_SPACING;

  ACanvas.Line(PtFrom, PtTo);
end;

procedure TItemsDrawer.DrawWireSelection(ACanvas: TCanvas; AWire: PTCWire;
  AWirePart: TCWirePart);
begin
  case AWirePart of
  
  wpPtFrom: ACanvas.Rectangle(
             AWire^.PtFrom.X * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtFrom.Y * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtFrom.X * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING,
             AWire^.PtFrom.Y * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING);
  
  wpPtTo:   ACanvas.Rectangle(
             AWire^.PtTo.X * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.Y * INT_SHEET_GRID_SPACING - INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.X * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING,
             AWire^.PtTo.Y * INT_SHEET_GRID_SPACING + INT_SHEET_GRID_HALFSPACING);

  end;
end;

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

