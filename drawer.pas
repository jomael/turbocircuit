unit drawer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics;

type

  { TDrawer }

  TDrawer = class(TCustomControl)
  private
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    bmpOutput: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    procedure Paint; override;
  end;

var
  vDrawer: TDrawer;
  
implementation

uses document, constants, app;

procedure TDrawer.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  vMainForm.pnlStatusBar.Panels.Items[ID_STATUS_MOUSEPOS].Text :=
   'X: ' + IntToStr(X) + ' Y: ' + IntToStr(Y);
end;

constructor TDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnMouseMove := HandleMouseMove;

  bmpOutput := TBitmap.Create;
  bmpOutput.Width := INT_SHEET_MAX_WIDTH;
  bmpOutput.Height := INT_SHEET_MAX_HEIGHT;
end;

destructor TDrawer.Destroy;
begin
  bmpOutput.Free;

  inherited Destroy;
end;

procedure TDrawer.DrawToCanvas(ACanvas: TCanvas);
var
  x, y, i: Integer;
begin
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
  for i := 0 to Length(vDocument.Components) - 1 do
  begin
  end;
  
  { Wires }
  
end;

{*******************************************************************
*  TDrawer.Paint ()
*
*  DESCRIPTION:    Processes Paint messages for the TDrawer control
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TDrawer.Paint;
begin
  { Ask for update of the whole window }
  DrawToCanvas(bmpOutput.Canvas);

  { Copies the buffer bitmap to the canvas }
  Canvas.Draw(0, 0, bmpOutput);

  inherited Paint;
end;

end.

