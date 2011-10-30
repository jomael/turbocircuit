unit fpvelectrialelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpimage, fpcanvas,
  //
  fpvectorial;

const
  WIRE_SUBPART_DEST = 1;

type

  { TWire }

  TWire = class(TvEntity)
  public
    DestPos: T3DPoint;
//    procedure Assign(ASource: TPath);
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; override;
    procedure TransladeSubpart(ADeltaX, ADeltaY: Integer; ASubpart: Cardinal); override;
    procedure Render(ADest: TFPCustomCanvas; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

implementation

{ TWire }

function TWire.TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult;
begin
  if (APos.X = X) and (APos.Y = Y) then Result := vfrFound
  else if (Apos.X = DestPos.X) and (APos.Y = DestPos.Y) then
  begin
    Result := vfrSubpartFound;
    ASubpart := WIRE_SUBPART_DEST;
  end
  else Result := vfrNotFound;
end;

procedure TWire.TransladeSubpart(ADeltaX, ADeltaY: Integer; ASubpart: Cardinal);
begin
  if ASubpart = WIRE_SUBPART_DEST then
  begin
    DestPos.X := DestPos.X + ADeltaX;
    DestPos.Y := DestPos.Y + ADeltaY;
  end;
end;

procedure TWire.Render(ADest: TFPCustomCanvas; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
begin

end;

end.

