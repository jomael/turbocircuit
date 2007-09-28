unit document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, constants;

type

  { TDocument }

  TDocument = class(TObject)
  public
    SheetWidth, SheetHeight: Integer;
    Components: array of TCComponent;
    constructor Create;
  end;

var
  vDocument: TDocument;

implementation

{ TDocument }

constructor TDocument.Create;
begin
  inherited Create;

  SheetWidth := 500;
  SheetHeight := 500;
end;

end.

