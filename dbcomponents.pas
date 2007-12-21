unit dbcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3ds,
  constants;

type

  { TComponentsDatabase }

  TComponentsDatabase = class(TObject)
  public
    FDataset: TSqlite3Dataset;
    constructor Create;
    destructor  Destroy; override;
    { Database access methods }
    procedure FillStringListWithNames(AStringList: TStrings);
    function  GetDrawingCode(AID: Integer): string;
    function  GetHeight(AID: Integer): Integer;
    function  GetPins(AID: Integer): Integer;
    function  GetWidth(AID: Integer): Integer;
  end;

var
  vComponentsDatabase: TComponentsDatabase;

implementation

{ TComponentsDatabase }

constructor TComponentsDatabase.Create;
begin
  inherited Create;

  FDataset := TSqlite3Dataset.Create(nil);
  FDataset.FileName := STR_DB_COMPONENTS_FILE;
  FDataset.TableName := STR_DB_COMPONENTS_TABLE;
  FDataset.PrimaryKey := STR_DB_COMPONENTS_ID;
  FDataset.Active := True;
end;

destructor TComponentsDatabase.Destroy;
begin
  FDataset.Free;

  inherited Destroy;
end;

procedure TComponentsDatabase.FillStringListWithNames(AStringList: TStrings);
var
  i: Integer;
begin
  AStringList.Clear;

  for i := 1 to FDataset.RecordCount do
  begin
    FDataset.RecNo := i;
    AStringList.Add(FDataset.FieldByName(STR_DB_COMPONENTS_NAMEEN).Value);
  end;
end;

function TComponentsDatabase.GetDrawingCode(AID: Integer): string;
begin
  FDataset.RecNo := AID;
  Result := FDataset.FieldByName(STR_DB_COMPONENTS_DRAWINGCODE).Value;
end;

function TComponentsDatabase.GetHeight(AID: Integer): Integer;
begin
  FDataset.RecNo := AID;
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_HEIGHT).Value);
end;

function TComponentsDatabase.GetPins(AID: Integer): Integer;
begin
  FDataset.RecNo := AID;
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_PINS).Value);
end;

function TComponentsDatabase.GetWidth(AID: Integer): Integer;
begin
  FDataset.RecNo := AID;
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_WIDTH).Value);
end;

initialization

  vComponentsDatabase := TComponentsDatabase.Create;

finalization

  vComponentsDatabase.Free;

end.

