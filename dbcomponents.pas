unit dbcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sdfdata, db,{sqlite3ds,}
  constants, tcsettings;

type

  { TComponentsDatabase }

  TComponentsDatabase = class(TObject)
  public
    FDataset: TSdfDataset; //TSqlite3Dataset;
    constructor Create;
    destructor  Destroy; override;
    { Database access methods }
    procedure FillStringListWithNames(AStringList: TStrings);
    function  GetDrawingCode(AID: Integer): string;
    function  GetHeight(AID: Integer): Integer;
    function  GetPins(AID: Integer): Integer;
    function  GetWidth(AID: Integer): Integer;
    procedure GoToRec(AID: Integer);
    { Data conversion routines }
    function  DBDrawingCodeToMemoString(AStr: string): string;
  end;

  { TDBDrawingCodeMemo }

  TDBDrawingCodeMemo = class(TDBMemo)
  protected
    procedure DataChange(Sender: TObject); override;
    procedure UpdateData(Sender: TObject); override;
  end;

var
  vComponentsDatabase: TComponentsDatabase;

implementation

{ TComponentsDatabase }

constructor TComponentsDatabase.Create;
begin
  inherited Create;

  FDataset := TSdfDataset.Create(nil);
  FDataset.FileName := vConfigurations.ComponentsDBFile;

  // Not necessary with TSdfDataset
//  FDataset.TableName := STR_DB_COMPONENTS_TABLE;
//  FDataset.PrimaryKey := STR_DB_COMPONENTS_ID;

  // Adds field definitions
  FDataset.FieldDefs.Add('ID', ftString);
  FDataset.FieldDefs.Add('NAMEEN', ftString);
  FDataset.FieldDefs.Add('NAMEPT', ftString);
  FDataset.FieldDefs.Add('HEIGHT', ftString);
  FDataset.FieldDefs.Add('WIDTH', ftString);
  FDataset.FieldDefs.Add('PINS', ftString);
  FDataset.FieldDefs.Add('DRAWINGCODE', ftString);

  // Necessary for TSdfDataset
  FDataset.Delimiter := ',';
  FDataset.FirstLineAsSchema := True;

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
  CurField: TField;
begin
  AStringList.Clear;

{  for i := 1 to FDataset.RecordCount do
  begin
    FDataset.RecNo := i;
    CurField := FDataset.FieldByName(STR_DB_COMPONENTS_NAMEEN);
    AStringList.Add(CurField.Value);
  end;}

  CurField := FDataset.FieldByName(STR_DB_COMPONENTS_NAMEEN);
  FDataset.First;
  while not FDataset.EOF do
  begin
    AStringList.Add(CurField.Value);
    FDataset.CursorPosChanged;
    FDataset.Next;
  end;
end;

function TComponentsDatabase.GetDrawingCode(AID: Integer): string;
begin
  GoToRec(AID);
  Result := FDataset.FieldByName(STR_DB_COMPONENTS_DRAWINGCODE).Value;
end;

function TComponentsDatabase.GetHeight(AID: Integer): Integer;
begin
  GoToRec(AID);
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_HEIGHT).Value);
end;

function TComponentsDatabase.GetPins(AID: Integer): Integer;
begin
  GoToRec(AID);
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_PINS).Value);
end;

function TComponentsDatabase.GetWidth(AID: Integer): Integer;
begin
  GoToRec(AID);
  Result := StrToInt(FDataset.FieldByName(STR_DB_COMPONENTS_WIDTH).Value);
end;

{
  Moves to the desired record using TDataset.Next and TDataset.Prior
  Avoids using TDataset.RecNo which doesn't work in all datasets
}
procedure TComponentsDatabase.GoToRec(AID: Integer);
begin
  // We are before the desired record, move forward
  if AID < FDataset.RecNo then
  begin
    while (not FDataset.EOF) and (AID < FDataset.RecNo) do
    begin
      FDataset.Next;
      FDataset.CursorPosChanged;
    end;
  end
  // We are after the desired record, move back
  else if AID > FDataset.RecNo then
  begin
    while (AID < FDataset.RecNo) do
    begin
      FDataset.Next;
      FDataset.CursorPosChanged;
    end;
  end;
end;

function TComponentsDatabase.DBDrawingCodeToMemoString(AStr: string): string;
begin
  Result := StringReplace(AStr, '#', LineEnding, [rfReplaceAll, rfIgnoreCase]);
end;

{ TDBDrawingCodeMemo }

{@@
  Loads data from the database to the memo
}
procedure TDBDrawingCodeMemo.DataChange(Sender: TObject);
begin
  inherited DataChange(Sender);
end;

{@@
  Loads data from the memo to the database
}
procedure TDBDrawingCodeMemo.UpdateData(Sender: TObject);
begin
  if not FDBMemoLoaded then exit;
  if FDataLink=nil then exit;
  if not FDataLink.CanModify then exit;
  FDataLink.Field.AsString:=Text;
end;

initialization

  vComponentsDatabase := TComponentsDatabase.Create;

finalization

  vComponentsDatabase.Free;

end.

