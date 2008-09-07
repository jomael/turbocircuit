unit dbcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sdfdata, dbctrls, stdctrls, db, lmessages,
  controls,
  constants, tcsettings;

type

  { TComponentsDatabase }

  TComponentsDatabase = class(TObject)
  public
    FDataset: TSdfDataset;
    CurrentRecNo: Integer;
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
    class function  DBDrawingCodeToMemoString(AStr: string): string;
    class function  MemoStringToDBDrawingCode(AStr: string): string;
  end;

  { TDBDrawingCodeMemo }

  TDBDrawingCodeMemo = class(TCustomMemo)
  private
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FDBMemoFocused: Boolean;
    FDBMemoLoaded: Boolean;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetAutoDisplay(const AValue: Boolean);
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
  protected
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(AValue: Boolean); override;
    function WordWrapIsStored: boolean; override;
    procedure DataChange(Sender: TObject); virtual;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure UpdateData(Sender: TObject); virtual;
    procedure FocusRequest(Sender: TObject); virtual;
    procedure Loaded; override;
    procedure EditingDone; override;
    procedure Change; override;
    procedure KeyPress(var Key:Char); override;
    procedure WndProc(var AMessage : TLMessage); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMemo; virtual;
    property Field: TField read GetField;
  published
    property Align;
    property Anchors;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BorderSpacing;
    property Color;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Font;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property ParentFont;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property TabOrder;
    property Tabstop;
    property Visible;
    property WordWrap stored WordWrapIsStored;
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
  FDataset.MaxRecordLength := 2048;

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

  // Sets the initial record
  CurrentRecNo := 1;
  FDataset.First;
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

  CurField := FDataset.FieldByName(STR_DB_COMPONENTS_NAMEEN);
  FDataset.First;
  while not FDataset.EOF do
  begin
    AStringList.Add(CurField.Value);
    FDataset.CursorPosChanged;
    FDataset.Next;
  end;

  FDataset.First;
  CurrentRecNo := 1;
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
  if CurrentRecNo < AID then
  begin
    while (not FDataset.EOF) and (CurrentRecNo < AID) do
    begin
      FDataset.Next;
      FDataset.CursorPosChanged;
      Inc(CurrentRecNo);
    end;
  end
  // We are after the desired record, move back
  else if CurrentRecNo > AID  then
  begin
    while (CurrentRecNo >= 1) and (CurrentRecNo > AID) do
    begin
      FDataset.Prior;
      FDataset.CursorPosChanged;
      Dec(CurrentRecNo);
    end;
  end;
end;

class function TComponentsDatabase.DBDrawingCodeToMemoString(AStr: string): string;
begin
  Result := StringReplace(AStr, '#', LineEnding, [rfReplaceAll, rfIgnoreCase]);
end;

class function TComponentsDatabase.MemoStringToDBDrawingCode(AStr: string): string;
begin
  Result := StringReplace(AStr, LineEnding, '#', [rfReplaceAll, rfIgnoreCase]);
end;

{ TDBDrawingCodeMemo }

function TDBDrawingCodeMemo.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBDrawingCodeMemo.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBDrawingCodeMemo.GetField: TField;
begin
  Result:=FDataLink.Field;
end;

function TDBDrawingCodeMemo.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBDrawingCodeMemo.SetAutoDisplay(const AValue: Boolean);
begin
  if FAutoDisplay=AValue then exit;
  FAutoDisplay:=AValue;
  if FAutoDisplay then LoadMemo;
end;

procedure TDBDrawingCodeMemo.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBDrawingCodeMemo.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBDrawingCodeMemo.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBDrawingCodeMemo.SetReadOnly(AValue: Boolean);
begin
  inherited;
  FDataLink.ReadOnly:=AValue;
end;

function TDBDrawingCodeMemo.WordWrapIsStored: boolean;
begin
  Result:=not WordWrap;
end;

procedure TDBDrawingCodeMemo.DataChange(Sender: TObject);
begin
  if FDataLink.Field<>nil then begin
    if FDataLink.Field.IsBlob then begin
      if FAutoDisplay or (FDataLink.Editing and FDBMemoLoaded) then begin
        FDBMemoLoaded:=False;
        LoadMemo;
      end else begin
        Text:=Format('(%s)', [FDataLink.Field.DisplayLabel]);
        FDBMemoLoaded:=False;
      end;
    end else begin
      if FDBMemoFocused and FDataLink.CanModify then
        // Modification
        Text:=TComponentsDatabase.DBDrawingCodeToMemoString(FDataLink.Field.Text)
      else
        // Modification
        Text:=TComponentsDatabase.DBDrawingCodeToMemoString(FDataLink.Field.DisplayText);
      FDBMemoLoaded:=True;
    end
  end else begin
    if csDesigning in ComponentState then
      Text:=Name
    else
      Text:='';
    FDBMemoLoaded:=False;
  end;
end;

procedure TDBDrawingCodeMemo.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then datachange(sender)
  else
    begin
    Lines.Clear;
    FDataLink.reset;
    end;
end;

procedure TDBDrawingCodeMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBDrawingCodeMemo.UpdateData(Sender: TObject);
begin
  if not FDBMemoLoaded then exit;
  if FDataLink=nil then exit;
  if not FDataLink.CanModify then exit;
  // Modification
  FDataLink.Field.AsString:=TComponentsDatabase.MemoStringToDBDrawingCode(Text);
end;

constructor TDBDrawingCodeMemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FAutoDisplay:=True;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLInk.OnActiveChange := @ActiveChange;
  FDataLink.OnUpdateData:=@UpdateData;
end;

procedure TDBDrawingCodeMemo.FocusRequest(Sender: TObject);
begin
  //the FieldLink has requested the control
  //recieve focus for some reason..
  //perhaps an error occured?
  SetFocus;
end;

procedure TDBDrawingCodeMemo.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TDBDrawingCodeMemo.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

procedure TDBDrawingCodeMemo.Change;
begin
  FDatalink.Modified;
  inherited Change;
end;

procedure TDBDrawingCodeMemo.KeyPress(var Key: Char);
  function CheckValidChar: boolean;
  begin
    result := FDBMemoLoaded and (FDatalink.Field<>nil) and
      FDatalink.Field.IsValidChar(Key);
    if Result then
      FDatalink.Edit
    else
      Key := #0;
  end;
  function CheckEditingKey: boolean;
  begin
    result := FDbMemoLoaded;
    if Result then
      FDatalink.Edit
    else
      Key := #0;
  end;
begin
  inherited KeyPress(Key);

  case key of
    #32..#255: // alphabetic characters
      CheckValidChar;
    ^M: // enter key
      if not CheckEditingKey then
        LoadMemo;
    #27: // escape
      if FDbMemoLoaded then
        FDatalink.Reset
      else
        Key:=#0;
    // Verifyes if we are in edit mode for special keys may change the text
    // Ctrl+I = Tab
    // Ctrl+J = LineFeed
    // Ctrl+H = Backspace
    ^X, ^V, ^Z, ^I, ^J, ^H:
      CheckEditingKey;
    // Don't do anything for special keys that don't change the text
    // Like Ctrl+C for example
  end;
end;

procedure TDBDrawingCodeMemo.WndProc(var AMessage: TLMessage);
begin
  case AMessage.Msg of
    LM_CLEAR,
    LM_CUT,
    LM_PASTE:
      FDatalink.Edit;
  end;
  inherited WndProc(AMessage);
end;

destructor TDBDrawingCodeMemo.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  inherited Destroy;
end;

procedure TDBDrawingCodeMemo.LoadMemo;
begin
  if not FDBMemoLoaded and (FDataLink.Field<>nil)
  and FDataLink.Field.IsBlob then begin
    try
      // Modification
      Lines.Text := TComponentsDatabase.DBDrawingCodeToMemoString(FDataLink.Field.AsString);
      FDBMemoLoaded:=True;
    except
      on E:EInvalidOperation do
        Lines.Text:='('+E.Message+')';
    end;
  end;
end;

initialization

  vComponentsDatabase := TComponentsDatabase.Create;

finalization

  vComponentsDatabase.Free;

end.

