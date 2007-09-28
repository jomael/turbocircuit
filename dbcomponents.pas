unit dbcomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3, ctypes;

type

  { TComponentsDatabase }

  TComponentsDatabase = class(TObject)
  public
    constructor Create;
  end;

var
  vComponentsDatabase: TComponentsDatabase;

implementation

{ TComponentsDatabase }

function dbcallback(NotUsed: Pointer; argc: cint; argv, azColName: PPChar): cint; cdecl;
var
  i: Integer;
begin
  for i := 0 to argc -1 do
    if argv[i] <> nil then
      WriteLn(azColName[i], ' = ',  argv[i])
    else
      WriteLn(azColName[i], ' = ',  'NIL');

  WriteLn('');
end;

constructor TComponentsDatabase.Create;
var
  db: Psqlite3;
  zErrMsg: PChar = nil;
  rc: cint;
begin
  inherited Create;
  
  rc := sqlite3_open('/Users/felipemonteirodecarvalho/components.db', @db);
  
  if (rc <> 0) then
  begin
    WriteLn('Can''t open database', '/Users/felipemonteirodecarvalho/components.db');
    sqlite3_close(db);
    Halt(1);
  end;
  
  rc := sqlite3_exec(db, 'CREATE TABLE Componentes;', @dbcallback, nil, @zErrMsg);
  if (rc <> SQLITE_OK) then
  begin
    WriteLn('SQL error: ', zErrMsg);
    sqlite3_free(zErrMsg);
  end;
  sqlite3_close(db);
end;

end.

