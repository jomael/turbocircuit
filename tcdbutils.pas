unit tcdbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation

{function dbcallback(UserData: Pointer; argc: cint; argv, azColName: PPChar): cint; cdecl;
var
  i: Integer;
begin  }
{  for i := 0 to argc - 1 do
    if argv[i] <> nil then
      WriteLn(azColName[i], ' = ',  argv[i])
    else
      WriteLn(azColName[i], ' = ',  'NIL');}

{//  WriteLn('');

  if argc < 3 then
  begin
    WriteLn('Too few collums');
    Exit;
  end;

  SetLength(vComponentsDatabase.ComponentTypes, Length(vComponentsDatabase.ComponentTypes) + 1);

  vComponentsDatabase.ComponentTypes[Length(vComponentsDatabase.ComponentTypes) - 1].ID := StrToInt(argv[0]);
  vComponentsDatabase.ComponentTypes[Length(vComponentsDatabase.ComponentTypes) - 1].Name := argv[1];
  vComponentsDatabase.ComponentTypes[Length(vComponentsDatabase.ComponentTypes) - 1].ImageFile := argv[2];
end;

procedure TComponentsDatabase.LoadComponents;
var
  db: Psqlite3;
  zErrMsg: PChar = nil;
  rc: cint;
  DBFileName: string;
begin
  DBFileName := '/Users/felipemonteirodecarvalho/turbocircuit/turbocircuit.dat';

  rc := sqlite3_open(PChar(DBFileName), @db);

  if (rc <> 0) then
  begin
    //WriteLn('Can''t open database ', DBFileName);
    sqlite3_close(db);
    Halt(1);
  end;

  rc := sqlite3_exec(db, 'SELECT * FROM Componentes;', @dbcallback, nil, @zErrMsg);
  if (rc <> SQLITE_OK) then
  begin
    //WriteLn('SQL error: ', zErrMsg);
    sqlite3_free(zErrMsg);
  end;

  sqlite3_close(db);
end;       }

end.

