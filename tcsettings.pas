unit tcsettings;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFDEF Win32}
  {$DEFINE Windows}
{$ENDIF}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
{$ifdef Darwin}
  MacOSAll,
{$endif}
  Classes, SysUtils, IniFiles, Forms,
  constants;

type
  { TConfigurations }

  TConfigurations = class(TObject)
  private
    ConfigFilePath: string;
  public
    MyDirectory: string;
    ComponentsDBFile: string;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromFile(Sender: TObject);
    procedure Save(Sender: TObject);
  end;

var
  vConfigurations: TConfigurations;

implementation

{ TConfigurations }

{
  TConfigurations.Create ()

  Creates an object,
  populates the class with the default configurations and
  then tryes to load the configurations from the XML file

  Under Mac OS X there is also the need to find the location
  of the Application Bundle
}
constructor TConfigurations.Create;
{$ifdef Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef Windows}
  ConfigFilePath := ExtractFilePath(Application.EXEName) + 'magnifier.ini';
{$endif}
{$ifdef Unix}
  {$ifdef Darwin}
    ConfigFilePath := GetEnvironmentVariable('HOME') + '/.magnifier.ini';
  {$else}
    ConfigFilePath := GetAppConfigFile(False) + '.conf';
  {$endif}
{$endif}

  // Under Mac OS X we need to get the location of the bundle
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  MyDirectory := pathStr + BundleResourcesDirectory;
{$endif}

{$ifdef Windows}
  MyDirectory := ExtractFilePath(Application.EXEName);
{$endif}

//  ReadFromFile(nil);

{  case Language of
   ID_MENU_PORTUGUESE: vTranslations.TranslateToPortuguese;
   ID_MENU_SPANISH: vTranslations.TranslateToSpanish;
   ID_MENU_FRENCH: vTranslations.TranslateToFrench;
   ID_MENU_GERMAN: vTranslations.TranslateToGerman;
   ID_MENU_ITALIAN: vTranslations.TranslateToItalian;
  else
   vTranslations.TranslateToEnglish;
  end;

  vTranslations.UpdateTranslations;  }

  ComponentsDBFile := MyDirectory + STR_DB_COMPONENTS_FILE;
end;

{  TConfigurations.Destroy ()

   Dont call this method directly. Use Free instead.
}
destructor TConfigurations.Destroy;
begin
//  Save(nil);

  inherited Destroy;
end;

procedure TConfigurations.ReadFromFile(Sender: TObject);
var
  MyFile: TIniFile;
begin
  MyFile := TIniFile.Create(ConfigFilePath);
  try
    {$ifdef UNIX}
      {$ifndef DARWIN}
        MyDirectory := MyFile.ReadString(SectionUnix, IdentMyDirectory, DefaultDirectory);
      {$endif}
    {$endif}
  finally
    MyFile.Free;
  end;
end;

procedure TConfigurations.Save(Sender: TObject);
var
  MyFile: TIniFile;
begin
  MyFile := TIniFile.Create(ConfigFilePath);
  try
//    MyFile.WriteBool(SectionGeneral, IdentgraphicalBorder, graphicalBorder);
  finally
    MyFile.Free;
  end;
end;

initialization

  vConfigurations := TConfigurations.Create;

finalization

  FreeAndNil(vConfigurations);

end.

