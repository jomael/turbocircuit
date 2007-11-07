unit constants;

{$mode objfpc}{$H+}

interface

uses Graphics;

{*******************************************************************
*  Main data structures
*******************************************************************}
type
  TCComponent = record
    Name: string;
    TypeID: Integer;
    PosX, PosY: Integer;
  end;
  
  TCComponentType = record
    ID: Integer;
    Name: string;
    ImageFile: string;
    ImageObject: TBitmap;
  end;

{*******************************************************************
*  Menu identifiers
*******************************************************************}
const
  ID_MENU_FILE              = 0;
  ID_MENU_COMPONENTS        = 1;
  ID_MENU_HELP              = 2;

  ID_MENU_FILE_NEW          = 0;
  ID_MENU_FILE_OPEN         = 1;
  ID_MENU_FILE_SAVE         = 2;
  ID_MENU_FILE_SAVE_AS      = 3;
  ID_MENU_FILE_EXIT         = 4;

  ID_MENU_COMPONENTS_CHECK  = 0;
  ID_MENU_COMPONENTS_EDIT   = 1;

  ID_MENU_HELP_HELP         = 0;
  ID_MENU_HELP_SEPARATOR    = 1;
  ID_MENU_HELP_ABOUT        = 2;

{*******************************************************************
*  StatusBar identifiers
*******************************************************************}
const
  ID_STATUS_MOUSEPOS        = 0;

{*******************************************************************
*  General User Interface constants
*******************************************************************}
const
  INT_SHEET_GRID_SPACING    = 10;
  INT_SHEET_MAX_WIDTH       = 1000;
  INT_SHEET_MAX_HEIGHT      = 1000;

{*******************************************************************
*  Strings not to be translated
*******************************************************************}
const

  szAppTitle = 'Turbo Circuit';

  lpSeparator = '-';

{$ifdef fpc}
  lpEnglish   = 'English';
  lpPortugues = 'PortuguÃªs';
{$else}
  lpEnglish   = 'English';
  lpPortugues = 'Português';
{$endif}

implementation

end.

