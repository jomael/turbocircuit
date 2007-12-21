unit constants;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics;

{*******************************************************************
*  Main data structures
*******************************************************************}
type
  TCComponentOrientation = (coEast = 0, coNorth = 1, coWest = 2, coSouth = 3);

  PTCComponent = ^TCComponent;
  
  TCComponent = record
    Name: string;
    TypeID: Integer;
    Orientation: TCComponentOrientation;
    PosX, PosY: Integer;
    Next, Previous: PTCComponent;
  end;
  
  PTCWire = ^TCWire;

  TCWire = record
    PtFrom: TPoint;
    PtTo: TPoint;
    Next, Previous: PTCWire;
  end;
  
  TCWirePart = (wpPtFrom, wpPtTo);
  
  TCTool = (toolArrow, toolComponent, toolWire);
  
{*******************************************************************
*  General use data structures
*******************************************************************}
type
  T10Strings = array[0..9] of shortstring;

{*******************************************************************
*  Drawing code commands
*******************************************************************}
const
  STR_DRAWINGCODE_LINE      = 'LINE';

{*******************************************************************
*  Database field names
*******************************************************************}
const
  STR_DB_COMPONENTS_FILE        = 'U:\turbocircuit\Components.dat';
  STR_DB_COMPONENTS_TABLE       = 'Components';
  STR_DB_COMPONENTS_ID          = 'ID';
  STR_DB_COMPONENTS_NAMEEN      = 'NAMEEN';
  STR_DB_COMPONENTS_NAMEPT      = 'NAMEPT';
  STR_DB_COMPONENTS_DRAWINGCODE = 'DRAWINGCODE';
  STR_DB_COMPONENTS_HEIGHT      = 'HEIGHT';
  STR_DB_COMPONENTS_WIDTH       = 'WIDTH';
  STR_DB_COMPONENTS_PINS        = 'PINS';

{*******************************************************************
*  Schematics file constants
*******************************************************************}
const
  STR_TCSCHEMATICS_IDENTIFIER   = 'Turbo Circuit 1.0';

{*******************************************************************
*  StatusBar panel identifiers
*******************************************************************}
const
  ID_STATUS_MOUSEPOS        = 0;

{*******************************************************************
*  General User Interface constants
*******************************************************************}
const
  INT_SHEET_GRID_SPACING     = 10;
  INT_SHEET_GRID_HALFSPACING = INT_SHEET_GRID_SPACING div 2;
  INT_SHEET_MAX_WIDTH        = 1000;
  INT_SHEET_MAX_HEIGHT       = 1000;
  INT_SHEET_DEFAULT_WIDTH    = 500;
  INT_SHEET_DEFAULT_HEIGHT   = 500;

{*******************************************************************
*  Strings not to be translated
*******************************************************************}
const

  szAppTitle  = 'Turbo Circuit';

  lpSeparator = '-';
  lpComma     = ',';
  
  lpEnglish   = 'English';
  lpPortugues = 'Português';

implementation

end.

