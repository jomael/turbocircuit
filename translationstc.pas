{
translationstc.pas

Class which stores the translated strings

Copyright (C) 2007 Felipe Monteiro de Carvalho

This file is part of Turbo Circuit.

Turbo Circuit is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Turbo Circuit is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating Turbo Circuit into proprietary programs.

AUTHORS: Felipe Monteiro de Carvalho
}
unit translationstc;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFDEF Win32}
  {$DEFINE Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils, constants;

type
  { TTranslations }

  TTranslations = class(TObject)
  public
    { Main Menu strings }
    lpFile, lpComponents, lpHelp: string;
    { File Menu strings }
    lpFileNew, lpFileOpen, lpFileSave, lpFileSaveAs, lpFileExit: string;
    { Components Menu strings }
    lpCheckComponentsDB, lpEditComponents: string;
    { Help Menu strings }
    lpHelpAbout: string;
    { Methods }
    procedure TranslateToEnglish;
    procedure TranslateToPortuguese;
    procedure UpdateTranslations;
  end;

var
  vTranslations: TTranslations;

implementation

{$ifndef fpc}
uses Windows;
{$endif}

{ TTranslations }

{*******************************************************************
*  TTranslations.TranslateToEnglish ()
*
*  DESCRIPTION:    Translates the user interface strings to english
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TTranslations.TranslateToEnglish;
begin
  { Main Menu strings }
  lpFile := 'File';
  lpComponents := 'Components';
  lpHelp := 'Help';
  
  { File Menu strings }
  lpFileNew := 'New';
  lpFileOpen := 'Open';
  lpFileSave := 'Save';
  lpFileSaveAs := 'Save As';
  lpFileExit := 'Exit';

  { Components Menu strings }
  lpCheckComponentsDB := 'Check Components Database';
  lpEditComponents := 'Edit Components';

  { Help Menu strings }
  lpHelpAbout := 'About';
end;

{*******************************************************************
*  TTranslations.TranslateToPortuguese ()
*
*  DESCRIPTION:    Translates the user interface strings to portuguese
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TTranslations.TranslateToPortuguese;
begin

end;

{*******************************************************************
*  TTranslations.UpdateTranslations ()
*
*  DESCRIPTION:    Under Desktop Windows, this procedure converts the
*                  UTF-8 strings to ISO 8859-1
*
*                  Under all other platforms no changes are required
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TTranslations.UpdateTranslations;

  {$ifndef fpc}
{  procedure UpdateString(var AStr: string);
  var
    CharBuffer: array[0..255] of Char;
    WideBuffer: array[0..255] of WideChar;
  begin
    FillChar(CharBuffer, SizeOf(CharBuffer), #0);
    FillChar(WideBuffer, SizeOf(WideBuffer), #0);
    MultibyteToWideChar(CP_UTF8, 0, PChar(AStr), -1, @WideBuffer, 255);
    WideCharToMultibyte(CP_ACP, 0, @WideBuffer, -1, @CharBuffer, 255, nil, nil);
    AStr := PChar(@CharBuffer);
  end;     }
  {$endif}

begin
  {$ifndef fpc}
  { Menu strings }
  {$endif}
end;

{*******************************************************************
*  Initialization section
*
*  DESCRIPTION:    Initializes the translations.
*
*******************************************************************}
initialization

  vTranslations := TTranslations.Create;
  vTranslations.TranslateToEnglish; // Default

{*******************************************************************
*  Finalization section
*
*  DESCRIPTION:    Free memory allocated on the initialization section
*
*******************************************************************}
finalization

  FreeAndNil(vTranslations);

end.

