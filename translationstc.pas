{
translationstc.pas

Class to store and translate user interface strings

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

