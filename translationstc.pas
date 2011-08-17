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
    { Main menu strings }
    lpFile, lpDocument, lpComponents, lpHelp: string;
    lpDocumentOptions: string;
    lpFileNew, lpFileOpen, lpFileSave, lpFileSaveAs, lpFileExportPng,
      lpFileExit: string;
    lpRecreateComponentsDatabase, lpComponentsEditor: string;
    lpHelpAbout: string;

    { Main Form }
    lpMainFormArrow, lpMainFormComponent, lpMainFormWire,
      lpMainFormText, lpMainFormPolyline, lpMainFormRasterImage: string;
    lpMainFormChooseComponent: string;

    { I/O strings }
    lpSaveDiagramFilter, lpSavePngFilter: string;

    { Document state related }
    lpUntitled: string;

    { Components editor dialog }
    lpCEInstructions, lpCEID, lpCENameEN, lpCENamePT, lpCEWidth, lpCEHeight,
      lpCEPins, lpCEDrawingCode, lpCEClose, lpCEPreview, lpCECaption: string;

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

{@@
  Translates the user interface strings to english
}
procedure TTranslations.TranslateToEnglish;
begin
  { Main menu strings }
  lpFile := 'File';
  lpDocument := 'Document';
  lpComponents := 'Components';
  lpHelp := 'Help';
  lpDocumentOptions := 'Document Options';
  lpFileNew := 'New';
  lpFileOpen := 'Open';
  lpFileSave := 'Save';
  lpFileSaveAs := 'Save As';
  lpFileExportPng := 'Export as PNG';
  lpFileExit := 'Exit';
  lpRecreateComponentsDatabase := 'Recreate Components Database';
  lpComponentsEditor := 'Components Editor';
  lpHelpAbout := 'About';

  { Main Form }
  lpMainFormArrow := 'Arrow';
  lpMainFormComponent := 'Component';
  lpMainFormWire := 'Wire';
  lpMainFormText := 'Text';
  lpMainFormPolyline := 'Polyline';
  lpMainFormRasterImage := 'Raster Image';

  lpMainFormChooseComponent := 'Component type:';

  { I/O strings }
  lpSaveDiagramFilter := 'All supported formats|*.tc;*.eps|Turbo Circuit Diagrams|*.tc|All files|*.*';
  lpSavePngFilter := 'Portable Network Graphic|*.png|All files|*.*';

  { Document state related }
  lpUntitled := 'Untitled';

  { Components editor dialog }
  lpCEInstructions := 'Atention: Changes are saved automatically and '
   + 'cannot be undone except by reloading the database from the backup.';
  lpCEID := 'ID:';
  lpCENameEN := 'English:';
  lpCENamePT := 'Português:';
  lpCEWidth := 'Width:';
  lpCEHeight := 'Height:';
  lpCEPins := 'Pins';
  lpCEDrawingCode := 'Drawing Code:';
  lpCEClose := 'Close';
  lpCEPreview := 'Preview';
  lpCECaption := 'Components Editor';
end;

{@@
  Translates the user interface strings to portuguese
}
procedure TTranslations.TranslateToPortuguese;
begin

  { Document state related }
  lpUntitled := 'Sem Título';
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

