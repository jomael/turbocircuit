{
turbocircuit.dpr

Main program file and program initialization

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


  Development notes:

  When adding new tools, procede as following:

  * Add the tool to the constants in constants.pas
  * Add lists for the tool in tclists.pas
  * Add code to save/load this new item in tcfileformat.pas and document.pas
  * Add event handling for it in toolscode.pas
  * Add drawing code for it in schematics.pas
  * Add description of the new item in document.pas

}
program turbocircuit;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  app,
  schematics,
  dbcomponents,
  translationstc,
  constants,
  document,
  dlgabout,
  dlgcomponentseditor,
  tcutils,
  tcdbutils,
  drawer,
  tcfileformat,
  dlgdocumentopts,
  tclists,
  tcsettings,
  toolscode, fpvectorialpkg, fpvelectrialelements;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, vMainForm);
  Application.CreateForm(TvAbout, vAbout);
  Application.CreateForm(TvComponentsEditor, vComponentsEditor);
  Application.CreateForm(TDocumentOptions, vDocumentOptions);
  Application.Run;
end.

