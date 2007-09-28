program turbocircuit;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  app,
  drawer,
  schematics,
  dbcomponents,
  translationstc,
  constants, document;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, vMainForm);
  Application.Run;
end.

