{
app.pas

Main window of the program

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
unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Menus, ExtCtrls, ComCtrls,
  ActnList, Buttons, StdCtrls, Dialogs,
  schematics, dbcomponents, constants, translationstc, document,
  dlgcomponentseditor, dlgabout, dlgdocumentopts;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnText: TSpeedButton;
    cmbComponents: TComboBox;
    lblChooseComponent: TLabel;
    mnuDocument: TMenuItem;
    mnuDocumentOptions: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuMainMenu: TMainMenu;
    mnuFileExit: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuHelp: TMenuItem;
    mnuComponents: TMenuItem;
    mnuHelpHelp: TMenuItem;
    mnuHelpSeparator: TMenuItem;
    mnuRecreateComponentsDatabase: TMenuItem;
    mnuComponentsEditor: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileSeparator: TMenuItem;
    FToolsNotebook: TNotebook;
    dialogOpen: TOpenDialog;
    Page1: TPage;
    Page2: TPage;
    pnlToolbar: TPanel;
    pnlTools: TPanel;
    pnlStatusBar: TStatusBar;
    btnArrow: TSpeedButton;
    btnComponent: TSpeedButton;
    btnWire: TSpeedButton;
    dialogSave: TSaveDialog;
    procedure HandleChangeTool(ASender: TObject);
    procedure HandleChooseNewComponentType(ASender: TObject);
    procedure HandleClose(ASender: TObject);
    procedure HandleFileNew(ASender: TObject);
    procedure HandleFileOpen(ASender: TObject);
    procedure HandleFileSave(ASender: TObject);
    procedure HandleFileSaveAs(ASender: TObject);
    procedure HandleRecreateComponentsDatabaseClick(Sender: TObject);
    procedure HandleShowAboutBox(ASender: TObject);
    procedure HandleShowComponentsEditor(ASender: TObject);
    procedure HandleShowDocumentOptions(ASender: TObject);
    procedure HandleUpdateSchematicsMousePos(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure TranslateMainMenu;
    procedure LoadUIItemsFromComponentsTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  vMainForm: TMainForm;
  
implementation

{ TMainForm }

procedure TMainForm.HandleChangeTool(ASender: TObject);
begin
  if ASender = btnArrow then vDocument.CurrentTool := toolArrow
  else if ASender = btnComponent then vDocument.CurrentTool := toolComponent
  else if ASender = btnWire then vDocument.CurrentTool := toolWire
  else if ASender = btnText then vDocument.CurrentTool := toolText;

  vSchematics.UpdateAndRepaint;
end;

procedure TMainForm.HandleChooseNewComponentType(ASender: TObject);
begin
  vSchematics.NewComponentType := cmbComponents.ItemIndex + 1;
end;

procedure TMainForm.HandleClose(ASender: TObject);
begin
  Close;
end;

procedure TMainForm.HandleFileNew(ASender: TObject);
begin

end;

procedure TMainForm.HandleFileOpen(ASender: TObject);
begin
  if dialogOpen.Execute then vDocument.LoadFromFile(dialogOpen.FileName);
end;

procedure TMainForm.HandleFileSave(ASender: TObject);
begin
  if dialogSave.Execute then vDocument.SaveToFile(dialogSave.FileName);
end;

procedure TMainForm.HandleFileSaveAs(ASender: TObject);
begin

end;

procedure TMainForm.HandleShowAboutBox(ASender: TObject);
begin
  vAbout.ShowModal;
end;

procedure TMainForm.HandleShowComponentsEditor(ASender: TObject);
begin
  vComponentsEditor.ShowModal;

  { Reload any UI elements that may have changed }
  LoadUIItemsFromComponentsTable();
end;

procedure TMainForm.HandleShowDocumentOptions(ASender: TObject);
begin
  vDocumentOptions.ShowModal;
end;

procedure TMainForm.HandleUpdateSchematicsMousePos(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  pnlStatusBar.Panels.Items[ID_STATUS_MOUSEPOS].Text :=
   'X: ' + IntToStr(X) + ' Y: ' + IntToStr(Y);
end;

procedure TMainForm.HandleRecreateComponentsDatabaseClick(Sender: TObject);
begin

end;

{@@
  Translates the main menu of the application
}
procedure TMainForm.TranslateMainMenu;
begin
  { Main menu strings }
  mnuFile.Caption := vTranslations.lpFile;
  mnuDocument.Caption := vTranslations.lpDocument;
  mnuComponents.Caption := vTranslations.lpComponents;
  mnuHelp.Caption := vTranslations.lpHelp;

  { File menu strings }
  mnuFileNew.Caption := vTranslations.lpFileNew;
  mnuFileOpen.Caption := vTranslations.lpFileOpen;
  mnuFileSave.Caption := vTranslations.lpFileSave;
  mnuFileSaveAs.Caption := vTranslations.lpFileSaveAs;
  mnuFileExit.Caption := vTranslations.lpFileExit;

  { Document menu strings }
  mnuDocumentOptions.Caption := vTranslations.lpDocumentOptions;

  { Components menu strings }
  mnuRecreateComponentsDatabase.Caption := vTranslations.lpRecreateComponentsDatabase;
  mnuComponentsEditor.Caption := vTranslations.lpComponentsEditor;

  { Help Menu strings }
  mnuHelpHelp.Caption := vTranslations.lpHelp;
  mnuHelpAbout.Caption := vTranslations.lpHelpAbout;
end;

{@@
  Load user interface items which depend in the components table
}
procedure TMainForm.LoadUIItemsFromComponentsTable;
begin
  { Component selection combo box }
  vComponentsDatabase.FillStringListWithNames(cmbComponents.Items);
  cmbComponents.ItemIndex := 0;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Translations }

  TranslateMainMenu;

  { Load user interface items which depend in the components table }
  LoadUIItemsFromComponentsTable();

  { Schematics area }
  vSchematics := TSchematics.Create(Self);
  vSchematics.Parent := Self;
  vSchematics.Top := pnlToolbar.Height;
  vSchematics.Left := pnlTools.Width;
  vSchematics.Height := Height - pnlToolbar.Height - pnlStatusbar.Height;
  vSchematics.Width := Width - pnlTools.Width;
  vSchematics.Anchors := [akTop, akLeft, akBottom, akRight];
  
  vSchematics.OnUpdateMousePos := @HandleUpdateSchematicsMousePos;
  
  vSchematics.UpdateAndRepaint;

  { Necessary to make sure the keyboard events are correctly handled }
  OnKeyPress := @vSchematics.HandleKeyPress;
  pnlTools.OnKeyPress := @vSchematics.HandleKeyPress;
  pnlToolbar.OnKeyPress := @vSchematics.HandleKeyPress;
end;

destructor TMainForm.Destroy;
begin

  inherited Destroy;
end;

initialization
  {$I app.lrs}

end.

