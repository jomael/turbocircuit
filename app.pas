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
  ActnList, Buttons, StdCtrls, Dialogs, Graphics,
  schematics, dbcomponents, constants, translationstc, document,
  dlgcomponentseditor, dlgabout, dlgdocumentopts;

type

  { TMainForm }

  TMainForm = class(TForm)
    actExportPng: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileNew: TAction;
    actFileExit: TAction;
    FMainFormAction: TActionList;
    btnText: TSpeedButton;
    cmbComponents: TComboBox;
    listActionImages: TImageList;
    lblComponentType: TLabel;
    mnuExportPng: TMenuItem;
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
    barFileToolbar: TToolBar;
    ToolButton1: TToolButton;
    toolSeparator1: TToolButton;
    toolOpen: TToolButton;
    toolSave: TToolButton;
    procedure actExportPngExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure HandleChangeTool(ASender: TObject);
    procedure HandleChooseNewComponentType(ASender: TObject);
    procedure HandleRecreateComponentsDatabaseClick(Sender: TObject);
    procedure HandleShowAboutBox(ASender: TObject);
    procedure HandleShowComponentsEditor(ASender: TObject);
    procedure HandleShowDocumentOptions(ASender: TObject);
    procedure HandleUpdateSchematicsMousePos(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    function  ShowDialogDiscartChanges: Boolean;
    procedure TranslateMainMenu;
    procedure TranslateActions;
    procedure TranslateUserInterface;
    procedure LoadUIItemsFromComponentsTable;
    procedure FillDocumentUIElements(Sender: TObject);
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

  { Updates the tool notebook }
  case vDocument.CurrentTool of
   toolArrow: FToolsNotebook.ActivePageComponent := FToolsNotebook.Page[INT_TOOLSNOTEBOOK_ARROW];
   toolComponent: FToolsNotebook.ActivePageComponent := FToolsNotebook.Page[INT_TOOLSNOTEBOOK_COMPONENTS];
   toolWire: FToolsNotebook.ActivePageComponent := FToolsNotebook.Page[INT_TOOLSNOTEBOOK_WIRE];
   toolText: FToolsNotebook.ActivePageComponent := FToolsNotebook.Page[INT_TOOLSNOTEBOOK_TEXT];
  end;

  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.actExportPngExecute(Sender: TObject);
var
  PngImage: TPortableNetworkGraphic;
begin
  dialogSave.Filter := vTranslations.lpSavePngFilter;
  if dialogSave.Execute then
  begin
    PngImage := TPortableNetworkGraphic.Create;
    try
      PngImage.Height := vDocument.SheetHeight;
      PngImage.Width := vDocument.SheetWidth;

      vSchematics.DrawToCanvas(PngImage.Canvas, False);
      PngImage.SaveToFile(dialogSave.FileName);
    finally
      PngImage.Free;
    end;
  end;
end;

procedure TMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actFileNewExecute(Sender: TObject);
begin
  if vDocument.Modified then if not ShowDialogDiscartChanges() then Exit;

  vDocument.Clear;
  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.actFileOpenExecute(Sender: TObject);
begin
  if vDocument.Modified then if not ShowDialogDiscartChanges() then Exit;

  dialogOpen.Filter := vTranslations.lpSaveDiagramFilter;
  if dialogOpen.Execute then
  begin
    vDocument.LoadFromFile(dialogOpen.FileName);
    vSchematics.UpdateAndRepaint(nil);
  end;
end;

procedure TMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  dialogSave.Filter := vTranslations.lpSaveDiagramFilter;
  if dialogSave.Execute then vDocument.SaveToFile(dialogSave.FileName);
end;

procedure TMainForm.actFileSaveExecute(Sender: TObject);
begin
  if vDocument.Saved then vDocument.SaveToFile(vDocument.FileName)
  else actFileSaveAsExecute(Sender);
end;

procedure TMainForm.HandleChooseNewComponentType(ASender: TObject);
begin
  vSchematics.NewComponentType := vComponentsDatabase.IndexToID(cmbComponents.ItemIndex + 1);
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

function TMainForm.ShowDialogDiscartChanges: Boolean;
begin
  Result := MessageDlg(
   'There are unsaved changes, would you like '
   + 'go ahead with this operation and discart them?',
   mtConfirmation,
   [mbYes, mbNo],
   0) = mrYes;
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

  { Document menu strings }
  mnuDocumentOptions.Caption := vTranslations.lpDocumentOptions;

  { Components menu strings }
  mnuRecreateComponentsDatabase.Caption := vTranslations.lpRecreateComponentsDatabase;
  mnuComponentsEditor.Caption := vTranslations.lpComponentsEditor;

  { Help Menu strings }
  mnuHelpHelp.Caption := vTranslations.lpHelp;
  mnuHelpAbout.Caption := vTranslations.lpHelpAbout;
end;

procedure TMainForm.TranslateActions;
begin
  { File menu action strings }
  actFileNew.Caption := vTranslations.lpFileNew;
  actFileNew.Hint := vTranslations.lpFileNew;
  actFileOpen.Caption := vTranslations.lpFileOpen;
  actFileOpen.Hint := vTranslations.lpFileOpen;
  actFileSave.Caption := vTranslations.lpFileSave;
  actFileSave.Hint := vTranslations.lpFileSave;
  actFileSaveAs.Caption := vTranslations.lpFileSaveAs;
  actFileSaveAs.Hint := vTranslations.lpFileSaveAs;
  actExportPng.Caption := vTranslations.lpFileExportPng;
  actExportPng.Hint := vTranslations.lpFileExportPng;
  actFileExit.Caption := vTranslations.lpFileExit;
  actFileExit.Hint := vTranslations.lpFileExit;
end;

{@@
  Translates the user interface of TMainForm
}
procedure TMainForm.TranslateUserInterface;
begin
  TranslateMainMenu;
  TranslateActions;

  lblComponentType.Caption := vTranslations.lpMainFormChooseComponent;
end;

{@@
  Load user interface items which depend in the components table
}
procedure TMainForm.LoadUIItemsFromComponentsTable;
begin
  { Component selection combo box }
  vComponentsDatabase.FillStringListWithNames(cmbComponents.Items);
  cmbComponents.ItemIndex := 0;
  vSchematics.NewComponentType := vComponentsDatabase.IndexToID(1);
end;

{
  Makes changes in the user interface to reflect the state of the document
}
procedure TMainForm.FillDocumentUIElements(Sender: TObject);
begin
  { Make sure this routine will be called allays when Saved or Title changes }
  vDocument.UIChangeCallback := @FillDocumentUIElements;

  { Window title }
  if vDocument.Saved then
    Caption := szAppTitle + ' - ' + vDocument.Title
  else
    Caption := szAppTitle + ' - ' + vTranslations.lpUntitled;

  if vDocument.Modified then Caption := Caption + '*';
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Translations }

  TranslateUserInterface();

  { Fill other user interface elements which depend on the document state }

  FillDocumentUIElements(Self);

  { Schematics area }
  vSchematics := TSchematics.Create(Self);
  vSchematics.Parent := Self;
  vSchematics.Top := pnlToolbar.Height;
  vSchematics.Left := pnlTools.Width;
  vSchematics.Height := Height - pnlToolbar.Height - pnlStatusbar.Height;
  vSchematics.Width := Width - pnlTools.Width;
  vSchematics.Anchors := [akTop, akLeft, akBottom, akRight];
  
  vSchematics.OnUpdateMousePos := @HandleUpdateSchematicsMousePos;
  
  vSchematics.UpdateAndRepaint(nil);

  { Necessary to make sure the keyboard events are correctly handled }
  OnKeyPress := @vSchematics.HandleKeyPress;
  pnlTools.OnKeyPress := @vSchematics.HandleKeyPress;
  pnlToolbar.OnKeyPress := @vSchematics.HandleKeyPress;

  { Load user interface items which depend in the components table }
  LoadUIItemsFromComponentsTable();
end;

destructor TMainForm.Destroy;
begin

  inherited Destroy;
end;

initialization
  {$I app.lrs}

end.

