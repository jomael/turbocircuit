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
  // RTL, FCL, LCL
  Classes, SysUtils, LResources, Forms, Controls, Menus, ExtCtrls, ComCtrls,
  ActnList, Buttons, StdCtrls, Dialogs, Graphics, EditBtn, Spin,
  // TC units
  schematics, dbcomponents, constants, translationstc, document,
  dlgcomponentseditor, dlgabout, dlgdocumentopts,
  toolscode,
  // fpvectorial
  fpvectorial;

type

  { TMainForm }

  TMainForm = class(TForm)
    actExportPng: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileNew: TAction;
    actFileExit: TAction;
    btnArrow: TSpeedButton;
    btnArrow3: TSpeedButton;
    btnArrow2: TSpeedButton;
    btnComponent: TSpeedButton;
    btnEllipse: TSpeedButton;
    btnPolyline: TSpeedButton;
    btnRasterImage: TSpeedButton;
    btnRectangle: TSpeedButton;
    btnText: TSpeedButton;
    btnWire: TSpeedButton;
    btnRenderPostScript: TButton;
    checkShowGrid: TCheckBox;
    checkSnapToGrid: TCheckBox;
    comboMode: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    memoTextTool: TMemo;
    memoEPS: TMemo;
    notebookTools: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
    Panel1: TPanel;
    spinPolylineWidth: TSpinEdit;
    spinImageProportion: TSpinEdit;
    spinZoom: TFloatSpinEdit;
    ToolBar1: TToolBar;
    txtRasterImage: TFileNameEdit;
    FMainFormAction: TActionList;
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
    NotebookSubTools: TNotebook;
    dialogOpen: TOpenDialog;
    pageArrow: TPage;
    pageComponent: TPage;
    pageWire: TPage;
    pageText: TPage;
    pagePolyline: TPage;
    pageRasterImage: TPage;
    pnlToolbar: TPanel;
    pnlTools: TPanel;
    pnlStatusBar: TStatusBar;
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
    procedure btnRenderPostScriptClick(Sender: TObject);
    procedure checkShowGridChange(Sender: TObject);
    procedure checkSnapToGridChange(Sender: TObject);
    procedure comboModeSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HandleChangeTool(ASender: TObject);
    procedure HandleChooseNewComponentType(ASender: TObject);
    procedure HandleRecreateComponentsDatabaseClick(Sender: TObject);
    procedure HandleShowAboutBox(ASender: TObject);
    procedure HandleShowComponentsEditor(ASender: TObject);
    procedure HandleShowDocumentOptions(ASender: TObject);
    procedure HandleUpdateSchematicsMousePos(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure memoTextToolChange(Sender: TObject);
    procedure spinImageProportionChange(Sender: TObject);
    procedure spinPolylineWidthChange(Sender: TObject);
    procedure spinZoomChange(Sender: TObject);
    procedure txtRasterImageAcceptFileName(Sender: TObject; var Value: String);
  private
    function  ShowDialogDiscartChanges: Boolean;
    procedure TranslateMainMenu;
    procedure TranslateActions;
    procedure TranslatePanels;
    procedure TranslateUserInterface;
    procedure LoadUIItemsFromComponentsTable;
    procedure FillDocumentUIElements(Sender: TObject);
    procedure UpdateNotebookPage(ATool: TCTool);
    procedure AppToDocumentUpdate();
    procedure DocumentToAppUpdate(Sender: TObject);
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
  if (ASender = btnArrow) or (ASender = btnArrow2) or (ASender = btnArrow3) then vDocument.CurrentTool := toolArrow
  else if ASender = btnComponent then vDocument.CurrentTool := toolComponent
  else if ASender = btnWire then vDocument.CurrentTool := toolWire
  else if ASender = btnText then vDocument.CurrentTool := toolText
  else if ASender = btnPolyline then vDocument.CurrentTool := toolPolyline
  else if ASender = btnRasterImage then vDocument.CurrentTool := toolRasterImage
  else if ASender = btnEllipse then vDocument.CurrentTool := toolEllipse
  else if ASender = btnRectangle then vDocument.CurrentTool := toolRectangle;

  { Updates the tool notebook }
  UpdateNotebookPage(vDocument.CurrentTool);

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
      PngImage.Height := Round(vDocument.Height);
      PngImage.Width := Round(vDocument.Width);

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
    vDocument.ReadFromFile(dialogOpen.FileName);
    vSchematics.UpdateAndRepaint(nil);
  end;
end;

procedure TMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  dialogSave.Filter := vTranslations.lpSaveDiagramFilter;
  if dialogSave.Execute then vDocument.WriteToFile(dialogSave.FileName);
end;

procedure TMainForm.actFileSaveExecute(Sender: TObject);
begin
  if vDocument.Saved then vDocument.WriteToFile(vDocument.FileName)
  else actFileSaveAsExecute(Sender);
end;

procedure TMainForm.btnRenderPostScriptClick(Sender: TObject);
begin
  vDocument.ReadFromStrings(memoEPS.Lines, vfEncapsulatedPostScript);
//  AppToDocumentUpdate();
  DocumentToAppUpdate(nil);
  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.checkShowGridChange(Sender: TObject);
begin
  AppToDocumentUpdate();
  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.checkSnapToGridChange(Sender: TObject);
begin
  AppToDocumentUpdate();
end;

procedure TMainForm.comboModeSelect(Sender: TObject);
begin
  notebookTools.PageIndex := comboMode.ItemIndex;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if vDocument.Modified then CanClose := ShowDialogDiscartChanges()
  else CanClose := True;
end;

procedure TMainForm.HandleChooseNewComponentType(ASender: TObject);
begin
  vToolsDelegate.NewComponentType := vComponentsDatabase.IndexToID(cmbComponents.ItemIndex + 1);
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

procedure TMainForm.memoTextToolChange(Sender: TObject);
var
  lText: TvText;
begin
  if vDocument.SelectedvElement is TvText then
  begin
    lText := vDocument.SelectedvElement as TvText;
    lText.Value := memoTextTool.Text;
    vSchematics.UpdateAndRepaint(nil);
  end;
end;

procedure TMainForm.spinImageProportionChange(Sender: TObject);
begin
  if not (vDocument.SelectedElementType = toolRasterImage) then Exit;

  vDocument.GetSelectedRasterImage^.Proportion := spinImageProportion.Value / 100;
  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.spinPolylineWidthChange(Sender: TObject);
begin
  if not (vDocument.SelectedElementType = toolPolyline) then Exit;

  vDocument.GetSelectedPolyline^.Width := spinPolylineWidth.Value;

  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.spinZoomChange(Sender: TObject);
begin
  if spinZoom.Value <= 2 then spinZoom.Increment := 0.1
  else if spinZoom.Value <= 10 then spinZoom.Increment := 1
  else if spinZoom.Value <= 50 then spinZoom.Increment := 10
  else spinZoom.Increment := 50;

  AppToDocumentUpdate();

  vSchematics.UpdateAndRepaint(nil);
end;

procedure TMainForm.txtRasterImageAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if not (vDocument.SelectedElementType = toolRasterImage) then Exit;

  if vDocument.GetSelectedRasterImage^.ImageData <> nil then
    FreeAndNil(vDocument.GetSelectedRasterImage^.ImageData);

  vDocument.GetSelectedRasterImage^.ImageData := TPicture.Create;
  vDocument.GetSelectedRasterImage^.ImageData.LoadFromFile(Value);

  vSchematics.UpdateAndRepaint(nil);
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

procedure TMainForm.TranslatePanels;
begin
  { Arrow page }
  pageArrow.Caption := vTranslations.lpMainFormArrow;

  { Component page }
  pageComponent.Caption := vTranslations.lpMainFormComponent;
  lblComponentType.Caption := vTranslations.lpMainFormChooseComponent;

  { Wire page }
  pageWire.Caption := vTranslations.lpMainFormWire;

  { Text page }
  pageText.Caption := vTranslations.lpMainFormText;

  { Polyline page }
  pagePolyline.Caption := vTranslations.lpMainFormPolyline;

  { Raster Image page }
  pageRasterImage.Caption := vTranslations.lpMainFormRasterImage;
end;

{@@
  Translates the user interface of TMainForm
}
procedure TMainForm.TranslateUserInterface;
begin
  TranslateMainMenu;
  TranslateActions;
  TranslatePanels;
end;

{@@
  Load user interface items which depend in the components table
}
procedure TMainForm.LoadUIItemsFromComponentsTable;
begin
  { Component selection combo box }
  vComponentsDatabase.FillStringListWithNames(cmbComponents.Items);
  cmbComponents.ItemIndex := 0;
  vToolsDelegate.NewComponentType := vComponentsDatabase.IndexToID(1);
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

  { If something is selected, show the appropriate page }
  if vDocument.IsSomethingSelected then
    UpdateNotebookPage(vDocument.SelectedElementType);
end;

procedure TMainForm.UpdateNotebookPage(ATool: TCTool);
begin
  case ATool of
   toolArrow: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_ARROW;
   toolComponent: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_COMPONENTS;
   toolWire: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_WIRE;
   toolText: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_TEXT;
   toolPolyline: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_POLYLINE;
   toolRasterImage: NotebookSubTools.PageIndex := INT_TOOLSNOTEBOOK_RASTERIMAGE;
  end;
end;

procedure TMainForm.AppToDocumentUpdate();
begin
  // Zoom
  vDocument.ZoomLevel := spinZoom.Value / 100;
  //INT_SHEET_GRID_SPACING := Round(INT_SHEET_DEFAULT_GRID_SPACING * FLOAT_SHEET_GRID_PROPORTION);
  //INT_SHEET_GRID_HALFSPACING := INT_SHEET_GRID_SPACING div 2;

  // Grid
  vDocument.SnapToGrid := checkSnapToGrid.Checked;
  vDocument.ShowGrid := checkShowGrid.Checked;
end;

procedure TMainForm.DocumentToAppUpdate(Sender: TObject);
var
  lText: TvText;
begin
  spinZoom.Value := vDocument.ZoomLevel * 100;

  if vDocument.SelectedvElement is TvText then
  begin
    lText := vDocument.SelectedvElement as TvText;
    memoTextTool.Text := lText.Value;
  end;
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

  // Connect the schematics with it's delegate
  vSchematics.Delegate := vToolsDelegate;
  vToolsDelegate.Owner := vSchematics;

  vToolsDelegate.OnUpdateMousePos := @HandleUpdateSchematicsMousePos;
  vToolsDelegate.OnUpdateAppInterface := @DocumentToAppUpdate;

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

