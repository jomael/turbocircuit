{
componentseditor.pas

Components editor window

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
unit dlgcomponentseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  DbCtrls, ExtCtrls, StdCtrls, db, SdfData,
  drawer, constants, tcsettings;

type

  { TvComponentsEditor }

  TvComponentsEditor = class(TForm)
    btnClose: TButton;
    btnPreview: TButton;
    FDatasetDRAWINGCODE1: TMemoField;
    FDatasetHEIGHT1: TLongintField;
    FDatasetID1: TLongintField;
    FDatasetNAMEEN1: TStringField;
    FDatasetNAMEPT1: TStringField;
    FDatasetPINS1: TLongintField;
    FDatasetWIDTH1: TLongintField;
    FDatasource: TDatasource;
    FDataset: TSdfDataSet;
    txtID: TDBEdit;
    txtNameEn: TDBEdit;
    txtWidth: TDBEdit;
    txtNamePt: TDBEdit;
    txtHeight: TDBEdit;
    txtPins: TDBEdit;
    memoDrawingCode: TDBMemo;
    FNavigator: TDBNavigator;
    imgPreview: TImage;
    lblID: TLabel;
    lblNameEn: TLabel;
    lblNamePt: TLabel;
    lblWidth: TLabel;
    lblDrawingCode: TLabel;
    lblPins: TLabel;
    lblHeight: TLabel;
    procedure btnPreviewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  vComponentsEditor: TvComponentsEditor;

implementation

{ TvComponentsEditor }

procedure TvComponentsEditor.btnPreviewClick(Sender: TObject);
begin
  { Clear the image area }
  imgPreview.Canvas.Brush.Color := clWhite;
  imgPreview.Canvas.FillRect(0, 0, imgPreview.Width, imgPreview.Height);

  { Draw a preview of the component }
  vItemsDrawer.DeltaX := 0;
  vItemsDrawer.DeltaY := 0;
  vItemsDrawer.DrawComponentFromStringList(imgPreview.Canvas, memoDrawingCode.Lines);
end;

procedure TvComponentsEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FDataset.Close;
end;

procedure TvComponentsEditor.FormShow(Sender: TObject);
begin
  FDataset.FileName := vConfigurations.ComponentsDBFile;
  FDataset.Active := True;
end;

initialization
  {$I dlgcomponentseditor.lrs}

end.

