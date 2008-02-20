unit dlgdocumentopts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin,
  document;

type

  { TDocumentOptions }

  TDocumentOptions = class(TForm)
    btnCancel: TButton;
    btnSave: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    spinSheetWidth: TSpinEdit;
    spinSheetHeight: TSpinEdit;
    procedure LoadData(Sender: TObject);
    procedure SaveDataAndClose(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  vDocumentOptions: TDocumentOptions;

implementation

{ TDocumentOptions }

{@@
  Loads current document data to the form
}
procedure TDocumentOptions.LoadData(Sender: TObject);
begin
  spinSheetHeight.Value := vDocument.SheetHeight;
  spinSheetWidth.Value := vDocument.SheetWidth;
end;

{@@
  Saves the new document data
}
procedure TDocumentOptions.SaveDataAndClose(Sender: TObject);
begin
  vDocument.SheetHeight := spinSheetHeight.Value;
  vDocument.SheetWidth := spinSheetWidth.Value;
  
  ModalResult := mrOk;
end;

initialization
  {$I dlgdocumentopts.lrs}

end.

