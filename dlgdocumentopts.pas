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
    spinSheetHeight: TFloatSpinEdit;
    spinSheetWidth: TFloatSpinEdit;
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
  spinSheetHeight.Value := vDocument.Height;
  spinSheetWidth.Value := vDocument.Width;
end;

{@@
  Saves the new document data
}
procedure TDocumentOptions.SaveDataAndClose(Sender: TObject);
begin
  vDocument.Height := spinSheetHeight.Value;
  vDocument.Width := spinSheetWidth.Value;
  
  ModalResult := mrOk;
end;

initialization
  {$I dlgdocumentopts.lrs}

end.

