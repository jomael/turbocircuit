unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, ExtCtrls, ComCtrls;
  
type

  { TMainForm }

  TMainForm = class(TForm)
  private
    mnuMainMenu: TMainMenu;
    MyMenuItems: array[0..2] of TMenuItem;
    MySubMenuItems: array[0..30] of TMenuItem;
    pnlToolbar: TPanel;
    pnlTools: TPanel;
    procedure CreateMainMenu;
  public
    pnlStatusBar: TStatusBar;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  vMainForm: TMainForm;
  
implementation

uses drawer, dbcomponents, constants, translationstc, document;

{ TMainForm }

{*******************************************************************
*  TMainForm.CreateMainMenu ()
*
*  DESCRIPTION:    Creates the main menu of the application
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TMainForm.CreateMainMenu;
var
  i: Integer;
begin
  mnuMainMenu := TMainMenu.Create(Self);
  mnuMainMenu.Parent := Self;

  MyMenuItems[ID_MENU_FILE] := TMenuItem.Create(Self);
  MyMenuItems[ID_MENU_FILE].Caption := vTranslations.lpFile;

    MySubMenuItems[ID_MENU_FILE_NEW] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_NEW].Caption := vTranslations.lpFileNew;
//    MySubMenuItems[ID_MENU_FILE_NEW].OnClick := Translate;

    MySubMenuItems[ID_MENU_FILE_OPEN] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_OPEN].Caption := vTranslations.lpFileOpen;
//    MySubMenuItems[ID_MENU_FILE_OPEN].OnClick := Translate;

    MySubMenuItems[ID_MENU_FILE_SAVE] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_SAVE].Caption := vTranslations.lpFileSave;
//    MySubMenuItems[ID_MENU_FILE_SAVE].OnClick := Translate;

    MySubMenuItems[ID_MENU_FILE_SAVE_AS] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_SAVE_AS].Caption := vTranslations.lpFileSaveAs;
//    MySubMenuItems[ID_MENU_FILE_SAVE_AS].OnClick := Translate;

    MySubMenuItems[ID_MENU_FILE_SAVE_AS] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_SAVE_AS].Caption := lpSeparator;

    MySubMenuItems[ID_MENU_FILE_SAVE_AS] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_FILE_SAVE_AS].Caption := vTranslations.lpFileExit;
//    MySubMenuItems[ID_MENU_FILE_SAVE_AS].OnClick := Translate;

    for i := 0 to 3 do MyMenuItems[ID_MENU_FILE].Add(MySubMenuItems[i]);

  MyMenuItems[ID_MENU_COMPONENTS] := TMenuItem.Create(Self);
  MyMenuItems[ID_MENU_COMPONENTS].Caption := vTranslations.lpComponents;

    MySubMenuItems[ID_MENU_COMPONENTS_CHECK] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_COMPONENTS_CHECK].Caption := vTranslations.lpCheckComponentsDB;
//    MySubMenuItems[ID_MENU_COMPONENTS_CHECK].OnClick := Translate;

    MySubMenuItems[ID_MENU_COMPONENTS_EDIT] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_COMPONENTS_EDIT].Caption := vTranslations.lpEditComponents;
//    MySubMenuItems[ID_MENU_COMPONENTS_EDIT].OnClick := Translate;

    for i := 0 to 1 do MyMenuItems[ID_MENU_FILE].Add(MySubMenuItems[i]);

  MyMenuItems[ID_MENU_HELP] := TMenuItem.Create(Self);
  MyMenuItems[ID_MENU_HELP].Caption := vTranslations.lpHelp;

    MySubMenuItems[ID_MENU_HELP_HELP] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_HELP_HELP].Caption := vTranslations.lpHelp;
//    MySubMenuItems[ID_MENU_HELP_HELP].OnClick := Translate;

    MySubMenuItems[ID_MENU_HELP_SEPARATOR] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_HELP_SEPARATOR].Caption := lpSeparator;

    MySubMenuItems[ID_MENU_HELP_ABOUT] := TMenuItem.Create(Self);
    MySubMenuItems[ID_MENU_HELP_ABOUT].Caption := vTranslations.lpHelp;
//    MySubMenuItems[ID_MENU_HELP_ABOUT].OnClick := Translate;

    for i := 0 to 2 do MyMenuItems[ID_MENU_HELP].Add(MySubMenuItems[i]);

  for i := 0 to 2 do mnuMainMenu.Items.Add(MyMenuItems[i]);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Base setup }
  
  vComponentsDatabase := TComponentsDatabase.Create;
  vComponentsDatabase.LoadComponents;

  vDocument := TDocument.Create;

  { User Interface setup }

  CreateMainMenu;

  pnlToolbar := TPanel.Create(Self);
  pnlToolbar.Parent := Self;
  pnlToolbar.Top := 0;
  pnlToolbar.Left := 0;
  pnlToolbar.Height := 40;
  pnlToolbar.Width := Width;
  pnlToolbar.Anchors := [akTop, akLeft, akRight];

  pnlTools := TPanel.Create(Self);
  pnlTools.Parent := Self;
  pnlTools.Top := 40;
  pnlTools.Left := 0;
  pnlTools.Width := 120;
  pnlTools.Height := Height - 40;
  pnlTools.Anchors := [akTop, akLeft, akBottom];

  pnlStatusBar := TStatusBar.Create(Self);
  pnlStatusBar.Parent := Self;
  pnlStatusBar.Top := Height - 20;
  pnlStatusBar.Left := 120;
  pnlStatusBar.Height := 20;
  pnlStatusBar.Width := Width - pnlTools.Width;
  pnlStatusBar.Align := alNone;
  pnlStatusBar.Anchors := [akLeft, akBottom, akRight];

  pnlStatusBar.SimplePanel := False;
  pnlStatusBar.Panels.Add;
  pnlStatusBar.Panels.Add;
  pnlStatusBar.Panels.Items[ID_STATUS_MOUSEPOS].Width := 100;

  vDrawer := TDrawer.Create(Self);
  vDrawer.Parent := Self;
  vDrawer.Top := 40;
  vDrawer.Left := 120;
  vDrawer.Height := Height - 60;
  vDrawer.Width := Width - 120;
  vDrawer.Anchors := [akTop, akLeft, akBottom, akRight];
end;

destructor TMainForm.Destroy;
begin

  inherited Destroy;
end;

end.

