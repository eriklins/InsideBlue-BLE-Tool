unit Help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, RichMemo,
  Util;

type

  { THelpForm }

  THelpForm = class(TForm)
    RichMemoHelp: TRichMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

  function HelpInit(): Boolean;
  procedure HelpShow(b: TButton);

var
  HelpForm: THelpForm;
  HelpFileExist: Boolean;
  HelpFilePath: String;
  RestoreButton: TButton;


implementation

{$R *.lfm}


{ THelpForm }

{ Init, check if help file exist }
function HelpInit(): Boolean;
begin

  HelpFileExist := true;
  HelpFilePath := '';  // first look into local folder
  if not FileExists(HelpFilePath + 'help.rtf') then begin
    HelpFilePath := 'help\';  // this is relative path when started from within Lazarus IDE
    if not FileExists(HelpFilePath + 'help.rtf') then begin
      UtilLog('Could not open help file.');
      HelpFileExist := false;
    end;
  end;
  Result := HelpFileExist;
end;


{ Show the help form }
procedure HelpShow(b: TButton);
begin
  Application.CreateForm(THelpForm, HelpForm);
  HelpForm.Top := UtilGetNextFormTop;
  HelpForm.Left := UtilGetNextFormLeft(HelpForm);
  HelpForm.Show;
  UtilSetNextFormTop(HelpForm);
  UtilSetNextFormLeft(HelpForm);
  RestoreButton := b;
end;


{ Create form and load rich text file into RichMemo }
procedure THelpForm.FormCreate(Sender: TObject);
var
  FS: TFileStream;
begin
  if HelpFileExist then begin
    FS := TFileStream.Create(HelpFilePath + 'help.rtf', fmOpenRead or fmShareDenyNone);
    try
      RichMemoHelp.LoadRichText(FS);
    finally
      FS.Free;
    end;
  end;
end;


{ On close help window we reset the coordinate for the next window and re-enable the help button }
procedure THelpForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UtilSetNextFormLeft(HelpForm, true);
  RestoreButton.Enabled := true;
end;


end.

