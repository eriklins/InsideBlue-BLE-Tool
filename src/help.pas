unit Help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, RichMemo;

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
  procedure HelpShow();

var
  HelpForm: THelpForm;
  HelpFileExist: Boolean;
  HelpFilePath: String;

implementation

{$R *.lfm}

uses Main;


{ THelpForm }

{ Init, check if help file exist }
function HelpInit(): Boolean;
begin

  HelpFileExist := true;
  HelpFilePath := '';
  if not FileExists(HelpFilePath + 'help.rtf') then begin
    HelpFilePath := '..\..\help\';
    if not FileExists(HelpFilePath + 'help.rtf') then begin
      ScanForm.LogOutput.Append('Could not open help file.');
      HelpFileExist := false;
    end;
  end;
  Result := HelpFileExist;
end;


{ Show the help form }
procedure HelpShow();
begin
  HelpForm.Top := ScanForm.Top;
  HelpForm.Left := NextFormLeftCoordinate;
  HelpForm.Show;
  NextFormLeftCoordinate := NextFormLeftCoordinate + HelpForm.Width + NextFormMargin;
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
  if NextFormLeftCoordinate <= HelpForm.Left + HelpForm.Width + NextFormMargin then
    NextFormLeftCoordinate := NextFormLeftCoordinate - (HelpForm.Width + NextFormMargin);
  ScanForm.ButtonHelp.Enabled := true;
end;


end.

