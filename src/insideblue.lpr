program InsideBlue;

{$mode objfpc}{$H+}
{$APPTYPE GUI}

{$UNDEF DYNAMIC_LOADING}
{$IFDEF WINDOWS}
  {$DEFINE DYNAMIC_LOADING}    { UNCOMMENT IF YOU WANT DYNAMIC LOADING }
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, Main, SimpleBle;

{$R *.res}


begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  {$IFDEF DYNAMIC_LOADING}
  if not SimpleBleLoadLibrary() then begin
    ShowMessage('Failed to load SimpleBLE library.');
    Halt;
  end;
  {$ENDIF}

  Application.CreateForm(TScanForm, ScanForm);
  Application.Run;
end.

