unit UartTerminal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Ble, Util;

type

  { TTerminalForm }
  TTerminalForm = class(TForm)
    ButtonSend: TButton;
    CheckBoxCr: TCheckBox;
    CheckBoxLf: TCheckBox;
    CheckBoxHexAscii: TCheckBox;
    ComboBox1: TComboBox;
    LabelAppend: TLabel;
    LabelMacAddress: TLabel;
    MemoReceiveData: TMemo;
    TextBoxDeviceName: TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

  public

  end;


procedure UartTerminalStart(DeIdx: Integer; SvIdx: Integer);

var
  TerminalForm: array of TTerminalForm;

implementation

{$R *.lfm}

uses Connect;  // not best practice, but need access to ScanForm for log output and some connected device data...


{ Start a vsp uart terminal to the device DeIdx on service SvIdx }
procedure UartTerminalStart(DeIdx: Integer; SvIdx: Integer);
var
  i: Integer;
begin
  BleConnectData[DeIdx].VspTerminal[SvIdx].IsActive := true;
  // extend form array and create form
  i := Length(TerminalForm);
  SetLength(TerminalForm, i+1);
  Application.CreateForm(TTerminalForm, TerminalForm[i]);
  TerminalForm[i].Tag := (DeIdx shl TagPosDev) or (SvIdx shl TagPosSrv) or i;
  TerminalForm[i].Top := UtilGetNextFormTop;
  TerminalForm[i].Left := UtilGetNextFormLeft;
  if BleConnectData[DeIdx].DeviceName = '' then begin
    UtilLog('Open uart terminal: "<unknown name>" [' + BleConnectData[DeIdx].MacAddress + '] - ' + BleConnectData[DeIdx].VspTerminal[SvIdx].ServiceName);
    TerminalForm[i].Caption := '"<unknown name>" [' + BleConnectData[DeIdx].MacAddress + '] - ' +
                               BleConnectData[DeIdx].VspTerminal[SvIdx].ServiceName + ' - Virtual Uart Terminal';
  end else begin
    UtilLog('Open uart terminal: ' + BleConnectData[DeIdx].DeviceName + ' [' + BleConnectData[DeIdx].MacAddress + '] - ' + BleConnectData[DeIdx].VspTerminal[SvIdx].ServiceName);
    TerminalForm[i].Caption := BleConnectData[DeIdx].DeviceName + ' [' + BleConnectData[DeIdx].MacAddress + '] - ' +
                             BleConnectData[DeIdx].VspTerminal[SvIdx].ServiceName + ' - Virtual Uart Terminal';
  end;
  TerminalForm[i].TextBoxDeviceName.Caption := BleConnectData[DeIdx].DeviceName;
  TerminalForm[i].LabelMacAddress.Caption := 'MAC Address [' + BleConnectData[DeIdx].MacAddress + ']';
  TerminalForm[i].Show;
  UtilSetNextFormTop(TerminalForm[i]);
  UtilSetNextFormLeft(TerminalForm[i]);
end;


{ Close vsp terminal }
procedure TTerminalForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i, FoIdx, DeIdx, SvIdx: Integer;
begin
  i := TForm(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  FoIdx := (i and $ff);
  UtilSetNextFormLeft(TerminalForm[Length(TerminalForm)-1], true);

  Delete(TerminalForm, FoIdx, 1);
  BleConnectData[DeIdx].VspTerminal[SvIdx].IsActive := false;
  ConnectRestoreVspPanel(DeIdx, SvIdx);
end;


end.

