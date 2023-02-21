unit UartTerminal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ExtCtrls,
  SimpleBle, Util, Ble;

type
  { TTerminalForm }
  TTerminalForm = class(TForm)
    ButtonSend: TButton;
    CheckBoxReceiveCR: TCheckBox;
    CheckBoxReceiveCR1: TCheckBox;
    CheckBoxReceiveLF: TCheckBox;
    CheckBoxHexAscii: TCheckBox;
    CheckBoxReceiveLF1: TCheckBox;
    ComboBox1: TComboBox;
    LabelLineEndingReceive: TLabel;
    LabelLineEndingSend: TLabel;
    LabelMacAddress: TLabel;
    MemoReceiveData: TMemo;
    TextBoxDeviceName: TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

  public

  end;

procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: string; restore: TPanel);
function  UartTerminalIsActive(PerHandle: TSimpleBlePeripheral): Boolean;

var
  TerminalForm: array of TTerminalForm;


implementation

{$R *.lfm}

type
  TBleVspTerminal = record
    Handle:       TSimpleBlePeripheral;
    DeviceName:   string;
    MacAddress:   string;
    UuidService:  string;
    ServiceName:  string;
    UuidRx:       string;
    UuidTx:       string;
    UuidModemIn:  string;
    UuidModemOut: string;
    IsActive:     Boolean;
  end;

var
  VspTerminal: array of TBleVspTerminal;
  RestorePanel: TPanel;


{ TTerminalForm }

{ Close vsp terminal }
procedure TTerminalForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  i := TForm(Sender).Tag;
  UtilSetNextFormLeft(TerminalForm[i], true);
  Delete(VspTerminal, i, 1);
  RestorePanel.Enabled := true;
end;


{ Start a vsp uart terminal to the device with given peripheral handle and service uuid }
procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: string; restore: TPanel);
var
  i, j: Integer;
begin

  // increment terminals and increase some arrays
  i := Length(VspTerminal);
  SetLength(VspTerminal, i+1);
  SetLength(TerminalForm, i+1);

  VspTerminal[i].Handle      := PerHandle;
  VspTerminal[i].UuidService := SvUuid;
  VspTerminal[i].DeviceName  := DevName;
  VspTerminal[i].MacAddress  := MacAddr;
  VspTerminal[i].IsActive    := true;
  RestorePanel := restore;

  // search rx/tx/modem uart characteristics of service
  for j := 0 to Length(VspServiceUuids)-1 do begin
      if SvUuid = VspServiceUuids[j].Uuid then begin
        VspTerminal[i].ServiceName  := VspServiceUuids[j].Name;
        VspTerminal[i].UuidRx       := VspCharacteristicUuids[VspServiceUuids[j].ChRx].Uuid;
        VspTerminal[i].UuidTx       := VspCharacteristicUuids[VspServiceUuids[j].ChTx].Uuid;
        if VspServiceUuids[j].ChModemIn < 0 then  // modem characteristics are optional
          VspTerminal[i].UuidModemIn := ''
        else
          VspTerminal[i].UuidModemIn  := VspCharacteristicUuids[VspServiceUuids[j].ChModemIn].Uuid;
        if VspServiceUuids[j].ChModemOut < 0 then  // modem characteristics are optional
          VspTerminal[i].UuidModemOut := ''
        else
          VspTerminal[i].UuidModemOut := VspCharacteristicUuids[VspServiceUuids[j].ChModemOut].Uuid;
      end;
  end;
  //UtilLog('VSP: ' + VspTerminal[i].UuidService);
  //UtilLog('     RX      : ' + VspTerminal[i].UuidRx);
  //UtilLog('     TX      : ' + VspTerminal[i].UuidTx);
  //UtilLog('     ModemIn : ' + VspTerminal[i].UuidModemIn);
  //UtilLog('     ModemOut: ' + VspTerminal[i].UuidModemOut);

  // create the form
  Application.CreateForm(TTerminalForm, TerminalForm[i]);
  TerminalForm[i].Tag := i;
  TerminalForm[i].Top := UtilGetNextFormTop();
  TerminalForm[i].Left := UtilGetNextFormLeft();
  if VspTerminal[i].DeviceName = '' then begin
    UtilLog('Open uart terminal: "<unknown name>" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName);
    TerminalForm[i].Caption := '"<unknown name>" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end else begin
      UtilLog('Open uart terminal: "' + VspTerminal[i].DeviceName + '" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName);
      TerminalForm[i].Caption := '"' + VspTerminal[i].DeviceName + '" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end;
  TerminalForm[i].TextBoxDeviceName.Caption := VspTerminal[i].DeviceName;
  TerminalForm[i].LabelMacAddress.Caption := 'MAC Address [' + VspTerminal[i].MacAddress + ']';
  TerminalForm[i].Show;

  UtilSetNextFormTop(TerminalForm[i]);
  UtilSetNextFormLeft(TerminalForm[i]);
end;


{ Check if there is a vsp terminal active for given device }
function UartTerminalIsActive(PerHandle: TSimpleBlePeripheral): Boolean;
var
  i: Integer;
begin
  Result := false;
  i := 0;
  while i < Length(VspTerminal) do begin
    if (VspTerminal[i].Handle = PerHandle) and (VspTerminal[i].IsActive) then begin
      Result := true;
      Exit;
    end;
    Inc(i);
  end;
end;


end.

