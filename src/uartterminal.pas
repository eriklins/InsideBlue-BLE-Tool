unit UartTerminal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ExtCtrls,
  SimpleBle, Util, Ble;

type
  { TTerminalForm }
  TTerminalForm = class(TForm)
    ButtonSend:            TButton;
    CheckBoxReceiveCR:      TCheckBox;
    CheckBoxSendCR:     TCheckBox;
    CheckBoxReceiveLF:      TCheckBox;
    CheckBoxSendLF:     TCheckBox;
    ComboBoxSendLine:       TComboBox;
    LabelHeader: TLabel;
    LabelLineEndingReceive: TLabel;
    LabelLineEndingSend:    TLabel;
    LabelMacAddress:        TLabel;
    MemoReceiveData:        TMemo;
    TextBoxDeviceName:      TEdit;
    procedure ButtonSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

  public

  end;

procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: TSimpleBleUuid; restore: TPanel);
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
    UuidService:  TSimpleBleUuid;
    ServiceName:  string;
    UuidRx:       TSimpleBleUuid;
    UuidTx:       TSimpleBleUuid;
    UuidModemIn:  TSimpleBleUuid;
    HaveModemIn:  Boolean;
    UuidModemOut: TSimpleBleUuid;
    HaveModemOut: Boolean;
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

  // unsubscribe from uart characteristics
  if SimpleBlePeripheralUnsubscribe(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidTx) = SIMPLEBLE_FAILURE then
    UtilLog('Unsubscribing from UART TX characteristic failed.')
  else
      UtilLog('Unsubscribed from UART TX characteristic.');
  if VspTerminal[i].HaveModemOut then
    if SimpleBlePeripheralUnsubscribe(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidModemOut) = SIMPLEBLE_FAILURE then
      UtilLog('Unsubscribing from UART ModemOut characteristic failed.')
    else
      UtilLog('Unsubscribed from UART ModemOut characteristic.');

  // clean up
  UtilSetNextFormLeft(TerminalForm[i], true);
  Delete(VspTerminal, i, 1);
  RestorePanel.Enabled := true;
end;


{ Button send line clicked }
procedure TTerminalForm.ButtonSendClick(Sender: TObject);
var
  i: Integer;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
  s: string;
  exist: Boolean;
begin
  SetLength(Buffer, CharDescMaxLength);

  s := TerminalForm[0].ComboBoxSendLine.Caption;

  if TerminalForm[0].CheckBoxSendCR.State = cbChecked then
    s := s + #13;
  if TerminalForm[0].CheckBoxSendLF.State = cbChecked then
    s := s + #10;

  ChLen := Length(s);
  for i := 0 to ChLen-1 do
    Buffer[i] := Byte(s[i+1]);
  ChData := PByte(Buffer);

  if SimpleBlePeripheralWriteCommand(VspTerminal[0].Handle, VspTerminal[0].UuidService, VspTerminal[0].UuidRx, ChData, ChLen) = SIMPLEBLE_FAILURE then begin
    ShowMessage('Failed to send data. Check TX max characters.');
    UtilLog('Failed to send uart terminal data.');
    exit;
  end;

  // append string to history if not existing
  if TerminalForm[0].ComboBoxSendLine.Items[0] = '' then
    TerminalForm[0].ComboBoxSendLine.Items.Clear;
  exist := false;
  for i := 0 to TerminalForm[0].ComboBoxSendLine.Items.Count-1 do
    if TerminalForm[0].ComboBoxSendLine.Items[i] = TerminalForm[0].ComboBoxSendLine.Caption then begin
      exist := true;
      break;
    end;
  if not exist then
    TerminalForm[0].ComboBoxSendLine.Items.Add(TerminalForm[0].ComboBoxSendLine.Caption);
end;


{ Callback function on notification from tx characteristic }
procedure UartTxOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
var
  i: Integer;
  c: Char;
begin
  for i := 0 to Len-1 do begin
    c := Char(Data[i]);
    if (c <> #13) and (c <> #10) then
      TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] := TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] + c
    else begin
      if (TerminalForm[0].CheckBoxReceiveCR.State = cbChecked) and (c = #13) then
        TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] := TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] + LineEnding;
      if (TerminalForm[0].CheckBoxReceiveLF.State = cbChecked) and (c = #10) then
        TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] := TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] + LineEnding;
    end;
  end;
end;


{ Callback function on notification from tx characteristic }
procedure UartModemOutOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
begin

end;


{ Start a vsp uart terminal to the device with given peripheral handle and service uuid }
procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: TSimpleBleUuid; restore: TPanel);
var
  i, j: Integer;
begin

  // need to limit to one terminal due to missing support in SimpleBle library
  if Length(VspTerminal) = 1 then begin
    ShowMessage('Currently only one uart terminal is supported.');
    exit;
  end;

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
      if CompareChar(SvUuid.Value, VspServiceUuids[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0 then begin
        VspTerminal[i].ServiceName  := VspServiceUuids[j].Name;
        VspTerminal[i].UuidRx       := VspCharacteristicUuids[VspServiceUuids[j].ChRx].Uuid;
        VspTerminal[i].UuidTx       := VspCharacteristicUuids[VspServiceUuids[j].ChTx].Uuid;
        if VspServiceUuids[j].ChModemIn < 0 then  // modem characteristics are optional
          VspTerminal[i].HaveModemIn := false
        else begin
          VspTerminal[i].HaveModemIn := True;
          VspTerminal[i].UuidModemIn  := VspCharacteristicUuids[VspServiceUuids[j].ChModemOut].Uuid;
        end;
        if VspServiceUuids[j].ChModemOut < 0 then  // modem characteristics are optional
          VspTerminal[i].HaveModemOut := false
        else begin
          VspTerminal[i].HaveModemOut := True;
          VspTerminal[i].UuidModemOut  := VspCharacteristicUuids[VspServiceUuids[j].ChModemOut].Uuid;
        end;
      end;
  end;

  // create the form
  Application.CreateForm(TTerminalForm, TerminalForm[i]);
  TerminalForm[i].Tag := i;
  TerminalForm[i].Top := UtilGetNextFormTop();
  TerminalForm[i].Left := UtilGetNextFormLeft();
  if VspTerminal[i].DeviceName = '' then begin
    UtilLog('Open UART terminal: "<unknown name>" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName);
    TerminalForm[i].Caption := '"<unknown name>" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end else begin
      UtilLog('Open UART terminal: "' + VspTerminal[i].DeviceName + '" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName);
      TerminalForm[i].Caption := '"' + VspTerminal[i].DeviceName + '" [' + VspTerminal[i].MacAddress + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end;
  TerminalForm[i].TextBoxDeviceName.Caption := VspTerminal[i].DeviceName;
  TerminalForm[i].LabelMacAddress.Caption := 'MAC Address [' + VspTerminal[i].MacAddress + ']';
  TerminalForm[i].Show;

  // subscribe to uart tx characteristic notifications
  if SimpleBlePeripheralNotify(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidTx, @UartTxOnNotify, Nil) = SIMPLEBLE_FAILURE then begin
    UtilLog('Subscribing to UART TX characteristic failed.');
    ShowMessage('Cannot subsribe to UART TX characteristic.');
    TerminalForm[i].Close;
    Exit;
  end;
  UtilLog('Subscribed to UART TX characteristic.');

  // subscribe to uart ModemOut characteristic notifications
  if VspTerminal[i].HaveModemOut then begin
    if SimpleBlePeripheralNotify(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidModemOut, @UartModemOutOnNotify, Nil) = SIMPLEBLE_FAILURE then begin
      UtilLog('Subscribing to UART ModemOut characteristic failed.');
      ShowMessage('Cannot subsribe to UART ModemOut characteristic.');
      SimpleBlePeripheralUnsubscribe(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidTx);
      TerminalForm[i].Close;
      Exit;
    end;
    UtilLog('Subscribed to UART ModemOut characteristic');
  end;

  UtilSetNextFormTop(TerminalForm[i]);
  UtilSetNextFormLeft(TerminalForm[i]);

  TerminalForm[0].ComboBoxSendLine.Items.Clear;
  TerminalForm[0].ComboBoxSendLine.Items.Add('');

  TerminalForm[0].MemoReceiveData.Clear;
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

