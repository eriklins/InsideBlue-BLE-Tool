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
    CheckBoxReceiveJson: TCheckBox;
    CheckBoxSendCR:     TCheckBox;
    CheckBoxReceiveLF:      TCheckBox;
    CheckBoxSendLF:     TCheckBox;
    ComboBoxSendLine:       TComboBox;
    LabelCharLength: TLabel;
    TextBoxCharLen: TEdit;
    LabelHeader: TLabel;
    LabelLineEndingReceive: TLabel;
    LabelLineEndingSend:    TLabel;
    LabelMacAddress:        TLabel;
    MemoReceiveData:        TMemo;
    TextBoxDeviceName:      TEdit;
    procedure ButtonSendClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TextBoxCharLenEditingDone(Sender: TObject);

  private

  public

  end;

procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: TSimpleBleUuid; restore: TPanel);
procedure UartTerminalStop(PerHandle: TSimpleBlePeripheral);
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
    HasModemIn:   Boolean;
    UuidModemOut: TSimpleBleUuid;
    HasModemOut:  Boolean;
    RxCharLength: Integer;
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
  if VspTerminal[i].HasModemOut then
    if SimpleBlePeripheralUnsubscribe(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidModemOut) = SIMPLEBLE_FAILURE then
      UtilLog('Unsubscribing from UART ModemOut characteristic failed.')
    else
      UtilLog('Unsubscribed from UART ModemOut characteristic.');

  // clean up
  UtilSetNextFormLeft(TerminalForm[i], true);
  Delete(VspTerminal, i, 1);
  RestorePanel.Enabled := true;
end;


{ Get RX characteristic length from edit box }
procedure TTerminalForm.TextBoxCharLenEditingDone(Sender: TObject);
var
  i: Integer;
begin
  if TextBoxCharLen.Caption <> '' then begin
    i := StrToInt(TextBoxCharLen.Caption);
    if i <= 0 then begin
      ShowMessage('RX Characteristic length must be >= 0');
      exit;
    end;
    VspTerminal[0].RxCharLength := StrToInt(TextBoxCharLen.Caption);
  end;
end;


{ Button send line clicked }
procedure TTerminalForm.ButtonSendClick(Sender: TObject);
var
  i, j: Integer;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
  s: string;
  exist: Boolean;
begin
  SetLength(Buffer, CharDescMaxLength);
  ChData := PByte(Buffer);

  // read string from edit field
  s := TerminalForm[0].ComboBoxSendLine.Caption;

  // append line endings
  if TerminalForm[0].CheckBoxSendCR.State = cbChecked then
    s := s + #13;
  if TerminalForm[0].CheckBoxSendLF.State = cbChecked then
    s := s + #10;

  // send data and split into chunks according to rx characteristic length
  ChLen := Length(s);
  i := 0;
  j := 0;
  while i < ChLen do begin
    Buffer[j] := Byte(s[i+1]);
    i := i + 1;
    j := j + 1;
    if (j = VspTerminal[0].RxCharLength) or (i = ChLen) then begin
      if SimpleBlePeripheralWriteCommand(VspTerminal[0].Handle, VspTerminal[0].UuidService, VspTerminal[0].UuidRx, ChData, j) = SIMPLEBLE_FAILURE then begin
        ShowMessage('Failed to send data. Check TX max characters.');
        UtilLog('Failed to send uart terminal data.');
        exit;
      end;
      j := 0;
    end;
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

procedure TTerminalForm.FormChangeBounds(Sender: TObject);
var
  i: Integer;
begin
  i := TForm(Sender).Tag;

  UtilSetNextFormTop(TerminalForm[i]);
  UtilSetNextFormLeft(TerminalForm[i]);
end;


{ Callback function on notification from tx characteristic }
procedure UartTxOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
var
  i: Integer;
  c: Char;
  s: String;
begin
  s := '';
  for i := 0 to Len-1 do begin
    c := Char(Data[i]);
    if (c <> #13) and (c <> #10) then  // check for <cr> or <lf>
      s := s + c
    else begin  // check different line ending charcters
      if (TerminalForm[0].CheckBoxReceiveCR.State = cbChecked) and (c = #13) then
        s := s + LineEnding;
      if (TerminalForm[0].CheckBoxReceiveLF.State = cbChecked) and (c = #10) then
        s := s + LineEnding;
    end;

    // if JSON closing bracket, then append line ending
    if (TerminalForm[0].CheckBoxReceiveJson.State = cbChecked) and (c = #125) then
      s := s + LineEnding;
  end;
  TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] := TerminalForm[0].MemoReceiveData.Lines[Pred(TerminalForm[0].MemoReceiveData.Lines.Count)] + s;
end;


{ Callback function on notification from tx characteristic }
procedure UartModemOutOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
begin
  UtilLog('Received ModemOut notification.');
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

  VspTerminal[i].Handle       := PerHandle;
  VspTerminal[i].UuidService  := SvUuid;
  VspTerminal[i].DeviceName   := DevName;
  VspTerminal[i].MacAddress   := MacAddr;
  VspTerminal[i].IsActive     := true;
  VspTerminal[i].RxCharLength := SimpleBlePeripheralMtu(VspTerminal[i].Handle);
  RestorePanel := restore;

  // search rx/tx/modem uart characteristics of service
  for j := 0 to Length(VspServiceUuids)-1 do begin
      if CompareChar(SvUuid.Value, VspServiceUuids[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0 then begin
        VspTerminal[i].ServiceName  := VspServiceUuids[j].Name;
        VspTerminal[i].UuidRx       := VspCharacteristicUuids[VspServiceUuids[j].ChRx].Uuid;
        VspTerminal[i].UuidTx       := VspCharacteristicUuids[VspServiceUuids[j].ChTx].Uuid;
        if VspServiceUuids[j].ChModemIn < 0 then  // modem characteristics are optional
          VspTerminal[i].HasModemIn := false
        else begin
          VspTerminal[i].HasModemIn := True;
          VspTerminal[i].UuidModemIn  := VspCharacteristicUuids[VspServiceUuids[j].ChModemOut].Uuid;
        end;
        if VspServiceUuids[j].ChModemOut < 0 then  // modem characteristics are optional
          VspTerminal[i].HasModemOut := false
        else begin
          VspTerminal[i].HasModemOut := True;
          VspTerminal[i].UuidModemOut  := VspCharacteristicUuids[VspServiceUuids[j].ChModemOut].Uuid;
        end;
      end;
  end;

  // create the form
  Application.CreateForm(TTerminalForm, TerminalForm[i]);
  TerminalForm[i].Tag := i;
  TerminalForm[i].Top := UtilGetNextFormTop();
  TerminalForm[i].Left := UtilGetNextFormLeft(TerminalForm[i]);
  if VspTerminal[i].DeviceName = '' then begin
    UtilLog('Open UART terminal: "<unknown name>" [' + UpperCase(VspTerminal[i].MacAddress) + '] - ' + VspTerminal[i].ServiceName);
    TerminalForm[i].Caption := '"<unknown name>" [' + UpperCase(VspTerminal[i].MacAddress) + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end else begin
      UtilLog('Open UART terminal: "' + VspTerminal[i].DeviceName + '" [' + UpperCase(VspTerminal[i].MacAddress) + '] - ' + VspTerminal[i].ServiceName);
      TerminalForm[i].Caption := '"' + VspTerminal[i].DeviceName + '" [' + UpperCase(VspTerminal[i].MacAddress) + '] - ' + VspTerminal[i].ServiceName + ' - Virtual Uart Terminal';
  end;
  TerminalForm[i].TextBoxDeviceName.Caption := VspTerminal[i].DeviceName;
  TerminalForm[i].LabelMacAddress.Caption   := 'MAC Address [' + UpperCase(VspTerminal[i].MacAddress) + ']';
  TerminalForm[i].TextBoxCharLen.Caption    := IntToStr(VspTerminal[i].RxCharLength);
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
  if VspTerminal[i].HasModemOut then begin
    if SimpleBlePeripheralNotify(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidModemOut, @UartModemOutOnNotify, Nil) = SIMPLEBLE_FAILURE then begin
      UtilLog('Subscribing to UART ModemOut characteristic failed, will continue without.');
      VspTerminal[i].HasModemOut := false;
    end else
      UtilLog('Subscribed to UART ModemOut characteristic');
  end;

  UtilSetNextFormTop(TerminalForm[i]);
  UtilSetNextFormLeft(TerminalForm[i]);

  TerminalForm[0].ComboBoxSendLine.Items.Clear;
  TerminalForm[0].ComboBoxSendLine.Items.Add('');

  TerminalForm[0].MemoReceiveData.Clear;
end;


{ Stop vsp terminal, unsubsribe characteristics, clean up }
procedure UartTerminalStop(PerHandle: TSimpleBlePeripheral);
var
  i: Integer;
begin
  // find vsp terminal and terminal form array index for given peripheral handle
  i := 0;
  while i < Length(VspTerminal) do begin
    if VspTerminal[i].Handle = PerHandle then
      break;
    Inc(i);
  end;

  // close form (also unsubscribes from notifications etc.
  TerminalForm[i].Close;
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

