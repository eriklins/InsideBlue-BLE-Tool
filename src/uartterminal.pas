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
    CheckBoxReceiveCR1:     TCheckBox;
    CheckBoxReceiveLF:      TCheckBox;
    CheckBoxReceiveLF1:     TCheckBox;
    ComboBoxSendLine:       TComboBox;
    LabelLineEndingReceive: TLabel;
    LabelLineEndingSend:    TLabel;
    LabelMacAddress:        TLabel;
    MemoReceiveData:        TMemo;
    TextBoxDeviceName:      TEdit;
    procedure ComboBoxSendLineEditingDone(Sender: TObject);
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
  UtilSetNextFormLeft(TerminalForm[i], true);
  Delete(VspTerminal, i, 1);
  RestorePanel.Enabled := true;
end;


{ Editing of send string is done }
procedure TTerminalForm.ComboBoxSendLineEditingDone(Sender: TObject);
begin

end;


{ Callback function on notification from tx characteristic }
procedure UartTxOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
begin

end;


{ Callback function on notification from tx characteristic }
procedure UartModemOutOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
begin

end;


{ Start a vsp uart terminal to the device with given peripheral handle and service uuid }
procedure UartTerminalStart(PerHandle: TSimpleBlePeripheral; DevName: string; MacAddr: string; SvUuid: TSimpleBleUuid; restore: TPanel);
var
  i, j, k: Integer;
  TmpSvUuid, TmpChUuid: TSimpleBleUuid;
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
    UtilLog('Subscribing to UART TX characteristic failed');
  end;
  UtilLog('Subscribed to UART TX characteristic');

  // subscribe to uart ModemOut characteristic notifications
  if VspTerminal[i].HaveModemOut then begin
    if SimpleBlePeripheralNotify(VspTerminal[i].Handle, VspTerminal[i].UuidService, VspTerminal[i].UuidModemOut, @UartModemOutOnNotify, Nil) = SIMPLEBLE_FAILURE then begin
      UtilLog('Subscribing to UART ModemOut characteristic failed');
    end;
    UtilLog('Subscribed to UART ModemOut characteristic');
  end;

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


{
procedure TForm1.Button1Click(Sender: TObject);
begin
  // make sure the memo is empty for the example
  Memo1.Lines.Clear;

  // simulate writing characters to the memo
  WriteToMemo('hello world' + LineEnding);
  WriteToMemo('This ');
  WriteToMemo('tex');
  WriteToMemo('t ');
  WriteToMemo('is wr');
  WriteToMemo('itte');
  WriteToMemo('n to the ');
  WriteToMemo('sam');
  WriteToMemo('e line');
  WriteToMemo(LineEnding);
  WriteToMemo('this contains' + LineEnding + 'a newline');
end;

procedure TForm1.WriteToMemo(S: string);
begin
  // memo has to contain at least a single line in order to be
  // able to use a valid index to the strings
  if Memo1.Lines.Count < 1 then Memo1.Lines.Add('');

  // add characters to the last line of the memo
  Memo1.Lines[Pred(Memo1.Lines.Count)] := Memo1.Lines[Pred(Memo1.Lines.Count)] + S;
end;

}
