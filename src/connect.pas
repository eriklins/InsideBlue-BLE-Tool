unit Connect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Util, Ble, Scan, UartTerminal, SimpleBle;

type

  { TDeviceForm }
  TDeviceForm = class(TForm)
    ButtonDisconnect: TButton;
    ScrollBoxGatt: TScrollBox;
    TextBoxDeviceName: TEdit;
    LabelMacAddress: TLabel;
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonVspTerminalClick(Sender: TObject);
    procedure ButtonCharRead(Sender: TObject);
    procedure ButtonCharWriteCommand(Sender: TObject);
    procedure ButtonCharWriteRequest(Sender: TObject);
    procedure ButtonCharNotify(Sender: TObject);
    procedure ButtonCharIndicate(Sender: TObject);
    procedure CheckboxHexAsciiClick(Sender: TObject);
    procedure CharEditingDone(Sender: TObject);
    procedure ConnectTimerTimer(Sender: TObject);
    //procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

  { Form elements to show device GATT structure }
  TDeviceFormElements = record
    Panel:                   array of TPanel;
    LabelServiceUuid:        array of TLabel;
    ButtonVspTerminal:       array of TButton;
    LabelCharacteristicUuid: array of array of TLabel;
    LabelDescriptorUuid:     array of array of array of TLabel;
    TextBoxCharacteristic:   array of array of TEdit;
    CheckBoxHexAscii:        array of array of TCheckBox;
    TextBoxDescriptor:       array of array of array of TEdit;
    ToggleBoxCharProp:       array of array of array [0..4] of TToggleBox;
  end;

  { Functions for other units }
  procedure ConnectInit;
  procedure ConnectDevice(DevName: string; MacAddr: string; PerHandle: TSimpleBlePeripheral; restore: TButton);
  function ConnectHandleIsConnected(h: TSimpleBlePeripheral): Boolean;
  procedure DisconnectDevice(idx: Integer);
  procedure ConnectRestoreVspPanel(DeIdx: Integer; SvIdx: Integer);


implementation

{$R *.lfm}

const
  DeviceFormPaddingVertical   = 6;
  DeviceFormPaddingHorizontal = 8;
  DeviceFormPropPadding       = 4;
  CanRead         = 0;
  CanWriteRequest = 1;
  CanWriteCommand = 2;
  CanNotify       = 3;
  CanIndicate     = 4;
  {$IFDEF UNIX}
    FormElementsExtraHeight = 4;
    FormElementsExtraWidth  = 8;
  {$ELSE}
    FormElementsExtraHeight = 0;
    FormElementsExtraWidth  = 0;
  {$ENDIF}
  DevicePanelColor = $EED4C0;

var
  DeviceForm: array of TDeviceForm;
  DeviceFormElements: array of TDeviceFormElements;
  RestoreFormElement: TButton;


{ initialize connect unit }
procedure ConnectInit;
begin

end;


{ Checks if the a peripheral handle is still being used for an active connection }
function ConnectHandleIsConnected(h: TSimpleBlePeripheral): Boolean;
var
  i: Integer;
begin
  Result := false;
  i := 0;
  while i < Length(BleConnectData) do begin
    if BleConnectData[i].PeripheralHandle = h then begin
      Result := true;
      Exit;
    end;
    Inc(i);
  end;
end;


{ Callback function on disconnect of device }
procedure DeviceOnDisconnect(Peripheral: TSimpleBlePeripheral; UserData: PPointer);
var
  idx: Integer;
begin
  idx := 0;
  while idx < Length(BleConnectData) do begin
    if BleConnectData[idx].IsConnected then begin
      if BleConnectData[idx].PeripheralHandle = Peripheral then begin
        UtilLog('Device "' + BleConnectData[idx].DeviceName + '" [' + BleConnectData[idx].MacAddress + '] disconnected unexpectedly.');
        ShowMessage('Device [' + BleConnectData[idx].MacAddress + '] disconnected unexpectedly.');
        DeviceForm[idx].Close;  // this automatically forces TDeviceForm.FormClose()
      end;
    end;
    Inc(idx);
  end;
end;


{ connect to device }
procedure ConnectDevice(DevName: string; MacAddr: string; PerHandle: TSimpleBlePeripheral; restore: TButton);
var
  i, j: Integer;
  SvIdx, ChIdx, DeIdx: Integer;
  s, n: String;
  LabelNoConnect: TLabel;
  NextElementVertical, NextPanelVertical, NextButtonCoord: Integer;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
  VspServiceFound: Boolean;
begin
  SetLength(Buffer, CharDescMaxLength);
  ChData := PByte(Buffer);

  // increment devices and increase some arrays
  i := Length(BleConnectData);
  SetLength(BleConnectData, i+1);
  SetLength(DeviceForm, i+1);
  SetLength(DeviceFormElements, i+1);

  // grab existing scan data
  BleConnectData[i].ScnIdx           := restore.Tag;
  BleConnectData[i].DeviceName       := DevName;
  BleConnectData[i].MacAddress       := MacAddr;
  BleConnectData[i].PeripheralHandle := PerHandle;
  RestoreFormElement := restore;  // this points to the connect button on the scan form we need to re-enable after disconnect

  // register on connect callback function
  SimpleBlePeripheralSetCallbackOnDisconnected(BleConnectData[i].PeripheralHandle, @DeviceOnDisconnect, Nil);

  // compose form for the new device
  UtilLog('Connecting...');
  Application.CreateForm(TDeviceForm, DeviceForm[i]);
  DeviceForm[i].Tag  := i;
  DeviceForm[i].ButtonDisconnect.Tag := i;
  DeviceForm[i].Top  := UtilGetNextFormTop;
  DeviceForm[i].Left := UtilGetNextFormLeft;
  if BleConnectData[i].DeviceName = '' then begin
    DeviceForm[i].Caption                   := '"<unknown name>" [' + UpperCase(BleConnectData[i].MacAddress) + '] - Connecting...';
    DeviceForm[i].TextBoxDeviceName.Caption := '<unknown name>';
    DeviceForm[i].TextBoxDeviceName.Enabled := false;
  end else begin
    DeviceForm[i].Caption                   := '"' + BleConnectData[i].DeviceName + '" [' + UpperCase(BleConnectData[i].MacAddress) + '] - Connecting...';
    DeviceForm[i].TextBoxDeviceName.Caption := BleConnectData[i].DeviceName;
  end;
  DeviceForm[i].LabelMacAddress.Caption   := 'MAC Address [' + UpperCase(BleConnectData[i].MacAddress) + ']';
  DeviceForm[i].Show;
  UtilSetNextFormTop(DeviceForm[i]);
  UtilSetNextFormLeft(DeviceForm[i]);

  Application.ProcessMessages;  // shows the form and elements before connecting because connecting blocks

  // try to connect to device
  if SimpleBlePeripheralConnect(BleConnectData[i].PeripheralHandle) = SIMPLEBLE_FAILURE then begin
    UtilLog('Failed to connect to "' + BleConnectData[i].DeviceName + '" [' + UpperCase(BleConnectData[i].MacAddress) + ']');
    DeviceForm[i].Caption := '"' + BleConnectData[i].DeviceName + '" [' + UpperCase(BleConnectData[i].MacAddress) + '] - Could not connect.';
    LabelNoConnect            := TLabel.Create(DeviceForm[i]);
    LabelNoConnect.Parent     := DeviceForm[i].ScrollBoxGatt;
    LabelNoConnect.Top        := DeviceForm[i].TextBoxDeviceName.Top + DeviceForm[i].TextBoxDeviceName.Height + 4*DeviceFormPaddingVertical;
    LabelNoConnect.Left       := DeviceFormPaddingHorizontal;
    LabelNoConnect.Font.Size  := 12;
    LabelNoConnect.Font.Color := clRed;
    LabelNoConnect.Font.Style := [fsBold];
    LabelNoConnect.Caption    := 'Failed to connect to device.';
    Exit;
  end else begin
    // connect was successful
    BleConnectData[i].IsConnected := true;
    UtilLog('Connected to "' + BleConnectData[i].DeviceName + '" [' + UpperCase(BleConnectData[i].MacAddress) + ']');
    DeviceForm[i].Caption := '"' + BleConnectData[i].DeviceName + '" [' + UpperCase(BleConnectData[i].MacAddress) + '] - Connected.';
  end;

  // check for GATT services, characteristics and descriptors
  UtilLog('Reading GATT table:');
  if SimpleBlePeripheralServicesCount(BleConnectData[i].PeripheralHandle) > 0 then begin
    BleConnectData[i].ServicesCount := SimpleBlePeripheralServicesCount(BleConnectData[i].PeripheralHandle);
    SetLength(BleConnectData[i].Services,                   BleConnectData[i].ServicesCount);
    SetLength(BleConnectData[i].Characteristic,             BleConnectData[i].ServicesCount);
    SetLength(BleConnectData[i].Descriptor,                 BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].Panel,                   BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelServiceUuid,        BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].ButtonVspTerminal,       BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelCharacteristicUuid, BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelDescriptorUuid,     BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].TextBoxCharacteristic,   BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].TextBoxDescriptor,       BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].ToggleBoxCharProp,       BleConnectData[i].ServicesCount);
    SetLength(DeviceFormElements[i].CheckBoxHexAscii,        BleConnectData[i].ServicesCount);
    NextPanelVertical := DeviceFormPaddingVertical;

    for SvIdx := 0 to BleConnectData[i].ServicesCount-1 do begin

      // create panel for service characteristics and descriptors
      DeviceFormElements[i].Panel[SvIdx]        := TPanel.Create(DeviceForm[i]);
      DeviceFormElements[i].Panel[SvIdx].Parent := DeviceForm[i].ScrollBoxGatt;
      DeviceFormElements[i].Panel[SvIdx].Top    := NextPanelVertical;
      DeviceFormElements[i].Panel[SvIdx].Left   := DeviceFormPaddingHorizontal;
      DeviceFormElements[i].Panel[SvIdx].Width  := DeviceForm[i].ScrollBoxGatt.Width - 4*DeviceFormPaddingHorizontal;
      DeviceFormElements[i].Panel[SvIdx].Color  := TColor(DevicePanelColor);
      NextElementVertical := DeviceFormPaddingVertical;
      SimpleBlePeripheralServicesGet(BleConnectData[i].PeripheralHandle, SvIdx, BleConnectData[i].Services[SvIdx]);
      SetString(s, BleConnectData[i].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

      // look for assigned service numbers or proprietary vsp services
      n := BleAssignedServiceUuidToName(s);
      if n = '' then begin
        n := BleVspServiceUuidToName(s);
        if n = '' then begin // neither assigned service nor vsp service
          UtilLog('     SV: ' + s);
        end else begin  // vsp service
          UtilLog('     SV: ' + s + ' (' + n + ')');
          VspServiceFound := true;
        end;
      end else begin  // assigned service
        UtilLog('     SV: ' + s + ' (' + n + ')');
        VspServiceFound := false;
      end;

      // show service uuid
      NextElementVertical := 2*DeviceFormPaddingVertical;
      DeviceFormElements[i].LabelServiceUuid[SvIdx]            := TLabel.Create(DeviceForm[i]);
      DeviceFormElements[i].LabelServiceUuid[SvIdx].Parent     := DeviceFormElements[i].Panel[SvIdx];
      DeviceFormElements[i].LabelServiceUuid[SvIdx].Top        := Trunc(NextElementVertical/2);
      DeviceFormElements[i].LabelServiceUuid[SvIdx].Left       := DeviceFormPaddingHorizontal;
      DeviceFormElements[i].LabelServiceUuid[SvIdx].Font.Size  := 11;
      DeviceFormElements[i].LabelServiceUuid[SvIdx].Font.Style := [fsBold];
      if n = '' then
        DeviceFormElements[i].LabelServiceUuid[SvIdx].Caption  := UpperCase(s)
      else begin
        DeviceFormElements[i].LabelServiceUuid[SvIdx].Caption  := n;
        DeviceFormElements[i].LabelServiceUuid[SvIdx].ShowHint := true;
        DeviceFormElements[i].LabelServiceUuid[SvIdx].Hint     := s;
      end;
      if VspServiceFound then begin
        DeviceFormElements[i].ButtonVspTerminal[SvIdx]         := TButton.Create(DeviceForm[i]);
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Parent  := DeviceFormElements[i].Panel[SvIdx];
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Tag     := (i shl TagPosDev) or (SvIdx shl TagPosSrv);
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Top     := DeviceFormElements[i].LabelServiceUuid[SvIdx].Top;
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Left    := DeviceFormElements[i].Panel[SvIdx].Width - 64 - DeviceFormPaddingHorizontal;
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Width   := 64;
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].Caption := 'Terminal';
        DeviceFormElements[i].ButtonVspTerminal[SvIdx].OnClick := @DeviceForm[i].ButtonVspTerminalClick;
      end;
      NextElementVertical := DeviceFormElements[i].LabelServiceUuid[SvIdx].Top + DeviceFormElements[i].LabelServiceUuid[SvIdx].Height + DeviceFormPaddingVertical div 2;

      if BleConnectData[i].Services[SvIdx].CharacteristicCount > 0 then begin
        SetLength(BleConnectData[i].Characteristic[SvIdx],             BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(BleConnectData[i].Descriptor[SvIdx],                 BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].LabelCharacteristicUuid[SvIdx], BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].LabelDescriptorUuid[SvIdx],     BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].TextBoxCharacteristic[SvIdx],   BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].TextBoxDescriptor[SvIdx],       BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].ToggleBoxCharProp[SvIdx],       BleConnectData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].CheckBoxHexAscii[SvIdx],        BleConnectData[i].Services[SvIdx].CharacteristicCount);

        for ChIdx := 0 to BleConnectData[i].Services[SvIdx].CharacteristicCount-1 do begin
          SetLength(BleConnectData[i].Characteristic[SvIdx][ChIdx].data, CharDescMaxLength);
          SetString(s, BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
          n := BleAssignedCharacteristicUuidToName(s);
          if n = '' then begin
            n := BleVspCharacteristicUuidToName(s);
            if n = '' then  // neither assigned service nor vsp service
              UtilLog('         CH: ' + s)
            else  // vsp service
              UtilLog('         CH: ' + s + ' (' + n + ')');
          end else  // assigned service
            UtilLog('         CH: ' + s + ' (' + n + ')');

          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx]            := TLabel.Create(DeviceForm[i]);
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Parent     := DeviceFormElements[i].Panel[SvIdx];
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Top        := NextElementVertical;
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Left       := 3*DeviceFormPaddingHorizontal;
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].AutoSize   := false;
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Width      := 248;
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Height     := 18;
          DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Font.Size  := 10;
          if n = '' then
            DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Caption  := UpperCase(s)
          else begin
            DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Caption  := n;
            DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].ShowHint := true;
            DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Hint     := s;
          end;
          NextElementVertical := DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Top + DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Height;

          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx]           := TEdit.Create(DeviceForm[i]);
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Parent    := DeviceFormElements[i].Panel[SvIdx];
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Tag       := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top       := NextElementVertical;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Left      := 3*DeviceFormPaddingHorizontal;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Font.Size := 10;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].AutoSize  := false;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Width     := 248;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Height    := 20;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Color     := clForm;
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].ReadOnly  := true;
          NextElementVertical := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top + DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Height + DeviceFormPaddingVertical div 2;

          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx]         := TCheckBox.Create(DeviceForm[i]);
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].Parent  := DeviceFormElements[i].Panel[SvIdx];
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].Tag     := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].Caption := 'ASCII';
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].Top     := DeviceFormElements[i].LabelCharacteristicUuid[SvIdx][ChIdx].Top;
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].Left    := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Left + DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Width + DeviceFormPaddingHorizontal;
          DeviceFormElements[i].CheckBoxHexAscii[SvIdx][ChIdx].OnClick := @DeviceForm[i].CheckboxHexAsciiClick;

          // check characteristic properties and add buttons for each
          NextButtonCoord := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Left + DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Width + DeviceFormPaddingHorizontal;
          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].CanRead then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Width      := 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Height     := 20 + FormElementsExtraHeight;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Font.Size  := 8;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Caption    := 'RD';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].OnChange   := @DeviceForm[i].ButtonCharRead;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth + DeviceFormPropPadding;

            // read characteristic value, store into device record and display in text box
            SimpleBlePeripheralRead(BleConnectData[i].PeripheralHandle, BleConnectData[i].Services[SvIdx].Uuid,
                                                                         BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                         ChData, ChLen);
            for j := 0 to ChLen-1 do
              BleConnectData[i].Characteristic[SvIdx][ChIdx].data[j] := ChData[j];
            BleConnectData[i].Characteristic[SvIdx][ChIdx].len := ChLen;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToHex(BleConnectData[i].Characteristic[SvIdx][ChIdx].data, BleConnectData[i].Characteristic[SvIdx][ChIdx].len);
          end;
          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].CanWriteCommand then begin
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].ReadOnly := false;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Width      := 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Height     := 20 + FormElementsExtraHeight;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Font.Size  := 8;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Caption    := 'WRc';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].OnChange   := @DeviceForm[i].ButtonCharWriteCommand;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth + DeviceFormPropPadding;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].OnEditingDone := @DeviceForm[i].CharEditingDone;
          end;
          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].CanWriteRequest then begin
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].ReadOnly := false;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Width      := 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Height     := 20 + FormElementsExtraHeight;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Font.Size  := 8;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Caption    := 'WRr';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].OnChange   := @DeviceForm[i].ButtonCharWriteRequest;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth + DeviceFormPropPadding;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].OnEditingDone := @DeviceForm[i].CharEditingDone;
          end;
          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].CanNotify then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Width      := 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Height     := 20 + FormElementsExtraHeight;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Font.Size  := 8;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Caption    := 'NOT';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].OnChange   := @DeviceForm[i].ButtonCharNotify;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth + DeviceFormPropPadding;
          end;
          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].CanIndicate then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Width      := 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Height     := 20 + FormElementsExtraHeight;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Font.Size  := 8;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Caption    := 'IND';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].OnChange   := @DeviceForm[i].ButtonCharIndicate;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + FormElementsExtraWidth + DeviceFormPropPadding;
          end;

          if BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount > 0 then begin
            SetLength(BleConnectData[i].Descriptor[SvIdx][ChIdx],             BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            SetLength(DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx], BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            SetLength(DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx],   BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            NextElementVertical := NextElementVertical - DeviceFormPaddingVertical div 2;

            for DeIdx := 0 to BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount-1 do begin
              SetString(s, BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].Descriptors[DeIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
              n := BleAssignedDescriptorUuidToName(s);
              if n = '' then
                UtilLog('             DE: ' + s)
              else
                UtilLog('             DE: ' + s + ' (' + n + ')');

              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx]            := TLabel.Create(DeviceForm[i]);
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Parent     := DeviceFormElements[i].Panel[SvIdx];
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Top        := NextElementVertical;
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Left       := 5*DeviceFormPaddingHorizontal;
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].AutoSize   := false;
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Width      := 248;
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Height     := 18;
              DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Font.Size  := 10;
              if n = '' then
                DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Caption  := UpperCase(s)
              else begin
                DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Caption  := n;
                DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].ShowHint := true;
                DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Hint     := s;
              end;
              NextElementVertical := DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Top + DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx][DeIdx].Height;

              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx]           := TEdit.Create(DeviceForm[i]);
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Parent    := DeviceFormElements[i].Panel[SvIdx];
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Top       := NextElementVertical;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Left      := 5*DeviceFormPaddingHorizontal;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Font.Size := 10;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].MaxLength := 4;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].AutoSize  := false;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Width     := 56;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Height    := 20;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Color     := clForm;
              NextElementVertical := DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Top + DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Height + DeviceFormPaddingVertical div 2;
              // read descriptor value, store in device data record and display in text box
              SetLength(BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].data, CharDescMaxLength);
              ChData := PByte(BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].data);
              SimpleBlePeripheralReadDescriptor(BleConnectData[i].PeripheralHandle, BleConnectData[i].Services[SvIdx].Uuid,
                                                                                     BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                                     BleConnectData[i].Services[SvIdx].Characteristics[ChIdx].Descriptors[DeIdx].Uuid,
                                                                                     ChData, ChLen);
              for j := 0 to ChLen-1 do
                BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].data[j] := ChData[j];
              BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].len := ChLen;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Caption := UtilDataToHex(BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].data, BleConnectData[i].Descriptor[SvIdx][ChIdx][DeIdx].len);
            end;
          end;
        end;
      end;
      DeviceFormElements[i].Panel[SvIdx].Height := NextElementVertical + DeviceFormPaddingVertical;
      NextPanelVertical := DeviceFormElements[i].Panel[SvIdx].Top + NextElementVertical + 2*DeviceFormPaddingVertical;
    end;
  end;
  DeviceForm[i].Height := NextPanelVertical + 12*DeviceFormPaddingVertical;
  DeviceForm[i].Constraints.MaxHeight := NextPanelVertical + 12*DeviceFormPaddingVertical;
end;


{ Disconnect from device }
procedure DisconnectDevice(idx: Integer);
begin
  if BleConnectData[idx].IsConnected then begin
    BleConnectData[idx].IsConnected := false;
    UtilLog('Disconnecting...');
    DeviceForm[idx].Caption := BleConnectData[idx].DeviceName + '" [' + UpperCase(BleConnectData[idx].MacAddress) + '] - Disconnecting...';
    Application.ProcessMessages;
    if SimpleBlePeripheralDisconnect(BleConnectData[idx].PeripheralHandle) = SIMPLEBLE_FAILURE then begin
      UtilLog('Failed to disconnect from "' + BleConnectData[idx].DeviceName + '" [' + UpperCase(BleConnectData[idx].MacAddress) + ']');
      DeviceForm[idx].Caption := BleConnectData[idx].DeviceName + '" [' + UpperCase(BleConnectData[idx].MacAddress) + '] - Failed to disconnect.';
    end else begin
      UtilLog('Disconnected from "' + BleConnectData[idx].DeviceName + '" [' + UpperCase(BleConnectData[idx].MacAddress) + ']');
      DeviceForm[idx].Caption := BleConnectData[idx].DeviceName + '" [' + UpperCase(BleConnectData[idx].MacAddress) + '] - Disconnected.';
      //SimpleBlePeripheralReleaseHandle(BleConnectData[idx].PeripheralHandle);
    end;
  end;
end;


{ Create the device form }
procedure TDeviceForm.FormCreate(Sender: TObject);
begin
  Self.Constraints.MaxWidth := Self.Width;
  Self.Constraints.MinWidth := Self.Width;

end;


{ close device form and also disconnect from device }
procedure TDeviceForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  idx, i: Integer;
begin
  idx := TForm(Sender).Tag;

  if UartTerminalIsActive(BleConnectData[idx].PeripheralHandle) then begin
    ShowMessage('Cannot close when uart terminal open.');
    CanClose := false;
    Exit;
  end;

  DisconnectDevice(idx);
  RestoreFormElement.Enabled := true;
  UtilSetNextFormLeft(DeviceForm[Length(BleConnectData)-1], true);
  Delete(BleConnectData, idx, 1);
  Delete(DeviceForm, idx, 1);
end;


{ Disconnect from device }
procedure TDeviceForm.ButtonDisconnectClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := TForm(Sender).Tag;
  DeviceForm[idx].Close;  // this automatically forces TDeviceForm.FormClose()
end;


{ Open vsp terminal }
procedure TDeviceForm.ButtonVspTerminalClick(Sender: TObject);
var
  i, DeIdx, SvIdx: Integer;
  s: string;
begin
  i := TCheckBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;

  Inc(BleConnectData[DeIdx].VspActiveCnt);
  DeviceFormElements[DeIdx].Panel[SvIdx].Enabled := false;

  SetString(s, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  UartTerminalStart(BleConnectData[DeIdx].PeripheralHandle, BleConnectData[DeIdx].DeviceName, BleConnectData[DeIdx].MacAddress, s, DeviceFormElements[DeIdx].Panel[SvIdx]);
end;


{ Re-enable the service panel in the connect window after closing vsp terminal }
procedure ConnectRestoreVspPanel(DeIdx: Integer; SvIdx: Integer);
begin
  DeviceFormElements[DeIdx].Panel[SvIdx].Enabled := true;
end;


{ Checkbox Hex/ASCII clicked }
procedure TDeviceForm.CheckboxHexAsciiClick(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
begin
  // from button tag property we get which device, service and characteristic we are about to change
  i := TCheckBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;
  if TCheckBox(Sender).Checked then  // show as ascii
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToAscii(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show as hex
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len);
end;


{ Read characteristic button clicked }
procedure TDeviceForm.ButtonCharRead(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  sSv, sCh: String;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
begin
  if TToggleBox(Sender).State = cbUnchecked then
    Exit;

  SetLength(Buffer, CharDescMaxLength);
  ChData := PByte(Buffer);

  // from button tag property we get which device, service and characteristic we are about to change
  i := TToggleBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  SetString(sSv, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  if(SimpleBlePeripheralRead(BleConnectData[DeIdx].PeripheralHandle, BleConnectData[DeIdx].Services[SvIdx].Uuid,
                                                                      BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                      ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    UtilLog('Read failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('Read failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin
    for i := 0 to ChLen-1 do
      BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := ChData[i];
    BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len := ChLen;
    UtilLog('Read: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len));
    if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
      DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToAscii(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len)
    else  // show data as hex string
      DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len);
  end;

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clForm;
  TToggleBox(Sender).Font.Style := [];
  TToggleBox(Sender).State := cbUnchecked;
end;


{ Write Command ccharacteristic button clicked }
procedure TDeviceForm.ButtonCharWriteCommand(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  sSv, sCh: String;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
begin
  if TToggleBox(Sender).State = cbUnchecked then
    Exit;

  SetLength(Buffer, CharDescMaxLength);

  // from button tag property we get which device, service and characteristic we are about to change
  i := TToggleBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  SetString(sSv, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  ChLen := BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len;
  for i := 0 to ChLen-1 do
    Buffer[i] := BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i];

  ChData := PByte(Buffer);
  if(SimpleBlePeripheralWriteCommand(BleConnectData[DeIdx].PeripheralHandle, BleConnectData[DeIdx].Services[SvIdx].Uuid,
                                                                              BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                              ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    UtilLog('WriteCommand failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('WriteCommand failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else
    UtilLog('WriteCommand: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len));

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clForm;
  TToggleBox(Sender).Font.Style := [];
  TToggleBox(Sender).State := cbUnchecked;
end;


{ Write Request ccharacteristic button clicked }
procedure TDeviceForm.ButtonCharWriteRequest(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  sSv, sCh: String;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
begin
  if TToggleBox(Sender).State = cbUnchecked then
    Exit;

  SetLength(Buffer, CharDescMaxLength);

  // from button tag property we get which device, service and characteristic we are about to change
  i := TToggleBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  SetString(sSv, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  ChLen := BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len;
  for i := 0 to ChLen-1 do
    Buffer[i] := BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i];

  ChData := PByte(Buffer);
  if(SimpleBlePeripheralWriteRequest(BleConnectData[DeIdx].PeripheralHandle, BleConnectData[DeIdx].Services[SvIdx].Uuid,
                                                                              BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                              ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    UtilLog('WriteRequest failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('WriteRequest failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else
    UtilLog('WriteRequest: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len));

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clForm;
  TToggleBox(Sender).Font.Style := [];
  TToggleBox(Sender).State := cbUnchecked;
end;


{ Callback function on notification from peripheral }
procedure PeripheralOnNotify(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
var
  i, j, k, DeIdx, SvIdx, ChIdx: Integer;
  s, sSv, sCh: String;
  f: Boolean;
begin
  SetString(sSv, SvUuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ChUuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  // get device, service and characteristic indexes from service and characteristic uuids
  f := false;
  for i := 0 to Length(BleConnectData)-1 do begin
    for j := 0 to BleConnectData[i].ServicesCount-1 do begin
      SetString(s, BleConnectData[i].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
      if s = sSv then
        for k := 0 to BleConnectData[i].Services[j].CharacteristicCount-1 do begin
          SetString(s, BleConnectData[i].Services[j].Characteristics[k].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
          if s = sCh then begin
            DeIdx := i;
            SvIdx := j;
            ChIdx := k;
            f := true;
            break;
          end;
        end;
      if f then break;
    end;
    if f then break;
  end;

  if not f then begin
    UtilLog('Notification: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" Unknown UUIDs.');
    Exit;
  end;

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  Application.ProcessMessages;

  for i := 0 to Len-1 do
    BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Data[i];
  BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len := Len;

  if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToAscii(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show data as hex string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len);

  UtilLog('Notification: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                            ' Data=' + UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len));
  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clForm;
end;


{ Notify characteristic button clicked }
procedure TDeviceForm.ButtonCharNotify(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  sSv, sCh: String;
begin
  // from button tag property we get which device, service and characteristic we are about to change
  i := tButton(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  SetString(sSv, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  if TToggleBox(Sender).State = cbChecked then begin  // subscribe
    if(SimpleBlePeripheralNotify(BleConnectData[DeIdx].PeripheralHandle,
                              BleConnectData[DeIdx].Services[SvIdx].Uuid,
                              BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                              @PeripheralOnNotify, Nil) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      UtilLog('Subscribing failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Subscribing failed.');
      TToggleBox(Sender).Font.Style := [];
    end else  // success...
      TToggleBox(Sender).Font.Style := [fsBold];
      UtilLog('Subscribed notifications: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin  // unsubscribe
    TToggleBox(Sender).Font.Style := [];
    if(SimpleBlePeripheralUnsubscribe(BleConnectData[DeIdx].PeripheralHandle,
                                   BleConnectData[DeIdx].Services[SvIdx].Uuid,
                                   BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      UtilLog('Unsubscribing failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Unsubscribing failed.');
    end else  // success
      UtilLog('Unsubscribed notifications: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end;
end;


{ Callback function on indication from peripheral }
procedure PeripheralOnIndicate(SvUuid: TSimpleBleUuid; ChUuid: TSimpleBleUuid; Data: PByte; Len: NativeUInt; UserData: PPointer);
var
  i, j, k, DeIdx, SvIdx, ChIdx: Integer;
  s, sSv, sCh: String;
  f: Boolean;
begin
  SetString(sSv, SvUuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ChUuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  // get device, service and characteristic indexes from service and characteristic uuids
  f := false;
  for i := 0 to Length(BleConnectData)-1 do begin
    for j := 0 to BleConnectData[i].ServicesCount-1 do begin
      SetString(s, BleConnectData[i].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
      if s = sSv then
        for k := 0 to BleConnectData[i].Services[j].CharacteristicCount-1 do begin
          SetString(s, BleConnectData[i].Services[j].Characteristics[k].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
          if s = sCh then begin
            DeIdx := i;
            SvIdx := j;
            ChIdx := k;
            f := true;
            break;
          end;
        end;
      if f then break;
    end;
    if f then break;
  end;

  if not f then begin
    UtilLog('Indication: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" Unknown UUIDs.');
    Exit;
  end;

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  Application.ProcessMessages;

  for i := 0 to Len-1 do
    BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Data[i];
  BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len := Len;

  if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToAscii(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show data as hex string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len);

  UtilLog('Indication: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                            ' Data=' + UtilDataToHex(BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data, BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len));
  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clForm;
end;


{ Indicate characteristic button clicked }
procedure TDeviceForm.ButtonCharIndicate(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  sSv, sCh: String;
begin
  // from button tag property we get which device, service and characteristic we are about to change
  i := tButton(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  SetString(sSv, BleConnectData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  if TToggleBox(Sender).State = cbChecked then begin  // subscribe
    if(SimpleBlePeripheralIndicate(BleConnectData[DeIdx].PeripheralHandle,
                              BleConnectData[DeIdx].Services[SvIdx].Uuid,
                              BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                              @PeripheralOnIndicate, Nil) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      UtilLog('Subscribing failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Subscribing failed.');
      TToggleBox(Sender).Font.Style := [];
    end else  // success...
      TToggleBox(Sender).Font.Style := [fsBold];
      UtilLog('Subscribed indications: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin  // unsubscribe
    TToggleBox(Sender).Font.Style := [];
    if(SimpleBlePeripheralUnsubscribe(BleConnectData[DeIdx].PeripheralHandle,
                                   BleConnectData[DeIdx].Services[SvIdx].Uuid,
                                   BleConnectData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      UtilLog('Unsubscribing failed: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Unsubscribing failed.');
    end else  // success
      UtilLog('Unsubscribed indications: [' + BleConnectData[DeIdx].MacAddress + '] "' + BleConnectData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end;
end;


{ Characteristic text box editing done }
procedure TDeviceForm.CharEditingDone(Sender: TObject);
var
  i, DeIdx, SvIdx, ChIdx: Integer;
  Buffer: array of Byte;
  len: Integer;
begin
  SetLength(Buffer, CharDescMaxLength);

  // from button tag property we get which device, service and characteristic we are about to change
  i := TToggleBox(Sender).Tag;
  DeIdx := (i shr TagPosDev) and $ff;
  SvIdx := (i shr TagPosSrv) and $ff;
  ChIdx := (i shr TagPosChr) and $ff;

  if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // convert ascii string to data
    Buffer := UtilAsciiToData(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption, len)
  else begin // convert hex string to data
    if not UtilStringIsHex(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption) then begin
      ShowMessage('Invalid hex characters or not multiple of 2');
      Exit;
    end;
    Buffer := UtilHexToData(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption, len);
  end;

  BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].len := len;
  for i := 0 to len-1 do
    BleConnectData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Buffer[i];
end;


{ Tick timer for device connect functions }
procedure TDeviceForm.ConnectTimerTimer(Sender: TObject);
var
  idx: Integer;
  c: Boolean;
begin
  idx := 0;
  c := false;
  while idx < Length(BleConnectData)-1 do begin
    if BleConnectData[idx].IsConnected then begin
      // check if device is still connected
      if SimpleBlePeripheralIsConnected(BleConnectData[idx].PeripheralHandle, c) = SIMPLEBLE_FAILURE then begin
        UtilLog('SIMPLEBLE_FAILURE');
        BleConnectData[idx].IsConnected := false;
      end;
      // not connected anymore, so we issue a message, clean up and close form
      if not c then begin
        BleConnectData[idx].IsConnected := false;
        UtilLog('Device "' + BleConnectData[idx].DeviceName + '" [' + BleConnectData[idx].MacAddress + '] disconnected unexpectedly.');
        ShowMessage('Device [' + BleConnectData[idx].MacAddress + '] disconnected unexpectedly.');
        DisconnectDevice(idx);
        RestoreFormElement.Enabled := True;
        Delete(BleConnectData, idx, 1);
        UtilSetNextFormLeft(DeviceForm[Length(BleConnectData)-1], true);
        Delete(DeviceForm, idx, 1);
      end;
    end;
    Inc(idx);
  end;
end;


end.


