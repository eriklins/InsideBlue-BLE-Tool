unit Connect;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Ble, Scan, SimpleBle;

type

  { TDeviceForm }
  TDeviceForm = class(TForm)
    ButtonDisconnect: TButton;
    ScrollBoxGatt: TScrollBox;
    TextBoxDeviceName: TEdit;
    LabelMacAddress: TLabel;
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonCharRead(Sender: TObject);
    procedure ButtonCharWriteCommand(Sender: TObject);
    procedure ButtonCharWriteRequest(Sender: TObject);
    procedure ButtonCharNotify(Sender: TObject);
    procedure ButtonCharIndicate(Sender: TObject);
    procedure CheckboxHexAsciiClick(Sender: TObject);
    procedure CharEditingDone(Sender: TObject);
    procedure ConnectTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

  { Form elements to show device GATT structure }
  TDeviceFormElements = record
    Panel: array of TPanel;
    LabelServiceUuid: array of TLabel;
    LabelCharacteristicUuid: array of array of TLabel;
    LabelDescriptorUuid: array of array of array of TLabel;
    TextBoxCharacteristic: array of array of TEdit;
    CheckBoxHexAscii: array of array of TCheckBox;
    TextBoxDescriptor: array of array of array of TEdit;
    ToggleBoxCharProp: array of array of array [0..4] of TToggleBox;
  end;

  TArrayOfByte = array of Byte;

  { Functions for other units }
  procedure ConnectInit;
  procedure ConnectDevice(ScIdx: Integer);
  procedure DisconnectDevice(idx: Integer);
  function DataToAscii(data: array of Byte; len: Integer): String;
  function DataToHex(data: array of Byte; len: Integer): String;
  function AsciiToData(data: string; var len: Integer): TArrayOfByte;
  function HexToData(data: string; var len: Integer): TArrayOfByte;
  function StringIsHex(s: string): Boolean;


implementation

{$R *.lfm}

uses Main;  // not best practice, but need access to ScanForm for log output...

type
  { Data of device characteristic or descriptor (for local copy) }
  TCharDescData = record
    data: array of Byte;
    len:  Integer;
  end;

  { Device data we need during an active device connection }
  TDeviceData = record
    ScnIdx: integer;
    PeripheralHandle:       TSimpleBlePeripheral;
    DeviceName:             String;
    MacAddress:             String;
    ServicesCount:          Integer;
    Services:               array of TSimpleBleService;
    Characteristic:         array of array of TCharDescData;
    Descriptor:             array of array of array of TCharDescData;
    IsConnected:            Boolean;
    VspTerminal:            TVspTerminal;
  end;

const
  CharDescMaxLength = 512;
  DeviceFormPaddingVertical   = 6;
  DeviceFormPaddingHorizontal = 8;
  DeviceFormPropPadding       = 4;
  CanRead         = 0;
  CanWriteRequest = 1;
  CanWriteCommand = 2;
  CanNotify       = 3;
  CanIndicate     = 4;
  TagPosDev = 24;
  TagPosSrv = 16;
  TagPosChr = 8;
  TagPosDes = 0;

var
  DeviceForm: array of TDeviceForm;
  DeviceFormElements: array of TDeviceFormElements;
  ConnDevicesData: array of TDeviceData;
  ConnDevicesCount: Integer;

{ initialize connect unit }
procedure ConnectInit;
begin

end;


procedure DeviceOnDisconnect(Peripheral: TSimpleBlePeripheral; UserData: PPointer);
var
  idx: Integer;
begin
  idx := 0;
  while idx < Length(ConnDevicesData) do begin
    if ConnDevicesData[idx].IsConnected then begin
      if ConnDevicesData[idx].PeripheralHandle = Peripheral then begin
        //ConnDevicesData[idx].IsConnected := false;
        ScanForm.LogOutput.Append('Device "' + ConnDevicesData[idx].DeviceName + '" [' + ConnDevicesData[idx].MacAddress + '] disconnected unexpectedly.');
        ShowMessage('Device [' + ConnDevicesData[idx].MacAddress + '] disconnected unexpectedly.');
        DeviceForm[idx].Close;  // this automatically forces TDeviceForm.FormClose()
        //DisconnectDevice(idx);
        //ScanRestoreConnectButton(ConnDevicesData[idx].PeripheralHandle);
        //if NextFormLeftCoordinate <= DeviceForm[Length(ConnDevicesData)-1].Left + DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin then
        //  NextFormLeftCoordinate := NextFormLeftCoordinate - (DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin);
        //Delete(ConnDevicesData, idx, 1);
        //Delete(DeviceForm, idx, 1);
      end;
    end;
    Inc(idx);
  end;
end;


{ connect to device }
procedure ConnectDevice(ScIdx: Integer);
var
  i, j: Integer;
  SvIdx, ChIdx, DeIdx: Integer;
  s, n: String;
  LabelNoConnect: TLabel;
  NextElementVertical, NextPanelVertical, NextButtonCoord: Integer;
  Buffer: array of Byte;
  ChData: PByte;
  ChLen: NativeUInt;
begin
  SetLength(Buffer, CharDescMaxLength);
  ChData := PByte(Buffer);

  // increment devices and increase some arrays
  i := Length(ConnDevicesData);
  SetLength(ConnDevicesData, i+1);
  SetLength(DeviceForm, i+1);
  SetLength(DeviceFormElements, i+1);

  // grab existing scan data
  ConnDevicesData[i].ScnIdx           := ScIdx;
  ConnDevicesData[i].DeviceName       := PeripheralScanData[ScIdx].DeviceName;
  ConnDevicesData[i].MacAddress       := PeripheralScanData[ScIdx].MacAddress;
  ConnDevicesData[i].PeripheralHandle := PeripheralScanData[ScIdx].PeripheralHandle;

  SimpleBlePeripheralSetCallbackOnDisconnected(ConnDevicesData[i].PeripheralHandle, @DeviceOnDisconnect, Nil);

  // mark device in scan data as connected
  PeripheralScanData[ScIdx].IsConnected := true;

  // compose form for the new device
  ScanForm.LogOutput.Append('Connecting...');
  Application.CreateForm(TDeviceForm, DeviceForm[i]);
  DeviceForm[i].Tag  := i;
  DeviceForm[i].ButtonDisconnect.Tag := i;
  DeviceForm[i].Top  := ScanForm.Top;
  DeviceForm[i].Left := NextFormLeftCoordinate;
  if ConnDevicesData[i].DeviceName = '' then begin
    DeviceForm[i].Caption                   := '"<unknown name>" [' + UpperCase(ConnDevicesData[i].MacAddress) + '] - Connecting...';
    DeviceForm[i].TextBoxDeviceName.Caption := '<unknown name>';
    DeviceForm[i].TextBoxDeviceName.Enabled := false;
  end else begin
    DeviceForm[i].Caption                   := '"' + ConnDevicesData[i].DeviceName + '" [' + UpperCase(ConnDevicesData[i].MacAddress) + '] - Connecting...';
    DeviceForm[i].TextBoxDeviceName.Caption := ConnDevicesData[i].DeviceName;
  end;
  DeviceForm[i].LabelMacAddress.Caption   := 'MAC Address [' + UpperCase(ConnDevicesData[i].MacAddress) + ']';
  DeviceForm[i].Show;
  NextFormLeftCoordinate := NextFormLeftCoordinate + DeviceForm[i].Width + NextFormMargin;

  Application.ProcessMessages;  // shows the form and elements before connecting because connecting blocks

  // try to connect to device
  if SimpleBlePeripheralConnect(ConnDevicesData[i].PeripheralHandle) = SIMPLEBLE_FAILURE then begin
    ScanForm.LogOutput.Append('Failed to connect to "' + ConnDevicesData[i].DeviceName + '" [' + UpperCase(ConnDevicesData[i].MacAddress) + ']');
    DeviceForm[i].Caption := '"' + ConnDevicesData[i].DeviceName + '" [' + UpperCase(ConnDevicesData[i].MacAddress) + '] - Could not connect.';
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
    ConnDevicesData[i].IsConnected := true;
    ScanForm.LogOutput.Append('Connected to "' + ConnDevicesData[i].DeviceName + '" [' + UpperCase(ConnDevicesData[i].MacAddress) + ']');
    DeviceForm[i].Caption := '"' + ConnDevicesData[i].DeviceName + '" [' + UpperCase(ConnDevicesData[i].MacAddress) + '] - Connected.';
  end;

  // check for GATT services, characteristics and descriptors
  ScanForm.LogOutput.Append('Reading GATT table:');
  if SimpleBlePeripheralServicesCount(ConnDevicesData[i].PeripheralHandle) > 0 then begin
    ConnDevicesData[i].ServicesCount := SimpleBlePeripheralServicesCount(ConnDevicesData[i].PeripheralHandle);
    SetLength(ConnDevicesData[i].Services,                   ConnDevicesData[i].ServicesCount);
    SetLength(ConnDevicesData[i].Characteristic,             ConnDevicesData[i].ServicesCount);
    SetLength(ConnDevicesData[i].Descriptor,                 ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].Panel,                   ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelServiceUuid,        ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelCharacteristicUuid, ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].LabelDescriptorUuid,     ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].TextBoxCharacteristic,   ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].TextBoxDescriptor,       ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].ToggleBoxCharProp,       ConnDevicesData[i].ServicesCount);
    SetLength(DeviceFormElements[i].CheckBoxHexAscii,        ConnDevicesData[i].ServicesCount);
    NextPanelVertical := DeviceFormPaddingVertical;

    for SvIdx := 0 to ConnDevicesData[i].ServicesCount-1 do begin

      // create panel for service characteristics and descriptors
      DeviceFormElements[i].Panel[SvIdx]        := TPanel.Create(DeviceForm[i]);
      DeviceFormElements[i].Panel[SvIdx].Parent := DeviceForm[i].ScrollBoxGatt;
      DeviceFormElements[i].Panel[SvIdx].Top    := NextPanelVertical;
      DeviceFormElements[i].Panel[SvIdx].Left   := DeviceFormPaddingHorizontal;
      DeviceFormElements[i].Panel[SvIdx].Width  := DeviceForm[i].ScrollBoxGatt.Width - 4*DeviceFormPaddingHorizontal;
      DeviceFormElements[i].Panel[SvIdx].Color  := clSkyBlue;
      NextElementVertical := DeviceFormPaddingVertical;
      SimpleBlePeripheralServicesGet(ConnDevicesData[i].PeripheralHandle, SvIdx, ConnDevicesData[i].Services[SvIdx]);
      SetString(s, ConnDevicesData[i].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

      // look for assigned service numbers or proprietary vsp services
      n := BleAssignedServiceUuidToName(s);
      if n = '' then begin
        n := BleVspServiceUuidToName(s);
        if n = '' then
          ScanForm.LogOutput.Append('     SV: ' + s)
        else
          ScanForm.LogOutput.Append('     SV: ' + s + ' (' + n + ')');
      end else
        ScanForm.LogOutput.Append('     SV: ' + s + ' (' + n + ')');

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
      NextElementVertical := DeviceFormElements[i].LabelServiceUuid[SvIdx].Top + DeviceFormElements[i].LabelServiceUuid[SvIdx].Height + DeviceFormPaddingVertical div 2;

      if ConnDevicesData[i].Services[SvIdx].CharacteristicCount > 0 then begin
        SetLength(ConnDevicesData[i].Characteristic[SvIdx],             ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(ConnDevicesData[i].Descriptor[SvIdx],                 ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].LabelCharacteristicUuid[SvIdx], ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].LabelDescriptorUuid[SvIdx],     ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].TextBoxCharacteristic[SvIdx],   ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].TextBoxDescriptor[SvIdx],       ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].ToggleBoxCharProp[SvIdx],       ConnDevicesData[i].Services[SvIdx].CharacteristicCount);
        SetLength(DeviceFormElements[i].CheckBoxHexAscii[SvIdx],        ConnDevicesData[i].Services[SvIdx].CharacteristicCount);

        for ChIdx := 0 to ConnDevicesData[i].Services[SvIdx].CharacteristicCount-1 do begin
          SetLength(ConnDevicesData[i].Characteristic[SvIdx][ChIdx].data, CharDescMaxLength);
          SetString(s, ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
          n := BleAssignedCharacteristicUuidToName(s);
          if n = '' then begin
            n := BleVspCharacteristicUuidToName(s);
            if n = '' then
              ScanForm.LogOutput.Append('         CH: ' + s)
          end else begin
            ScanForm.LogOutput.Append('         CH: ' + s + ' (' + n + ')');
          end;

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
          DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Tag       := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);;
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
          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].CanRead then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Width      := 4*DeviceFormPaddingHorizontal;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Height     := 20;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Font.Size  := 8;
            //DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Font.Style := [fsBold];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].Caption    := 'RD';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanRead].OnChange   := @DeviceForm[i].ButtonCharRead;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + DeviceFormPropPadding;

            // read characteristic value, store into device record and display in text box
            SimpleBlePeripheralRead(ConnDevicesData[i].PeripheralHandle, ConnDevicesData[i].Services[SvIdx].Uuid,
                                                                         ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                         ChData, ChLen);
            for j := 0 to ChLen-1 do
              ConnDevicesData[i].Characteristic[SvIdx][ChIdx].data[j] := ChData[j];
            ConnDevicesData[i].Characteristic[SvIdx][ChIdx].len := ChLen;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToHex(ConnDevicesData[i].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[i].Characteristic[SvIdx][ChIdx].len);
          end;
          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].CanWriteCommand then begin
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].ReadOnly := false;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Width      := 4*DeviceFormPaddingHorizontal;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Height     := 20;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Font.Size  := 8;
            //DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Font.Style := [fsBold];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].Caption    := 'WRc';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteCommand].OnChange   := @DeviceForm[i].ButtonCharWriteCommand;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + DeviceFormPropPadding;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].OnEditingDone := @DeviceForm[i].CharEditingDone;
          end;
          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].CanWriteRequest then begin
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].ReadOnly := false;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Width      := 4*DeviceFormPaddingHorizontal;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Height     := 20;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Font.Size  := 8;
            //DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Font.Style := [fsBold];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].Caption    := 'WRr';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanWriteRequest].OnChange   := @DeviceForm[i].ButtonCharWriteRequest;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + DeviceFormPropPadding;
            DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].OnEditingDone := @DeviceForm[i].CharEditingDone;
          end;
          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].CanNotify then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Width      := 4*DeviceFormPaddingHorizontal;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Height     := 20;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Font.Size  := 8;
            //DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Font.Style := [fsBold];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].Caption    := 'NOT';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanNotify].OnChange   := @DeviceForm[i].ButtonCharNotify;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + DeviceFormPropPadding;
          end;
          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].CanIndicate then begin
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate]            := TToggleBox.Create(DeviceForm[i]);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Parent     := DeviceFormElements[i].Panel[SvIdx];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Tag        := (i shl TagPosDev) or (SvIdx shl TagPosSrv) or (ChIdx shl TagPosChr);
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Top        := DeviceFormElements[i].TextBoxCharacteristic[SvIdx][ChIdx].Top;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Left       := NextButtonCoord;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Width      := 4*DeviceFormPaddingHorizontal;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Height     := 20;
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Font.Size  := 8;
            //DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Font.Style := [fsBold];
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].Caption    := 'IND';
            DeviceFormElements[i].ToggleBoxCharProp[SvIdx][ChIdx][CanIndicate].OnChange   := @DeviceForm[i].ButtonCharIndicate;
            NextButtonCoord := NextButtonCoord + 4*DeviceFormPaddingHorizontal + DeviceFormPropPadding;
          end;

          if ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount > 0 then begin
            SetLength(ConnDevicesData[i].Descriptor[SvIdx][ChIdx],             ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            SetLength(DeviceFormElements[i].LabelDescriptorUuid[SvIdx][ChIdx], ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            SetLength(DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx],   ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount);
            NextElementVertical := NextElementVertical - DeviceFormPaddingVertical div 2;

            for DeIdx := 0 to ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].DescriptorCount-1 do begin
              SetString(s, ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].Descriptors[DeIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
              n := BleAssignedDescriptorUuidToName(s);
              if n = '' then
                ScanForm.LogOutput.Append('             DE: ' + s)
              else
                ScanForm.LogOutput.Append('             DE: ' + s + ' (' + n + ')');

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
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Width     := 40;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Height    := 20;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Color     := clForm;
              NextElementVertical := DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Top + DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Height + DeviceFormPaddingVertical div 2;
              // read descriptor value, store in device data record and display in text box
              SetLength(ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].data, CharDescMaxLength);
              ChData := PByte(ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].data);
              SimpleBlePeripheralReadDescriptor(ConnDevicesData[i].PeripheralHandle, ConnDevicesData[i].Services[SvIdx].Uuid,
                                                                                     ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                                     ConnDevicesData[i].Services[SvIdx].Characteristics[ChIdx].Descriptors[DeIdx].Uuid,
                                                                                     ChData, ChLen);
              for j := 0 to ChLen-1 do
                ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].data[j] := ChData[j];
              ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].len := ChLen;
              DeviceFormElements[i].TextBoxDescriptor[SvIdx][ChIdx][DeIdx].Caption := DataToHex(ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].data, ConnDevicesData[i].Descriptor[SvIdx][ChIdx][DeIdx].len);
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
  if ConnDevicesData[idx].IsConnected then begin
    ConnDevicesData[idx].IsConnected := false;
    ScanForm.LogOutput.Append('Disconnecting...');
    DeviceForm[idx].Caption := ConnDevicesData[idx].DeviceName + '" [' + UpperCase(ConnDevicesData[idx].MacAddress) + '] - Disconnecting...';
    Application.ProcessMessages;
    if SimpleBlePeripheralDisconnect(ConnDevicesData[idx].PeripheralHandle) = SIMPLEBLE_FAILURE then begin
      ScanForm.LogOutput.Append('Failed to disconnect from "' + ConnDevicesData[idx].DeviceName + '" [' + UpperCase(ConnDevicesData[idx].MacAddress) + ']');
      DeviceForm[idx].Caption := ConnDevicesData[idx].DeviceName + '" [' + UpperCase(ConnDevicesData[idx].MacAddress) + '] - Failed to disconnect.';
    end else begin
      ScanForm.LogOutput.Append('Disconnected from "' + ConnDevicesData[idx].DeviceName + '" [' + UpperCase(ConnDevicesData[idx].MacAddress) + ']');
      DeviceForm[idx].Caption := ConnDevicesData[idx].DeviceName + '" [' + UpperCase(ConnDevicesData[idx].MacAddress) + '] - Disconnected.';
      //SimpleBlePeripheralReleaseHandle(ConnDevicesData[idx].PeripheralHandle);
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
procedure TDeviceForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  idx: Integer;
begin
  idx := TForm(Sender).Tag;
  DisconnectDevice(idx);
  ScanRestoreConnectButton(ConnDevicesData[idx].PeripheralHandle);
  if NextFormLeftCoordinate <= DeviceForm[Length(ConnDevicesData)-1].Left + DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin then
    NextFormLeftCoordinate := NextFormLeftCoordinate - (DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin);
  Delete(ConnDevicesData, idx, 1);
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
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToAscii(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show as hex
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len);
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

  SetString(sSv, ConnDevicesData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  if(SimpleBlePeripheralRead(ConnDevicesData[DeIdx].PeripheralHandle, ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                                                                      ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                      ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    ScanForm.LogOutput.Append('Read failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('Read failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin
    for i := 0 to ChLen-1 do
      ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := ChData[i];
    ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len := ChLen;
    ScanForm.LogOutput.Append('Read: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len));
    if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
      DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToAscii(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len)
    else  // show data as hex string
      DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len);
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

  SetString(sSv, ConnDevicesData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  ChLen := ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len;
  for i := 0 to ChLen-1 do
    Buffer[i] := ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i];

  ChData := PByte(Buffer);
  if(SimpleBlePeripheralWriteCommand(ConnDevicesData[DeIdx].PeripheralHandle, ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                                                                              ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                              ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    ScanForm.LogOutput.Append('WriteCommand failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('WriteCommand failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else
    ScanForm.LogOutput.Append('WriteCommand: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len));

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

  SetString(sSv, ConnDevicesData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  TToggleBox(Sender).Font.Style := [fsBold];
  Application.ProcessMessages;

  ChLen := ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len;
  for i := 0 to ChLen-1 do
    Buffer[i] := ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i];

  ChData := PByte(Buffer);
  if(SimpleBlePeripheralWriteRequest(ConnDevicesData[DeIdx].PeripheralHandle, ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                                                                              ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                                                                              ChData, ChLen) = SIMPLEBLE_FAILURE) then
  begin
    ScanForm.LogOutput.Append('WriteRequest failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
    ShowMessage('WriteRequest failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else
    ScanForm.LogOutput.Append('WriteRequest: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                              ' Data=' + DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len));

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
  for i := 0 to Length(ConnDevicesData)-1 do begin
    for j := 0 to ConnDevicesData[i].ServicesCount-1 do begin
      SetString(s, ConnDevicesData[i].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
      if s = sSv then
        for k := 0 to ConnDevicesData[i].Services[j].CharacteristicCount-1 do begin
          SetString(s, ConnDevicesData[i].Services[j].Characteristics[k].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
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
    ScanForm.LogOutput.Append('Notification: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" Unknown UUIDs.');
    Exit;
  end;

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  Application.ProcessMessages;

  for i := 0 to Len-1 do
    ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Data[i];
  ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len := Len;

  if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToAscii(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show data as hex string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len);

  ScanForm.LogOutput.Append('Notification: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                            ' Data=' + DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len));
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

  SetString(sSv, ConnDevicesData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  if TToggleBox(Sender).State = cbChecked then begin  // subscribe
    if(SimpleBlePeripheralNotify(ConnDevicesData[DeIdx].PeripheralHandle,
                              ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                              ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                              @PeripheralOnNotify, Nil) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      ScanForm.LogOutput.Append('Subscribing failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Subscribing failed.');
      TToggleBox(Sender).Font.Style := [];
    end else  // success...
      TToggleBox(Sender).Font.Style := [fsBold];
      ScanForm.LogOutput.Append('Subscribed notifications: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin  // unsubscribe
    TToggleBox(Sender).Font.Style := [];
    if(SimpleBlePeripheralUnsubscribe(ConnDevicesData[DeIdx].PeripheralHandle,
                                   ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                                   ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      ScanForm.LogOutput.Append('Unsubscribing failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Unsubscribing failed.');
    end else  // success
      ScanForm.LogOutput.Append('Unsubscribed notifications: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
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
  for i := 0 to Length(ConnDevicesData)-1 do begin
    for j := 0 to ConnDevicesData[i].ServicesCount-1 do begin
      SetString(s, ConnDevicesData[i].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
      if s = sSv then
        for k := 0 to ConnDevicesData[i].Services[j].CharacteristicCount-1 do begin
          SetString(s, ConnDevicesData[i].Services[j].Characteristics[k].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
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
    ScanForm.LogOutput.Append('Indication: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" Unknown UUIDs.');
    Exit;
  end;

  DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Color := clGradientInactiveCaption;
  Application.ProcessMessages;

  for i := 0 to Len-1 do
    ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Data[i];
  ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len := Len;

  if DeviceFormElements[DeIdx].CheckBoxHexAscii[SvIdx][ChIdx].State = cbChecked then  // show data as ascii string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToAscii(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len)
  else  // show data as hex string
    DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption := DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len);

  ScanForm.LogOutput.Append('Indication: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4) +
                            ' Data=' + DataToHex(ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data, ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len));
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

  SetString(sSv, ConnDevicesData[DeIdx].Services[SvIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
  SetString(sCh, ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);

  if TToggleBox(Sender).State = cbChecked then begin  // subscribe
    if(SimpleBlePeripheralIndicate(ConnDevicesData[DeIdx].PeripheralHandle,
                              ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                              ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid,
                              @PeripheralOnIndicate, Nil) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      ScanForm.LogOutput.Append('Subscribing failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Subscribing failed.');
      TToggleBox(Sender).Font.Style := [];
    end else  // success...
      TToggleBox(Sender).Font.Style := [fsBold];
      ScanForm.LogOutput.Append('Subscribed indications: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
  end else begin  // unsubscribe
    TToggleBox(Sender).Font.Style := [];
    if(SimpleBlePeripheralUnsubscribe(ConnDevicesData[DeIdx].PeripheralHandle,
                                   ConnDevicesData[DeIdx].Services[SvIdx].Uuid,
                                   ConnDevicesData[DeIdx].Services[SvIdx].Characteristics[ChIdx].Uuid) = SIMPLEBLE_FAILURE) then
    begin  // failed...
      ScanForm.LogOutput.Append('Unsubscribing failed: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
      ShowMessage('Unsubscribing failed.');
    end else  // success
      ScanForm.LogOutput.Append('Unsubscribed indications: [' + ConnDevicesData[DeIdx].MacAddress + '] "' + ConnDevicesData[DeIdx].DeviceName + '" SV:' + Copy(sSv, 5, 4) + ' CH:' + Copy(sCh, 5, 4));
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
    Buffer := AsciiToData(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption, len)
  else begin // convert hex string to data
    if not StringIsHex(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption) then begin
      ShowMessage('Invalid hex characters or not multiple of 2');
      Exit;
    end;
    Buffer := HexToData(DeviceFormElements[DeIdx].TextBoxCharacteristic[SvIdx][ChIdx].Caption, len);
  end;

  ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].len := len;
  for i := 0 to len-1 do
    ConnDevicesData[DeIdx].Characteristic[SvIdx][ChIdx].data[i] := Buffer[i];
end;


{ Convert data bytes to ASCII string }
function DataToAscii(data: array of Byte; len: Integer): String;
var
  i: Integer;
  c: Byte;
  s: String;
begin
  s := '';
  i := 0;
  while i < len do begin
    c := data[i];
    if (c >= 32) and (c <= 127) then
      s := s + AnsiChar(c)
    else
      s := s + '■';  // ALT-254 should be a solid square block to indicate non-ascii bytes
    Inc(i);
  end;
  Result := s;
end;


{ Convert data bytes to HEX string }
function DataToHex(data: array of Byte; len: Integer): String;
var
  i: Integer;
  s: String;
begin
  s := '';
  i := 0;
  while i < len do begin
    s := s + IntToHex(data[i], 2);
    Inc(i);
  end;
  Result := s;
end;


{ Convert ASCII string to array of bytes}
function AsciiToData(data: string; var len: Integer): TArrayOfByte;
var
  i: Integer;
  s: TArrayOfByte;
begin
  SetLength(s, CharDescMaxLength);
  i := 0;
  while i < Length(data) do begin
    s[i] := Byte(data[i+1]);
    Inc(i);
  end;
  len := i;
  Result := s;
end;


{ Convert HEX string to array of bytes }
function HexToData(data: string; var len: Integer): TArrayOfByte;
var
  i: Integer;
  Buffer: TArrayOfByte;
  t: string;
begin
  SetLength(Buffer, CharDescMaxLength);
  len := Length(data) div 2;
  i := 0;
  while i < len do begin
    t := Copy(data, (i*2)+1, 2);  // get two characters (one hex number) from the string (index starts at one!)
    Buffer[i] := Byte(Hex2Dec(t));
    Inc(i);
  end;
  Result := Buffer;
end;


{ Test if string only contains valid hex numbers and is of even length }
function StringIsHex(s: string): Boolean;
var
  i, len: Integer;
  c: Byte;
begin
  len := Length(s);
  if Odd(len) then begin  // string must of even length
    Result := false;
    Exit;
  end;
  i := 1;
  while i <= len do begin
    c := Byte(s[i]);
    if not ((c >= 48) and (c <= 57) or (c >= 65) and (c <= 70) or (c >= 97) and (c <= 102)) then begin  // 0-9 or A-F or a-f
      Result := false;
      Exit;
    end;
    Inc(i);
  end;
  Result := true;
end;


{ Tick timer for device connect functions }
procedure TDeviceForm.ConnectTimerTimer(Sender: TObject);
var
  idx: Integer;
  c: Boolean;
begin
  idx := 0;
  c := false;
  while idx < Length(ConnDevicesData)-1 do begin
    if ConnDevicesData[idx].IsConnected then begin
      // check if device is still connected
      if SimpleBlePeripheralIsConnected(ConnDevicesData[idx].PeripheralHandle, c) = SIMPLEBLE_FAILURE then begin
        ScanForm.LogOutput.Append('SIMPLEBLE_FAILURE');
        ConnDevicesData[idx].IsConnected := false;
      end;
      // not connected anymore, so we issue a message, clean up and close form
      if not c then begin
        ConnDevicesData[idx].IsConnected := false;
        ScanForm.LogOutput.Append('Device "' + ConnDevicesData[idx].DeviceName + '" [' + ConnDevicesData[idx].MacAddress + '] disconnected unexpectedly.');
        ShowMessage('Device [' + ConnDevicesData[idx].MacAddress + '] disconnected unexpectedly.');
        DisconnectDevice(idx);
        ScanRestoreConnectButton(ConnDevicesData[idx].PeripheralHandle);
        Delete(ConnDevicesData, idx, 1);
        if NextFormLeftCoordinate <= DeviceForm[Length(ConnDevicesData)-1].Left + DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin then
          NextFormLeftCoordinate := NextFormLeftCoordinate - (DeviceForm[Length(ConnDevicesData)-1].Width + NextFormMargin);
        Delete(DeviceForm, idx, 1);
      end;
    end;
    Inc(idx);
  end;
end;


end.


