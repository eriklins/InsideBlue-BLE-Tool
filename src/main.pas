unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ClipBrd,
  SimpleBle, Ble, Scan, Connect, Help;

type
  { type: form elements for ble device scanning }
  TPanelBleScanElements = record
    PanelDeviceInfo:        TPanel;
    TextBoxDeviceName:      TEdit;
    ButtonConnectDevice:    TButton;
    LabelDeviceConnectable: TLabel;
    LabelTxPower:           TLabel;
    LabelRssi:              TLabel;
    LabelMacAddress:        TLabel;
    LabelServices:          array of TLabel;
    LabelManufData:         TLabel;
    LabelManufDataId:       array of TLabel;
    TextBoxManufData:       array of TEdit;
    CheckBoxHexAscii:       TCheckBox;
  end;

  { TScanForm }
  TScanForm = class(TForm)
    ButtonHelp: TButton;
    ButtonCopyLog: TButton;
    ButtonClearLog: TButton;
    ButtonStartScan: TButton;
    ButtonClear: TButton;
    CheckBoxShowLog: TCheckBox;
    EditFilterDeviceName: TEdit;
    EditFilterMacAddress: TEdit;
    EditFilterRssi: TEdit;
    LabelHeader: TLabel;
    LabelFilter: TLabel;
    LabelFilterRssi: TLabel;
    LabelFilterMacAddress: TLabel;
    LabelFilterDeviceName: TLabel;
    LabelNofDevices: TLabel;
    LogOutput: TMemo;
    ScrollBoxDevices: TScrollBox;
    ScanTimer: TTimer;
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonCopyLogClick(Sender: TObject);
    procedure ButtonStartScanClick(Sender: TObject);
    procedure CheckBoxShowLogChange(Sender: TObject);
    procedure EditFilterDeviceNameEditingDone(Sender: TObject);
    procedure EditFilterMacAddressEditingDone(Sender: TObject);
    procedure EditFilterRssiEditingDone(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnectDeviceClick(Sender: TObject);
    procedure CheckBoxHexAsciiClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScanTimerTimer(Sender: TObject);

    private

    public

  end;

const
  ScanPanelHeight = 132;
  ScanPanelPaddingHorizontal = 8;
  ScanPanelPaddingVertical = 4;
  NextFormMargin = 8;

var
  ScanForm: TScanForm;
  PeripheralScanPanel: array of TPanelBleScanElements;
  NextFormLeftCoordinate: Integer;

implementation

{$R *.lfm}


{ Tick timer for updating the scan device list }
procedure TScanForm.ScanTimerTimer(Sender: TObject);
var
  DevIdx, i, j: Integer;
  DevicePanelTop: Integer;
  s, n: String;
  c: Byte;
begin

  // check if BLE adapter is still enabled
  if SimpleBleAdapterIsBluetoothEnabled() <> BleIsEnabled then begin
    BleIsEnabled := SimpleBleAdapterIsBluetoothEnabled();
    if not BleIsEnabled then begin
      LabelNofDevices.Caption := 'Bluetooth is disabled.';
      ButtonStartScan.Enabled := false;
      FlagBleScanningActive := false;
      for i := 0 to Length(PeripheralScanPanel)-1 do begin
        PeripheralScanPanel[i].PanelDeviceInfo.Free;
        PeripheralScanPanel[i].PanelDeviceInfo := Nil;
      end;
      SetLength(PeripheralScanPanel, 0);
      ScanClearPeripheralList;
      LogOutput.Append('BLuetooth is disabled.');
      Exit;
    end else begin
      LogOutput.Append('BLuetooth is enabled.');
      LabelNofDevices.Caption := 'Press Start Scan to Discover Devices...';
      ButtonStartScan.Enabled := true;
      ButtonStartScan.Caption := 'Start Scan';
      FlagBleScanningActive := false;
      SimpleBleAdapterScanStop(BleAdapter);
      Exit;
    end;
  end;

  // create and/or update device form elements if scanning is active and we have found some devices
  if FlagBleScanningActive and (PeripheralNofDevices > 0) then begin

    // show headline with number of devices found
    LabelNofDevices.Caption := IntToStr(PeripheralNofDevices) + ' Devices Found';

    if Length(PeripheralScanPanel) < PeripheralNofDevices then
      SetLength(PeripheralScanPanel, PeripheralNofDevices);

    // walk through all existing devices
    for DevIdx := 0 to PeripheralNofDevices-1 do begin

      // calculate top coordinate of current device panel
      If DevIdx > 0 then
        DevicePanelTop := PeripheralScanPanel[DevIdx-1].PanelDeviceInfo.Top + PeripheralScanPanel[DevIdx-1].PanelDeviceInfo.Height + 2*ScanPanelPaddingVertical
      else
        DevicePanelTop := 2*ScanPanelPaddingVertical;

      // check if device's form elements needs an update
      if PeripheralScanData[DevIdx].UpdateForm then begin

        // if it's a newly discovered device we draw the form elements
        if PeripheralScanPanel[DevIdx].PanelDeviceInfo = Nil then begin
          PeripheralScanPanel[DevIdx].PanelDeviceInfo             := TPanel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Parent      := ScanForm.ScrollBoxDevices;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Top         := DevicePanelTop;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Left        := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width       := ScanForm.ScrollBoxDevices.Width - 32;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height      := ScanPanelHeight;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Color       := clSkyBlue;

          PeripheralScanPanel[DevIdx].TextBoxDeviceName            := TEdit.Create(ScanForm);
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Parent     := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.ReadOnly   := True;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Top        := 2*ScanPanelPaddingVertical;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Left       := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Width      := Trunc((ScanForm.ScrollBoxDevices.Width - 32) * 2 / 3);
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Color      := clWhite;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Font.Size  := 10;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Font.Style := [fsBold];
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.AutoSize   := True;
          PeripheralScanPanel[DevIdx].TextBoxDeviceName.Caption    := 'Device Name: ';    // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].ButtonConnectDevice           := TButton.Create(ScanForm);
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Caption   := 'Connect';
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Width     := (ScanForm.ScrollBoxDevices.Width - 32) - PeripheralScanPanel[DevIdx].TextBoxDeviceName.Width - (3*ScanPanelPaddingHorizontal);
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Height    := PeripheralScanPanel[DevIdx].TextBoxDeviceName.Height;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Top       := 2*ScanPanelPaddingVertical;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Left      := PeripheralScanPanel[DevIdx].TextBoxDeviceName.Width + 2*ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Font.Size := 10;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Tag       := DevIdx;
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.OnClick   := @ScanForm.ButtonConnectDeviceClick;

          PeripheralScanPanel[DevIdx].LabelDeviceConnectable           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top       := PeripheralScanPanel[DevIdx].TextBoxDeviceName.Height + 2*ScanPanelPaddingVertical;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Left      := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Caption   :=  'Connectable: Yes';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].LabelTxPower           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelTxPower.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelTxPower.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top;
          PeripheralScanPanel[DevIdx].LabelTxPower.Left      := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Left + PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Width + (4*ScanPanelPaddingHorizontal);
          PeripheralScanPanel[DevIdx].LabelTxPower.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: n/a';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].LabelRssi           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelRssi.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelRssi.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top;
          PeripheralScanPanel[DevIdx].LabelRssi.Left      := PeripheralScanPanel[DevIdx].LabelTxPower.Left + PeripheralScanPanel[DevIdx].LabelTxPower.Width + (4*ScanPanelPaddingHorizontal);
          PeripheralScanPanel[DevIdx].LabelRssi.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelRssi.Caption   := 'RSSI: -99 dBm';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].LabelMacAddress           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelMacAddress.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top + PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Height + Trunc(ScanPanelPaddingVertical/2);
          PeripheralScanPanel[DevIdx].LabelMacAddress.Left      := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Caption   := 'MAC Address:';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelMacAddress.Top + PeripheralScanPanel[DevIdx].LabelMacAddress.Height + 2*ScanPanelPaddingVertical;

          j := PeripheralScanPanel[DevIdx].LabelMacAddress.Top + PeripheralScanPanel[DevIdx].LabelMacAddress.Height + Trunc(ScanPanelPaddingVertical/2);

          // if we have advertised services we add lebels for each
          if PeripheralScanData[DevIdx].ServicesCount > 0 then begin
            SetLength(PeripheralScanPanel[DevIdx].LabelServices, PeripheralScanData[DevIdx].ServicesCount);
            for i := 0 to PeripheralScanData[DevIdx].ServicesCount-1 do begin
              PeripheralScanPanel[DevIdx].LabelServices[i]           := TLabel.Create(ScanForm);
              PeripheralScanPanel[DevIdx].LabelServices[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
              PeripheralScanPanel[DevIdx].LabelServices[i].Caption   := 'Service: ';
              PeripheralScanPanel[DevIdx].LabelServices[i].Top       := j;
              PeripheralScanPanel[DevIdx].LabelServices[i].Left      := ScanPanelPaddingHorizontal;
              PeripheralScanPanel[DevIdx].LabelServices[i].Font.Size := 10;
              j := PeripheralScanPanel[DevIdx].LabelServices[i].Top + PeripheralScanPanel[DevIdx].LabelServices[i].Height + Trunc(ScanPanelPaddingVertical/2);
            end;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelServices[i].Top + PeripheralScanPanel[DevIdx].LabelServices[i].Height + 2*ScanPanelPaddingVertical;
          end;

          // if we have manufacturer specific data we add a label and a check box
          if PeripheralScanData[DevIdx].ManufacturerDataCount > 0 then begin
            PeripheralScanPanel[DevIdx].LabelManufData           := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelManufData.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].LabelManufData.Caption   := 'Manufacturer Specific Data:';
            PeripheralScanPanel[DevIdx].LabelManufData.Top       := j;//PeripheralScanPanel[DevIdx].LabelMacAddress.Top + (i+1) * (PeripheralScanPanel[DevIdx].LabelMacAddress.Height + Trunc(ScanPanelPaddingVertical/2));
            PeripheralScanPanel[DevIdx].LabelManufData.Left      := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelManufData.Font.Size := 10;
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii         := TCheckBox.Create(ScanForm);
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Parent  := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Caption := 'View as ASCII';
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Top     := PeripheralScanPanel[DevIdx].LabelManufData.Top;
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Left    := PeripheralScanPanel[DevIdx].LabelManufData.Left + PeripheralScanPanel[DevIdx].LabelManufData.Width + (16*ScanPanelPaddingHorizontal);
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Tag     := DevIdx;
            PeripheralScanPanel[DevIdx].CheckBoxHexAscii.OnClick := @ScanForm.CheckBoxHexAsciiClick;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelManufData.Top + PeripheralScanPanel[DevIdx].LabelManufData.Height + 2*ScanPanelPaddingVertical;
          end;

        end;

        // check if manufacturing data is available and if we need to add form elements for that
        j := Length(PeripheralScanPanel[DevIdx].LabelManufDataId);
        if PeripheralScanData[DevIdx].ManufacturerDataCount > j then begin
          SetLength(PeripheralScanPanel[DevIdx].LabelManufDataId, PeripheralScanData[DevIdx].ManufacturerDataCount);
          SetLength(PeripheralScanPanel[DevIdx].TextBoxManufData, PeripheralScanData[DevIdx].ManufacturerDataCount);
          for i := 0 to PeripheralScanData[DevIdx].ManufacturerDataCount-1 do begin
            PeripheralScanPanel[DevIdx].LabelManufDataId[i]           := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption   := 'ID=0xFFFF';  // placeholder for proper positioning of further elements
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Top       := PeripheralScanPanel[DevIdx].LabelManufData.Top + 2 + ((i+1) * PeripheralScanPanel[DevIdx].LabelManufData.Height + (i+1) * Trunc(ScanPanelPaddingVertical/2) + i * Trunc(ScanPanelPaddingVertical/2));
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Left      := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].AutoSize  := false;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Width     := 72;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Height    := 20;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Font.Size := 10;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i]           := TEdit.Create(ScanForm);
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].ReadOnly  := True;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Top       := PeripheralScanPanel[DevIdx].LabelManufData.Top + 2 + ((i+1) * PeripheralScanPanel[DevIdx].LabelManufData.Height + (i+1) * Trunc(ScanPanelPaddingVertical/2) + i * Trunc(ScanPanelPaddingVertical/2));
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Left      := PeripheralScanPanel[DevIdx].LabelManufDataId[i].Width + (2*ScanPanelPaddingHorizontal);
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Width     := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - PeripheralScanPanel[DevIdx].LabelManufDataId[i].Width - (3*ScanPanelPaddingHorizontal);
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].AutoSize  := false;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Height    := 20;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Color     := clForm;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Font.Size := 10;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].TextBoxManufData[i].Top + PeripheralScanPanel[DevIdx].TextBoxManufData[i].Height + 2*ScanPanelPaddingVertical;
          end;
        end;

        // update device's form elements
        PeripheralScanPanel[DevIdx].TextBoxDeviceName.Caption    := PeripheralScanData[DevIdx].DeviceName;
        if PeripheralScanData[DevIdx].IsConnectable then begin
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Enabled := true;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Caption :=  'Connectable: Yes'
        end else begin
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Enabled := false;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Caption :=  'Connectable: No';
        end;
        if PeripheralScanData[DevIdx].TxPower = -32768 then
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: n/a'
        else
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: ' + IntToStr(PeripheralScanData[DevIdx].TxPower) + ' dBm';
        PeripheralScanPanel[DevIdx].LabelRssi.Caption   := 'RSSI: ' + IntToStr(PeripheralScanData[DevIdx].Rssi) + ' dBm';
        PeripheralScanPanel[DevIdx].LabelMacAddress.Caption   := 'MAC Address: ' + UpperCase(PeripheralScanData[DevIdx].MacAddress);

        // add services information if available
        if PeripheralScanData[DevIdx].ServicesCount > 0 then begin
          if PeripheralScanData[DevIdx].ServicesCount > Length(PeripheralScanPanel[DevIdx].LabelServices) then
            LogOutput.Append('ServicesCount too large [' + PeripheralScanData[DevIdx].MacAddress + ']')  // need to check on this...
          else
            for i := 0 to PeripheralScanData[DevIdx].ServicesCount -1 do begin
              SetString(s, PeripheralScanData[DevIdx].Services[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
              n := BleAssignedServiceUuidToName(s);
              if n = '' then begin
                n := BleVspServiceUuidToName(s);
                if n = '' then
                  PeripheralScanPanel[DevIdx].LabelServices[i].Caption  := 'Service: ' + s
                else begin
                  PeripheralScanPanel[DevIdx].LabelServices[i].Caption  := 'Service: ' + n;
                  PeripheralScanPanel[DevIdx].LabelServices[i].ShowHint := true;
                  PeripheralScanPanel[DevIdx].LabelServices[i].Hint     := s;
                end;
              end else begin
                PeripheralScanPanel[DevIdx].LabelServices[i].Caption  := 'Service: ' + n;
                PeripheralScanPanel[DevIdx].LabelServices[i].ShowHint := true;
                PeripheralScanPanel[DevIdx].LabelServices[i].Hint     := s;
              end;
            end;
        end;

        // add manufacturer specific data if available
        if PeripheralScanData[DevIdx].ManufacturerDataCount > 0 then begin
          if PeripheralScanData[DevIdx].ManufacturerDataCount > Length(PeripheralScanPanel[DevIdx].LabelManufDataId) then
            LogOutput.Append('[ERR] ManufacturerDataCount too large [' + PeripheralScanData[DevIdx].MacAddress + ']');
          PeripheralScanPanel[DevIdx].CheckBoxHexAscii.Enabled := true;
          for i := 0 to PeripheralScanData[DevIdx].ManufacturerDataCount-1 do begin
            n := BleAssignedCompanyIdToName(LowerCase(IntToHex(PeripheralScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4)));
            if n = '' then
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption  := 'ID=0x' + IntToHex(PeripheralScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4)
            else begin
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption  := n;
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].ShowHint := true;
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Hint     := n + ' (0x' + IntToHex(PeripheralScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4) + ')';
            end;


            if PeripheralScanPanel[DevIdx].CheckBoxHexAscii.State = cbChecked then
              PeripheralScanPanel[DevIdx].TextBoxManufData[i].Caption := DataToAscii(PeripheralScanData[DevIdx].ManufacturerData[i].Data, PeripheralScanData[DevIdx].ManufacturerData[i].DataLength)
            else
              PeripheralScanPanel[DevIdx].TextBoxManufData[i].Caption := DataToHex(PeripheralScanData[DevIdx].ManufacturerData[i].Data, PeripheralScanData[DevIdx].ManufacturerData[i].DataLength);
          end;
        end;

        // clear flag since we updated everything
        PeripheralScanData[DevIdx].UpdateForm := false;
      end;
    end;
  end;
end;


{ ScanForm functions }

{ create main form and initialize SimpleBLE adapter and callbacks }
procedure TScanForm.FormCreate(Sender: TObject);
begin

  // resize main form to hide log output
  Self.Constraints.MaxWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
  Self.Constraints.MinWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
  Self.Width := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
  NextFormLeftCoordinate := Self.Left + Self.Width + NextFormMargin;

  LogOutput.Clear;

  // initialize BLE units
  BleInit;
  ScanInit;
  ConnectInit;

  // init help form
  if not HelpInit() then
    ButtonHelp.Enabled := false;

  // check if BLE adapter could be initialized and Bluetooth is enabled
  if not BleAdapterIsInitialized then begin
    ButtonStartScan.Enabled := false;
    LabelNofDevices.Caption := 'Could not initialize BLE adapter.';
    LogOutput.Append('Could not initialize BLE adapter.');
  end else if not BleIsEnabled then begin
    ButtonStartScan.Enabled := false;
    LabelNofDevices.Caption := 'Bluetooth is disabled.';
    LogOutput.Append('BLuetooth is disabled.');
  end;
end;


{ Help button clicked }
procedure TScanForm.ButtonHelpClick(Sender: TObject);
begin
  ButtonHelp.Enabled := false;
  HelpShow;
end;


{ Start / Stop Scan button clicked }
procedure TScanForm.ButtonStartScanClick(Sender: TObject);
var
  i: Integer;
begin

  if not FlagBleScanningActive then begin
    FlagBleScanningActive := true;
    EditFilterDeviceName.Enabled := false;
    EditFilterMacAddress.Enabled := false;
    EditFilterRssi.Enabled := false;
    ButtonClear.Enabled := false;
    ButtonStartScan.Caption := 'Stop Scan';
    LabelNofDevices.Caption := IntToStr(PeripheralNofDevices) + ' Devices Found';
    SimpleBleAdapterScanStart(BleAdapter);
  end else begin
    FlagBleScanningActive := false;
    EditFilterDeviceName.Enabled := true;
    EditFilterMacAddress.Enabled := true;
    EditFilterRssi.Enabled := true;
    ButtonClear.Enabled := true;
    ButtonStartScan.Caption := 'Start Scan';
    LabelNofDevices.Caption := 'Press Start Scan to Discover Devices...';
    SimpleBleAdapterScanStop(BleAdapter);
  end;

end;


{ Clear scan list button clicked }
procedure TScanForm.ButtonClearClick(Sender: TObject);
var
  i: Integer;
begin
  // delete form elements of devices
  for i := 0 to Length(PeripheralScanPanel)-1 do begin
    PeripheralScanPanel[i].PanelDeviceInfo.Free;
    PeripheralScanPanel[i].PanelDeviceInfo := Nil;
  end;
  // clear record
  SetLength(PeripheralScanPanel, 0);
  // clear all peripheral devices
  ScanClearPeripheralList;
end;


{ BLE scan filter values (device name, mac address and rssi }
procedure TScanForm.EditFilterDeviceNameEditingDone(Sender: TObject);
begin
  PeripheralScanFilter.DeviceName := EditFilterDeviceName.Caption;
end;

procedure TScanForm.EditFilterMacAddressEditingDone(Sender: TObject);
begin
  PeripheralScanFilter.MacAddress := LowerCase(EditFilterMacAddress.Caption);
end;

procedure TScanForm.EditFilterRssiEditingDone(Sender: TObject);
begin
  if EditFilterRssi.Caption <> '' then
    PeripheralScanFilter.Rssi := -StrToInt(EditFilterRssi.Caption)
  else
    PeripheralScanFilter.Rssi := 0;
end;


{ Connect to device button clicked }
procedure TScanForm.ButtonConnectDeviceClick(Sender: TObject);
begin
  tButton(Sender).Enabled := false;
  ConnectDevice(tButton(Sender).Tag);
end;


{ Checkbox Ascii Hex clicked }
procedure TScanForm.CheckBoxHexAsciiClick(Sender: TObject);
var
  DevIdx: Integer;
  i, j: Integer;
  c: Byte;
  s: String;
begin
  DevIdx := tButton(Sender).Tag;

  j := 0;
  while j < PeripheralScanData[DevIdx].ManufacturerDataCount do begin
    if PeripheralScanPanel[DevIdx].CheckBoxHexAscii.State = cbChecked then begin
      s := '';
      for i := 0 to PeripheralScanData[DevIdx].ManufacturerData[j].DataLength-1 do begin
        c := PeripheralScanData[DevIdx].ManufacturerData[j].Data[i];
        if (c >= 32) and (c <= 127) then
          s := s + AnsiChar(c)
        else
          s := s + '■';  // ALT-254 should be a solid square block to indicate non-ascii bytes
      end;
    end else begin
      s := '';
      for i := 0 to PeripheralScanData[DevIdx].ManufacturerData[j].DataLength-1 do begin
        s := s + IntToHex(PeripheralScanData[DevIdx].ManufacturerData[j].Data[i], 2)
      end;
    end;
    PeripheralScanPanel[DevIdx].TextBoxManufData[j].Caption := s;
    Inc(j);
  end;
end;


{ log output checkbox clicked }
procedure TScanForm.CheckBoxShowLogChange(Sender: TObject);
begin
  if CheckBoxShowLog.State = cbChecked then begin
    // increase scan form width to show log output
    ScanForm.Constraints.MaxWidth := 0;
    ScanForm.Constraints.MinWidth := ButtonClearLog.Left + ButtonClearLog.Width + ButtonCopyLog.Width + 2*ScanPanelPaddingHorizontal;
    ScanForm.Width := ScrollBoxDevices.Left + ScrollBoxDevices.Width + 456 + ScanPanelPaddingHorizontal;
    ScanForm.ButtonCopyLog.Left := ScanForm.Width - ScanForm.ButtonCopyLog.Width - ScanPanelPaddingHorizontal;
    ScanForm.ButtonCopyLog.Anchors := [akTop, akRight];
    NextFormLeftCoordinate := ScanForm.Left + ScanForm.Width + NextFormMargin;
  end else begin
    // decrease scan form width to hide log output
    ScanForm.ButtonCopyLog.Anchors := [akTop, akLeft];
    ScanForm.Constraints.MaxWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    ScanForm.Constraints.MinWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    ScanForm.Width := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    NextFormLeftCoordinate := ScanForm.Left + ScanForm.Width + NextFormMargin;
  end;
end;


{ Clear Log button clicked }
procedure TScanForm.ButtonClearLogClick(Sender: TObject);
begin
  LogOutput.Clear;
end;


{ button copy log to clipboard clicked }
procedure TScanForm.ButtonCopyLogClick(Sender: TObject);
begin
  Clipboard.AsText := LogOutput.Text;
end;


{ Form changed bounds, used to set coordinates for device form }
procedure TScanForm.FormChangeBounds(Sender: TObject);
begin
  NextFormLeftCoordinate := ScanForm.Left + ScanForm.Width + NextFormMargin;
end;


{ On exit we release the ble adapter handle }
procedure TScanForm.FormDestroy(Sender: TObject);
begin
  SimpleBleAdapterReleaseHandle(BleAdapter);
end;



end.

