unit Main;

{$mode objfpc}{$H+}

{ InsideBlue BLE Tool
  ===================
  Welcome to InsideBlue BLE Tool, an easy to use cross-platform
  application for Bluetooth Low-Energy (BLE) connectivity leveraging
  the PC’s integrated Bluetooth adapter.

  You can scan for BLE devices that are advertising and discover
  their GAP services or manufacturer specific data, engage a
  connection and access the peripheral GATT table.

  For more details click on help button or check README.md.

  Project
    InsideBlue is at github: https://github.com/eriklins/InsideBlue-BLE-Tool

  BLE Library
    Inside Blue BLE Tool is based on the SimpleBLE library
    (https://github.com/OpenBluetoothToolbox/SimpleBLE)
    and uses the Pascal bindings for that library
    (https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library).

  License
    Copyright (C) 2023 Erik Lins
    This project is released under the MIT License (see README.md)
}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ClipBrd,
  SimpleBle, Util, Ble, Scan, Connect, Help;

type
  { type: form elements for ble device scanning }
  TPanelBleScanElements = record
    PanelDeviceInfo:        TPanel;
    TextBoxDeviceName:      TEdit;
    ButtonConnectDevice:    TButton;
    LabelDeviceConnectable: TLabel;
    LabelTxPower:           TLabel;
    LabelRssi:              TLabel;
    LabelPaired:            TLabel;
    LabelMacAddress:        TLabel;
    LabelServices:          TLabel;
    LabelServiceUuid:       array of TLabel;
    LabelServiceData:       TLabel;
    TextBoxServiceData:     array of TEdit;
    CheckBoxSrvsHexAscii:   array of TCheckBox;
    LabelManufData:         TLabel;
    LabelManufDataId:       array of TLabel;
    TextBoxManufData:       array of TEdit;
    CheckBoxManufHexAscii:  array of TCheckBox;
    BevelSeparatorServices: TBevel;
    BevelSeparatorManufact: TBevel;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnectDeviceClick(Sender: TObject);
    procedure CheckBoxManufHexAsciiClick(Sender: TObject);
    procedure CheckBoxServiceHexAsciiClick(Sender: TObject);
    procedure ScanTimerTimer(Sender: TObject);

    private

    public

  end;

const
  ScanPanelHeight = 132;
  ScanPanelPaddingHorizontal = 8;
  ScanPanelPaddingVertical = 4;
  ScanPanelColor = $EED4C0;

var
  ScanForm: TScanForm;
  PeripheralScanPanel: array of TPanelBleScanElements;
  ScanFormWidth: Integer = 0;

implementation

{$R *.lfm}


{ Tick timer for updating the scan device list }
procedure TScanForm.ScanTimerTimer(Sender: TObject);
var
  DevIdx, i, j: Integer;
  DevicePanelTop: Integer;
  s, n: String;
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
      UtilLog('Bluetooth is disabled.');
      Exit;
    end else begin
      UtilLog('Bluetooth is enabled.');
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
      if BleScanData[DevIdx].UpdateForm then begin

        // if it's a newly discovered device we draw the form elements
        if PeripheralScanPanel[DevIdx].PanelDeviceInfo = Nil then begin
          PeripheralScanPanel[DevIdx].PanelDeviceInfo             := TPanel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Parent      := ScanForm.ScrollBoxDevices;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Top         := DevicePanelTop;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Left        := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width       := ScanForm.ScrollBoxDevices.Width - 32;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height      := ScanPanelHeight;
          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Color       := TColor(ScanPanelColor);

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
          PeripheralScanPanel[DevIdx].LabelTxPower.Left      := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Left + PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Width + (3*ScanPanelPaddingHorizontal);
          PeripheralScanPanel[DevIdx].LabelTxPower.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: n/a';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].LabelRssi           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelRssi.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelRssi.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top;
          PeripheralScanPanel[DevIdx].LabelRssi.Left      := PeripheralScanPanel[DevIdx].LabelTxPower.Left + PeripheralScanPanel[DevIdx].LabelTxPower.Width + (3*ScanPanelPaddingHorizontal);
          PeripheralScanPanel[DevIdx].LabelRssi.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelRssi.Caption   := 'RSSI: -99 dBm';  // placeholder for proper positioning of further elements

          if BleScanData[DevIdx].IsPaired then begin
            PeripheralScanPanel[DevIdx].LabelPaired           := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelPaired.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].LabelPaired.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top;
            PeripheralScanPanel[DevIdx].LabelPaired.Left      := PeripheralScanPanel[DevIdx].LabelRssi.Left + PeripheralScanPanel[DevIdx].LabelRssi.Width + (3*ScanPanelPaddingHorizontal);
            PeripheralScanPanel[DevIdx].LabelPaired.Font.Size := 10;
            PeripheralScanPanel[DevIdx].LabelPaired.Caption   := 'Paired';  // placeholder for proper positioning of further elements
          end;

          PeripheralScanPanel[DevIdx].LabelMacAddress           := TLabel.Create(ScanForm);
          PeripheralScanPanel[DevIdx].LabelMacAddress.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Top       := PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Top + PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Height + ScanPanelPaddingVertical div 2;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Left      := ScanPanelPaddingHorizontal;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Font.Size := 10;
          PeripheralScanPanel[DevIdx].LabelMacAddress.Caption   := 'MAC Address:';  // placeholder for proper positioning of further elements

          PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelMacAddress.Top + PeripheralScanPanel[DevIdx].LabelMacAddress.Height + 2*ScanPanelPaddingVertical;

          // if we have advertised services we add labels for each
          if BleScanData[DevIdx].ServicesCount > 0 then begin
            SetLength(PeripheralScanPanel[DevIdx].LabelServiceUuid, BleScanData[DevIdx].ServicesCount);
            SetLength(PeripheralScanPanel[DevIdx].TextBoxServiceData, BleScanData[DevIdx].ServicesCount);
            SetLength(PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii, BleScanData[DevIdx].ServicesCount);

            PeripheralScanPanel[DevIdx].BevelSeparatorServices        := TBevel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].BevelSeparatorServices.Parent := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].BevelSeparatorServices.Top    := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].BevelSeparatorServices.Left   := ScanPanelPaddingHorizontal - (ScanPanelPaddingHorizontal div 2);
            PeripheralScanPanel[DevIdx].BevelSeparatorServices.Width  := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].BevelSeparatorServices.Height := 1;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].BevelSeparatorServices.Top + PeripheralScanPanel[DevIdx].BevelSeparatorServices.Height + ScanPanelPaddingVertical;

            PeripheralScanPanel[DevIdx].LabelServices          := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelServices.Parent   := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].LabelServices.Caption  := 'Services:';
            PeripheralScanPanel[DevIdx].LabelServices.Top      := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].LabelServices.Left     := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelServices.Font.Size := 10;

            SetLength(PeripheralScanPanel[DevIdx].LabelServiceUuid, BleScanData[DevIdx].ServicesCount);
            for i := 0 to BleScanData[DevIdx].ServicesCount-1 do begin
              // label for the service uuid or assigned name
              PeripheralScanPanel[DevIdx].LabelServiceUuid[i]           := TLabel.Create(ScanForm);
              PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
              PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Top       := PeripheralScanPanel[DevIdx].LabelServices.Top + i*PeripheralScanPanel[DevIdx].LabelServices.Height;
              PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Left      := PeripheralScanPanel[DevIdx].LabelServices.Left + PeripheralScanPanel[DevIdx].LabelServices.Width + ScanPanelPaddingHorizontal;
              PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Font.Size := 10;
              // check if service is BT assigned number or VSP service
              SetString(s, BleScanData[DevIdx].Services[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1);
              n := BleAssignedServiceUuidToName(BleScanData[DevIdx].Services[i].Uuid);
              if n = '' then begin
                n := BleVspServiceUuidToName(BleScanData[DevIdx].Services[i].Uuid);
                if n = '' then
                  PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Caption  := s
                else begin
                  PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Caption  := n;
                  PeripheralScanPanel[DevIdx].LabelServiceUuid[i].ShowHint := true;
                  PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Hint     := s;
                end;
              end else begin
                PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Caption  := n;
                PeripheralScanPanel[DevIdx].LabelServiceUuid[i].ShowHint := true;
                PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Hint     := s;
              end;
              PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Top + PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Height + 2*ScanPanelPaddingVertical;

              if BleScanData[DevIdx].Services[i].DataLength > 0 then begin
                // checkbox for ascii/hex view
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i]         := TCheckBox.Create(ScanForm);
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Parent  := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Caption := 'ASCII';
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Top     := PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Top;
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Left    := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Width - ScanPanelPaddingHorizontal;
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Tag     := (DevIdx shl TagPosDev) or (i shl TagPosSrv);
                PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].OnClick := @ScanForm.CheckBoxServiceHexAsciiClick;
                // resize of company name to fit the ascii/hex checkbox
                PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Width := PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Width - PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].Width - ScanPanelPaddingHorizontal;
                // textbox for service data
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i]           := TEdit.Create(ScanForm);
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].ReadOnly  := True;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Top       := PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Top + PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Height;// + ScanPanelPaddingVertical;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Left      := PeripheralScanPanel[DevIdx].LabelServiceUuid[i].Left;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Width     := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - PeripheralScanPanel[DevIdx].LabelServices.Width - (3*ScanPanelPaddingHorizontal);
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].AutoSize  := false;
                //PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Height    := 20;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Color     := clForm;
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Font.Size := 10;
                PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Top + PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Height + 2*ScanPanelPaddingVertical;
              end;
            end;
          end;
        end;

        // check if new manufacturing data is available and if we need to add form elements for that
        j := Length(PeripheralScanPanel[DevIdx].LabelManufDataId);
        if BleScanData[DevIdx].ManufacturerDataCount > j then begin

          if Length(PeripheralScanPanel[DevIdx].LabelManufDataId) = 0 then begin  // one very first data set we also add a separator and label
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact        := TBevel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Parent := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Top    := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Left   := ScanPanelPaddingHorizontal - (ScanPanelPaddingHorizontal div 2);
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Width  := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Height := 1;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Top + PeripheralScanPanel[DevIdx].BevelSeparatorManufact.Height + ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].LabelManufData           := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelManufData.Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].LabelManufData.Caption   := 'Manufacturer Specific Data:';
            PeripheralScanPanel[DevIdx].LabelManufData.Top       := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].LabelManufData.Left      := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelManufData.Font.Size := 10;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].LabelManufData.Top + PeripheralScanPanel[DevIdx].LabelManufData.Height + 2*ScanPanelPaddingVertical;
          end;

          SetLength(PeripheralScanPanel[DevIdx].LabelManufDataId, BleScanData[DevIdx].ManufacturerDataCount);
          SetLength(PeripheralScanPanel[DevIdx].TextBoxManufData, BleScanData[DevIdx].ManufacturerDataCount);
          SetLength(PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii, BleScanData[DevIdx].ManufacturerDataCount);

          for i := j to BleScanData[DevIdx].ManufacturerDataCount-1 do begin
            // checkbox for ascii/hex view of manufacturer data
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i]         := TCheckBox.Create(ScanForm);
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Parent  := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Caption := 'ASCII';
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Top     := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - 2*ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Left    := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Width - ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Tag     := (DevIdx shl TagPosDev) or (i shl TagPosSrv);
            PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].OnClick := @ScanForm.CheckBoxManufHexAsciiClick;
            // label for the company name
            PeripheralScanPanel[DevIdx].LabelManufDataId[i]           := TLabel.Create(ScanForm);
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            //PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption   := 'Company name or id';
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Top       := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height - 2*ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Left      := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].AutoSize  := false;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Width     := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].Width - 4*ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Height    := 20;
            PeripheralScanPanel[DevIdx].LabelManufDataId[i].Font.Size := 10;
            // check if company id is assigned company name
            n := BleAssignedCompanyIdToName(LowerCase(IntToHex(BleScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4)));
            if n = '' then
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption  := 'Unknown (' + IntToHex(BleScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4) + ')'
            else begin
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Caption  := n;
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].ShowHint := true;
              PeripheralScanPanel[DevIdx].LabelManufDataId[i].Hint     := n + ' (0x' + IntToHex(BleScanData[DevIdx].ManufacturerData[i].ManufacturerId, 4) + ')';
            end;
            // textbox for the manufacturer data payload
            PeripheralScanPanel[DevIdx].TextBoxManufData[i]           := TEdit.Create(ScanForm);
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Parent    := PeripheralScanPanel[DevIdx].PanelDeviceInfo;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].ReadOnly  := True;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Top       := PeripheralScanPanel[DevIdx].LabelManufDataId[i].top + PeripheralScanPanel[DevIdx].LabelManufDataId[i].Height;// - 3*ScanPanelPaddingVertical;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Left      := ScanPanelPaddingHorizontal;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].AutoSize  := false;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Width     := PeripheralScanPanel[DevIdx].PanelDeviceInfo.Width - (2*ScanPanelPaddingHorizontal);
            //PeripheralScanPanel[DevIdx].TextBoxManufData[i].Height    := 20;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Color     := clForm;
            PeripheralScanPanel[DevIdx].TextBoxManufData[i].Font.Size := 10;
            PeripheralScanPanel[DevIdx].PanelDeviceInfo.Height := PeripheralScanPanel[DevIdx].TextBoxManufData[i].Top + PeripheralScanPanel[DevIdx].TextBoxManufData[i].Height + 2*ScanPanelPaddingVertical;
          end;
        end;

        // update device's form elements
        PeripheralScanPanel[DevIdx].TextBoxDeviceName.Caption    := BleScanData[DevIdx].DeviceName;
        if BleScanData[DevIdx].IsConnectable then begin
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Enabled := true;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Caption :=  'Connectable: Yes'
        end else begin
          PeripheralScanPanel[DevIdx].ButtonConnectDevice.Enabled := false;
          PeripheralScanPanel[DevIdx].LabelDeviceConnectable.Caption :=  'Connectable: No';
        end;
        if BleScanData[DevIdx].TxPower = -32768 then
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: n/a'
        else
          PeripheralScanPanel[DevIdx].LabelTxPower.Caption   := 'TX Power: ' + IntToStr(BleScanData[DevIdx].TxPower) + ' dBm';
        PeripheralScanPanel[DevIdx].LabelRssi.Caption   := 'RSSI: ' + IntToStr(BleScanData[DevIdx].Rssi) + ' dBm';
        PeripheralScanPanel[DevIdx].LabelMacAddress.Caption   := 'MAC Address: ' + UpperCase(BleScanData[DevIdx].MacAddress);

        // add service data if available
        if BleScanData[DevIdx].ServicesCount > 0 then begin
          for i := 0 to BleScanData[DevIdx].ServicesCount-1 do
            if BleScanData[DevIdx].Services[i].DataLength > 0 then begin
              if PeripheralScanPanel[DevIdx].CheckBoxSrvsHexAscii[i].State = cbChecked then
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Caption := UtilDataToAscii(BleScanData[DevIdx].Services[i].Data, BleScanData[DevIdx].Services[i].DataLength)
              else
                PeripheralScanPanel[DevIdx].TextBoxServiceData[i].Caption := UtilDataToHex(BleScanData[DevIdx].Services[i].Data, BleScanData[DevIdx].Services[i].DataLength);
            end;
        end;

        // add manufacturer specific data if available
        if BleScanData[DevIdx].ManufacturerDataCount > 0 then begin
          for i := 0 to BleScanData[DevIdx].ManufacturerDataCount-1 do
            if PeripheralScanPanel[DevIdx].CheckBoxManufHexAscii[i].State = cbChecked then
              PeripheralScanPanel[DevIdx].TextBoxManufData[i].Caption := UtilDataToAscii(BleScanData[DevIdx].ManufacturerData[i].Data, BleScanData[DevIdx].ManufacturerData[i].DataLength)
            else
              PeripheralScanPanel[DevIdx].TextBoxManufData[i].Caption := UtilDataToHex(BleScanData[DevIdx].ManufacturerData[i].Data, BleScanData[DevIdx].ManufacturerData[i].DataLength);
        end;

        // clear flag since we updated everything
        BleScanData[DevIdx].UpdateForm := false;
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
  UtilSetNextFormLeft(Self);
  LogOutput.Clear;

  // welcome log output
  UtilInit(Self.LogOutput);
  UtilLog('Welcome to InsideBlue BLE Tool V0.7.2');
  UtilLog('Copyright (C) 2023 Erik Lins');
  UtilLog('https://github.com/eriklins/InsideBlue-BLE-Tool');
  UtilLog('Uses SimpleBLE version ' + String(SimpleBleGetVersion()));
  UtilLog('');

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
    UtilLog('Could not initialize BLE adapter.');
  end else if not BleIsEnabled then begin
    ButtonStartScan.Enabled := false;
    LabelNofDevices.Caption := 'Bluetooth is disabled.';
    UtilLog('Bluetooth is disabled.');
  end;

  UtilLog('Init done.');
end;


{ Help button clicked }
procedure TScanForm.ButtonHelpClick(Sender: TObject);
begin
  ButtonHelp.Enabled := false;
  HelpShow(ButtonHelp);
end;


{ Start / Stop Scan button clicked }
procedure TScanForm.ButtonStartScanClick(Sender: TObject);
begin
  if not FlagBleScanningActive then begin
    FlagBleScanningActive := true;
    EditFilterDeviceName.Enabled := false;
    EditFilterMacAddress.Enabled := false;
    EditFilterRssi.Enabled := false;
    ButtonClear.Enabled := false;
    ButtonStartScan.Caption := 'Stop Scan';
    LabelNofDevices.Caption := IntToStr(PeripheralNofDevices) + ' Devices Found';
    //ShowMessage('#paired=' + IntToStr(SimpleBleAdapterGetPairedPeripheralsCount(BleAdapter)));
    SimpleBleAdapterScanStart(BleAdapter);
  end else begin
    FlagBleScanningActive := false;
    EditFilterDeviceName.Enabled := true;
    EditFilterMacAddress.Enabled := true;
    EditFilterRssi.Enabled := true;
    ButtonClear.Enabled := true;
    ButtonStartScan.Caption := 'Start Scan';
    LabelNofDevices.Caption := 'Press Start Scan to Discover Devices...';
    //ShowMessage('#paired=' + IntToStr(SimpleBleAdapterGetPairedPeripheralsCount(BleAdapter)));
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
  BleScanFilter.DeviceName := EditFilterDeviceName.Caption;
end;

procedure TScanForm.EditFilterMacAddressEditingDone(Sender: TObject);
begin
  BleScanFilter.MacAddress := LowerCase(EditFilterMacAddress.Caption);
end;

procedure TScanForm.EditFilterRssiEditingDone(Sender: TObject);
begin
  if EditFilterRssi.Caption <> '' then
    BleScanFilter.Rssi := -StrToInt(EditFilterRssi.Caption)
  else
    BleScanFilter.Rssi := 0;
end;


{ Connect to device button clicked }
procedure TScanForm.ButtonConnectDeviceClick(Sender: TObject);
var
  i: Integer;
begin
  tButton(Sender).Enabled := false;
  i := TButton(Sender).Tag;
  ConnectDevice(BleScanData[i].DeviceName, BleScanData[i].MacAddress, BleScanData[i].PeripheralHandle, tButton(Sender));
end;


{ Checkbox service data ascii hex clicked }
procedure TScanForm.CheckBoxServiceHexAsciiClick(Sender: TObject);
var
  DeIdx, SvIdx: Integer;
begin
  DeIdx := (TCheckBox(Sender).Tag shr TagPosDev) and $ff;
  SvIdx := (TCheckBox(Sender).Tag shr TagPosSrv) and $ff;
  if PeripheralScanPanel[DeIdx].CheckBoxSrvsHexAscii[SvIdx].State = cbChecked then
    PeripheralScanPanel[DeIdx].TextBoxServiceData[SvIdx].Caption := UtilDataToAscii(BleScanData[DeIdx].Services[SvIdx].Data, BleScanData[DeIdx].Services[SvIdx].DataLength)
  else
    PeripheralScanPanel[DeIdx].TextBoxServiceData[SvIdx].Caption := UtilDataToHex(BleScanData[DeIdx].Services[SvIdx].Data, BleScanData[DeIdx].Services[SvIdx].DataLength);
end;


{ Checkbox manufacturer data ascii hex clicked }
procedure TScanForm.CheckBoxManufHexAsciiClick(Sender: TObject);
var
  DeIdx, SvIdx: Integer;
begin
  DeIdx := (TCheckBox(Sender).Tag shr TagPosDev) and $ff;
  SvIdx := (TCheckBox(Sender).Tag shr TagPosSrv) and $ff;
  if PeripheralScanPanel[DeIdx].CheckBoxManufHexAscii[SvIdx].State = cbChecked then
    PeripheralScanPanel[DeIdx].TextBoxManufData[SvIdx].Caption := UtilDataToAscii(BleScanData[DeIdx].ManufacturerData[SvIdx].Data, BleScanData[DeIdx].ManufacturerData[SvIdx].DataLength)
  else
    PeripheralScanPanel[DeIdx].TextBoxManufData[SvIdx].Caption := UtilDataToHex(BleScanData[DeIdx].ManufacturerData[SvIdx].Data, BleScanData[DeIdx].ManufacturerData[SvIdx].DataLength);
end;


{ log output checkbox clicked }
procedure TScanForm.CheckBoxShowLogChange(Sender: TObject);
begin
  if CheckBoxShowLog.State = cbChecked then begin
    // increase scan form width to show log output
    ScanForm.Constraints.MaxWidth := 0;
    ScanForm.Constraints.MinWidth := ButtonClearLog.Left + ButtonClearLog.Width + ButtonCopyLog.Width + 2*ScanPanelPaddingHorizontal;
    if ScanFormWidth = 0 then
       ScanForm.Width := ScrollBoxDevices.Left + ScrollBoxDevices.Width + 456 + ScanPanelPaddingHorizontal
    else
       ScanForm.Width := ScanFormWidth;
    ScanForm.ButtonCopyLog.Left := ScanForm.Width - ScanForm.ButtonCopyLog.Width - ScanPanelPaddingHorizontal;
    ScanForm.ButtonCopyLog.Anchors := [akTop, akRight];
    UtilSetNextFormTop(ScanForm);
    UtilSetNextFormLeft(ScanForm);
  end else begin
    // decrease scan form width to hide log output
    ScanForm.ButtonCopyLog.Anchors := [akTop, akLeft];
    ScanForm.Constraints.MaxWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    ScanForm.Constraints.MinWidth := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    ScanFormWidth := ScanForm.Width;  // remember width of form for later
    ScanForm.Width := ScrollBoxDevices.Left + ScrollBoxDevices.Width;
    UtilSetNextFormTop(ScanForm);
    UtilSetNextFormLeft(ScanForm);
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
  UtilSetNextFormTop(ScanForm);
  UtilSetNextFormLeft(ScanForm);
end;

procedure TScanForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SimpleBleAdapterReleaseHandle(BleAdapter);
end;


end.

