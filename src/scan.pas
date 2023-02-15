unit Scan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils,
  SimpleBle, Ble;

type

  { type: device data from BLE scanning }
  TBleScanData = record
    PeripheralHandle:       TSimpleBlePeripheral;
    DeviceName:             String;
    MacAddress:             String;
    IsConnectable:          Boolean;
    TxPower:                Integer;
    Rssi:                   Integer;
    ServicesCount:          Integer;
    Services:               array of TSimpleBleService;
    ManufacturerDataCount:  Integer;
    ManufacturerData:       array of TSimpleBleManufacturerData;
    UpdateForm:             Boolean;
    IsConnected:            Boolean;
  end;

  { type: filters for ble scanning }
  TBleScanFilter = record
    DeviceName: String;
    MacAddress: String;
    Rssi:       Integer;
  end;

  procedure ScanClearPeripheralList;
  procedure ScanRestoreConnectButton(peripheral: TSimpleBlePeripheral);
  procedure ScanInit;

var
  FlagBleScanningActive: Boolean = false;
  PeripheralNofDevices: Integer = 0;
  PeripheralScanData: array of TBleScanData;
  PeripheralScanFilter: TBleScanFilter;


implementation

uses Main, Connect;  // not best practice, but need access to ScanForm for log output...


{ Callback functions for SimpleBLE library }

{ SimpleBLE callback for scan start }
procedure AdapterOnScanStart(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  ScanForm.LogOutput.Append('Scanning started...');
end;


{ SimpleBLE callback for scan stop }
procedure AdapterOnScanStop(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  ScanForm.LogOutput.Append('Scanning stopped.');
end;


{ SimpleBLE callback for scan found or updated }
procedure AdapterOnScanFoundUpdated(Adapter: TSimplebleAdapter; Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
var
  AdapterIdentifier: PChar;
  PeripheralAddress: PChar;
  DevIdx, j, k: Integer;
  FlagNewData: Boolean;
  s: String;
  TmpManufacturerData: TSimpleBleManufacturerData;
begin

  AdapterIdentifier := SimpleBleAdapterIdentifier(Adapter);
  PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);

  if (AdapterIdentifier = '') or (PeripheralAddress = '') then
    Exit;

  // filter for device name
  if PeripheralScanFilter.DeviceName <> '' then
    if not AnsiContainsText(string(SimpleBlePeripheralIdentifier(Peripheral)), PeripheralScanFilter.DeviceName) then
      Exit;

  // filter for mac address
  if PeripheralScanFilter.MacAddress <> '' then
    if not AnsiContainsText(string(SimpleBlePeripheralAddress(Peripheral)), PeripheralScanFilter.MacAddress) then
      Exit;

  // filter for rssi
  if PeripheralScanFilter.Rssi <> 0 then
    if SimpleBlePeripheralRssi(Peripheral) < PeripheralScanFilter.Rssi then
      Exit;

  // check if device already exist
  DevIdx := 0;
  while DevIdx < PeripheralNofDevices do begin
    if PeripheralScanData[DevIdx].MacAddress = string(SimpleBlePeripheralAddress(Peripheral)) then begin
      break;  // DevIdx is now index of existing device
    end;
    Inc(DevIdx);
  end;

  // if device doesn't exist, we extend the array and increment nof devices
  if DevIdx = PeripheralNofDevices then begin
    SetLength(PeripheralScanData, DevIdx + 1);
    //SetLength(PeripheralScanPanel, DevIdx + 1);
    Inc(PeripheralNofDevices);
  end;

  // Skip device in case of an active BLE connection
  if PeripheralScanData[DevIdx].IsConnected then
    Exit;

  // populate device scan data
  PeripheralScanData[DevIdx].PeripheralHandle := Peripheral;
  PeripheralScanData[DevIdx].DeviceName := string(SimpleBlePeripheralIdentifier(Peripheral));
  PeripheralScanData[DevIdx].MacAddress := string(SimpleBlePeripheralAddress(Peripheral));
  PeripheralScanData[DevIdx].Rssi := SimpleBlePeripheralRssi(Peripheral);
  SimpleBlePeripheralIsConnectable(Peripheral, PeripheralScanData[DevIdx].IsConnectable);
  PeripheralScanData[DevIdx].TxPower := SimpleBlePeripheralTxPower(Peripheral);

  s := '';
  if not PeripheralScanData[DevIdx].IsConnectable then s := 'Not ';
  ScanForm.LogOutput.Append('Dev: [' + UpperCase(PeripheralScanData[DevIdx].MacAddress) + '] "' + PeripheralScanData[DevIdx].DeviceName + '" ' + IntToStr(PeripheralScanData[DevIdx].Rssi) + 'dBm ' + s + 'Connectable');

  // check if we got advertised services
  if SimpleBlePeripheralServicesCount(Peripheral) > 0 then begin
    if PeripheralScanData[DevIdx].ServicesCount = 0 then begin
      PeripheralScanData[DevIdx].ServicesCount := SimpleBlePeripheralServicesCount(Peripheral);
      SetLength(PeripheralScanData[DevIdx].Services, PeripheralScanData[DevIdx].ServicesCount);
    end;
    for j := 0 to PeripheralScanData[DevIdx].ServicesCount-1 do begin
      SimpleBlePeripheralServicesGet(Peripheral, j, PeripheralScanData[DevIdx].Services[j]);
      SetString(s, PeripheralScanData[DevIdx].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN);
      ScanForm.LogOutput.Append('     SV: ' + s);
    end;
  end;

  // check if we got manufacturer specific data
  if SimpleBlePeripheralManufacturerDataCount(Peripheral) > 0 then begin
    SimpleBlePeripheralManufacturerDataGet(Peripheral, 0, TmpManufacturerData);  // store manuf data temporarily
    j := 0;
    FlagNewData := true;
    while j < PeripheralScanData[DevIdx].ManufacturerDataCount do begin
      if PeripheralScanData[DevIdx].ManufacturerData[j].ManufacturerId = TmpManufacturerData.ManufacturerId then begin
        FlagNewData := false;
        break;
      end;
      Inc(j);
    end;
    if FlagNewData then begin
      SetLength(PeripheralScanData[DevIdx].ManufacturerData, j + 1);
      PeripheralScanData[DevIdx].ManufacturerDataCount := j + 1;
    end;
    SimpleBlePeripheralManufacturerDataGet(Peripheral, 0, PeripheralScanData[DevIdx].ManufacturerData[j]);
    for j := 0 to PeripheralScanData[DevIdx].ManufacturerDataCount-1 do begin
      s := '';
      for k := 0 to PeripheralScanData[DevIdx].ManufacturerData[j].DataLength-1 do
        s := s + IntToHex(PeripheralScanData[DevIdx].ManufacturerData[j].Data[k], 2);
      ScanForm.LogOutput.Append('     MD: ' + IntToHex(PeripheralScanData[DevIdx].ManufacturerData[j].ManufacturerId, 4) + ':' + s);
    end;
  end;

  // set flag to update form elements in timer event
  PeripheralScanData[DevIdx].UpdateForm := true;
end;


{ Clear the list of scanned devices }
procedure ScanClearPeripheralList;
var
  i: Integer;
  c: Boolean;
begin
  // delete peripheral scan data and release ble peripheral handles
  for i := 0 to PeripheralNofDevices-1 do begin
    if not PeripheralScanData[i].IsConnected then  // only release handle if device is not connected!
      SimpleBlePeripheralReleaseHandle(PeripheralScanData[i].PeripheralHandle);
  end;
  // reset number of devices and records for scan data and form elements
  PeripheralNofDevices := 0;
  SetLength(PeripheralScanData, 0);
end;


{ When disconnecting from a device we may restore the Connect button }
procedure ScanRestoreConnectButton(peripheral: TSimpleBlePeripheral);
var
  i: Integer;
begin
  for i := 0 to PeripheralNofDevices-1 do begin
    if PeripheralScanData[i].PeripheralHandle = peripheral then begin
      PeripheralScanPanel[i].ButtonConnectDevice.Enabled := true;
      break;
    end;
  end;
end;


procedure ScanInit;
begin

  // register SimpleBLE scan callback functions
  SimpleBleAdapterSetCallbackOnScanStart(BleAdapter, @AdapterOnScanStart, Nil);
  SimpleBleAdapterSetCallbackOnScanStop(BleAdapter, @AdapterOnScanStop, Nil);
  SimpleBleAdapterSetCallbackOnScanFound(BleAdapter, @AdapterOnScanFoundUpdated, Nil);
  SimpleBleAdapterSetCallbackOnScanUpdated(BleAdapter, @AdapterOnScanFoundUpdated, Nil);

end;

end.

