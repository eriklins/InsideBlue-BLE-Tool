unit Scan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, StrUtils,
  SimpleBle, Util, Ble;

type

  { type: device data from BLE scanning }
  TBleScanData = record
    PeripheralHandle:       TSimpleBlePeripheral;
    DeviceName:             String;
    MacAddress:             String;
    IsConnectable:          Boolean;
    IsPaired:               Boolean;
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
  procedure ScanInit;

var
  FlagBleScanningActive: Boolean = false;
  PeripheralNofDevices: Integer = 0;
  BleScanData: array of TBleScanData;
  BleScanFilter: TBleScanFilter;


implementation


{ Callback functions for SimpleBLE library }

{ SimpleBLE callback for scan start }
procedure AdapterOnScanStart(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  UtilLog('Scanning started...');
end;


{ SimpleBLE callback for scan stop }
procedure AdapterOnScanStop(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  UtilLog('Scanning stopped.');
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
  if BleScanFilter.DeviceName <> '' then
    if not AnsiContainsText(string(SimpleBlePeripheralIdentifier(Peripheral)), BleScanFilter.DeviceName) then
      Exit;

  // filter for mac address
  if BleScanFilter.MacAddress <> '' then
    if not AnsiContainsText(string(SimpleBlePeripheralAddress(Peripheral)), BleScanFilter.MacAddress) then
      Exit;

  // filter for rssi
  if BleScanFilter.Rssi <> 0 then
    if SimpleBlePeripheralRssi(Peripheral) < BleScanFilter.Rssi then
      Exit;

  // check if device already exist
  DevIdx := 0;
  while DevIdx < PeripheralNofDevices do begin
    if BleScanData[DevIdx].MacAddress = string(SimpleBlePeripheralAddress(Peripheral)) then begin
      break;  // DevIdx is now index of existing device
    end;
    Inc(DevIdx);
  end;

  // if device doesn't exist, we extend the array and increment nof devices
  if DevIdx = PeripheralNofDevices then begin
    SetLength(BleScanData, DevIdx + 1);
    //SetLength(PeripheralScanPanel, DevIdx + 1);
    Inc(PeripheralNofDevices);
  end;

  // Skip device in case of an active BLE connection
  if BleScanData[DevIdx].IsConnected then
    Exit;

  // populate device scan data
  BleScanData[DevIdx].PeripheralHandle := Peripheral;
  BleScanData[DevIdx].DeviceName := string(SimpleBlePeripheralIdentifier(Peripheral));
  BleScanData[DevIdx].MacAddress := string(SimpleBlePeripheralAddress(Peripheral));
  BleScanData[DevIdx].Rssi := SimpleBlePeripheralRssi(Peripheral);
  SimpleBlePeripheralIsConnectable(Peripheral, BleScanData[DevIdx].IsConnectable);
  BleScanData[DevIdx].TxPower := SimpleBlePeripheralTxPower(Peripheral);

  s := '';
  if not BleScanData[DevIdx].IsConnectable then s := 'Not ';
  UtilLog('Dev: [' + UpperCase(BleScanData[DevIdx].MacAddress) + '] "' + BleScanData[DevIdx].DeviceName + '" ' + IntToStr(BleScanData[DevIdx].Rssi) + 'dBm ' + s + 'Connectable');

  // check if we got advertised services
  if SimpleBlePeripheralServicesCount(Peripheral) > 0 then begin
    if BleScanData[DevIdx].ServicesCount = 0 then begin
      BleScanData[DevIdx].ServicesCount := SimpleBlePeripheralServicesCount(Peripheral);
      SetLength(BleScanData[DevIdx].Services, BleScanData[DevIdx].ServicesCount);
    end;
    for j := 0 to BleScanData[DevIdx].ServicesCount-1 do begin
      SimpleBlePeripheralServicesGet(Peripheral, j, BleScanData[DevIdx].Services[j]);
      SetString(s, BleScanData[DevIdx].Services[j].Uuid.Value, SIMPLEBLE_UUID_STR_LEN);
      UtilLog('     SV: ' + s);
    end;
  end;

  // check if we got manufacturer specific data
  if SimpleBlePeripheralManufacturerDataCount(Peripheral) > 0 then begin
    SimpleBlePeripheralManufacturerDataGet(Peripheral, 0, TmpManufacturerData);  // store manuf data temporarily
    j := 0;
    FlagNewData := true;
    while j < BleScanData[DevIdx].ManufacturerDataCount do begin
      if BleScanData[DevIdx].ManufacturerData[j].ManufacturerId = TmpManufacturerData.ManufacturerId then begin
        FlagNewData := false;
        break;
      end;
      Inc(j);
    end;
    if FlagNewData then begin
      SetLength(BleScanData[DevIdx].ManufacturerData, j + 1);
      BleScanData[DevIdx].ManufacturerDataCount := j + 1;
    end;
    SimpleBlePeripheralManufacturerDataGet(Peripheral, 0, BleScanData[DevIdx].ManufacturerData[j]);
    for j := 0 to BleScanData[DevIdx].ManufacturerDataCount-1 do begin
      s := '';
      for k := 0 to BleScanData[DevIdx].ManufacturerData[j].DataLength-1 do
        s := s + IntToHex(BleScanData[DevIdx].ManufacturerData[j].Data[k], 2);
      UtilLog('     MD: ' + IntToHex(BleScanData[DevIdx].ManufacturerData[j].ManufacturerId, 4) + ':' + s);
    end;
  end;

  // set flag to update form elements in timer event
  BleScanData[DevIdx].UpdateForm := true;
end;


{ Clear the list of scanned devices }
procedure ScanClearPeripheralList;
var
  i: Integer;
  connected: Boolean;
begin
  // delete peripheral scan data and release ble peripheral handles
  for i := 0 to PeripheralNofDevices-1 do begin
    SimpleBlePeripheralIsConnected(BleScanData[i].PeripheralHandle, connected);
    if not connected then  // only release handle if device is not connected!
      SimpleBlePeripheralReleaseHandle(BleScanData[i].PeripheralHandle);
  end;
  // reset number of devices and records for scan data and form elements
  PeripheralNofDevices := 0;
  SetLength(BleScanData, 0);
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

