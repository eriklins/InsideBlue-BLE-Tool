unit Ble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, SimpleBle, AssignedNumbers;

type

  { Data of device characteristic or descriptor (for local copy) }
  TCharDescData = record
    data: array of Byte;
    len:  Integer;
  end;

  TBleVspTerminal = record
    ServiceFound:       Boolean;
    ServiceName:        string;
    CharacteristicName: array [0..3] of string;
    CharacteristicIdx:  array [0..3] of Integer;
    IsActive:           Boolean;
    DeviceId:           Integer;
  end;

  { Device data we need during an active device connection }
  TBleConnectData = record
    ScnIdx: integer;
    PeripheralHandle:       TSimpleBlePeripheral;
    DeviceName:             String;
    MacAddress:             String;
    ServicesCount:          Integer;
    Services:               array of TSimpleBleService;
    Characteristic:         array of array of TCharDescData;
    Descriptor:             array of array of array of TCharDescData;
    IsConnected:            Boolean;
    IsPaired:               Boolean;
    VspTerminal:            array of TBleVspTerminal;
  end;

  TVspServiceUuids = record
    Uuid: String;
    Name: String;
  end;

  TVspCharacteristicUuids = record
    Uuid: String;
    Name: String;
    Func: Integer;
  end;

  procedure BleInit;
  function BleAssignedServiceUuidToName(uuid: String): String;
  function BleAssignedCharacteristicUuidToName(uuid: String): String;
  function BleAssignedDescriptorUuidToName(uuid: String): String;
  function BleAssignedCompanyIdToName(code: String): String;
  function BleVspServiceUuidToName(uuid: String): String;
  function BleVspCharacteristicUuidToName(uuid: String; var f: Integer): String;

const

  CharDescMaxLength = 512;


  // proprietary ble uart services, indexes point to characteristics in VspCharacteristicUuids (set to -1 if some do not exist)
  VspServiceUuids: array of TVspServiceUuids = (
    (Uuid: '569a1101-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Connectivity VSP Service'),
    (Uuid: '6e400001-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'Nordic UART Service (NUS)')
  );

  VspCharacteristicUuids: array of TVspCharacteristicUuids = (  // proprietary ble uart characteristics
    (Uuid: '569a2001-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp Rx';       Func: 0),
    (Uuid: '569a2000-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp Tx';       Func: 1),
    (Uuid: '569a2003-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemIn';  Func: 2),
    (Uuid: '569a2002-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemOut'; Func: 3),
    (Uuid: '6e400002-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS Rx'; Func: 0),
    (Uuid: '6e400003-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS TX'; Func: 1)
  );

  VspFuncRx       = 0;
  VspFuncTx       = 1;
  VspFuncModemIn  = 2;
  VspFuncModemOut = 3;
  TagPosDev = 24;
  TagPosSrv = 16;
  TagPosChr = 8;
  TagPosDes = 0;

var
  BleConnectData: array of TBleConnectData;
  BleAdapter: TSimplebleAdapter;
  SimpleBleErr: TSimpleBleErr;
  BleAdapterIsInitialized: Boolean;
  BleIsEnabled: Boolean;
  BleVspTerminal: TBleVspTerminal;


implementation

{ Initilize Ble, mainly loading SimpleBLE lib }
procedure BleInit;
begin

    {$IFDEF DYNAMIC_LOADING}
    if not SimpleBleLoadLibrary() then begin
      ShowMessage('Failed to load library simpleble-c.dll');
      exit;
    end;
    {$ENDIF}

    BleAdapterIsInitialized := false;
    BleIsEnabled := false;

    // look for BLE adapters
    if SimpleBleAdapterGetCount() = 0 then begin
      ShowMessage('No BLE adapter was found.');
      Exit;
    end;

    // get a handle for the BLE BleAdapter
    BleAdapter := SimpleBleAdapterGetHandle(0);
    if BleAdapter = 0 then begin
      ShowMessage('Could not get handle for BLE adapter.');
      Exit
    end;

    BleAdapterIsInitialized := true;

    // check if BLE is enabled
    BleIsEnabled := SimpleBleAdapterIsBluetoothEnabled();

end;




{ Convert 128 bit assigned service uuid to name }
function BleAssignedServiceUuidToName(uuid: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(ServiceUuids)-1 do begin  // check official service uuids
    if (ServiceUuids[i].Uuid = uuid) or (ServiceUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      Result := ServiceUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit assigned characteristic uuid to name }
function BleAssignedCharacteristicUuidToName(uuid: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(CharacteristicUuids)-1 do begin
    if (CharacteristicUuids[i].Uuid = uuid) or (CharacteristicUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      Result := CharacteristicUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit assigned descriptor uuid to name }
function BleAssignedDescriptorUuidToName(uuid: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(DescriptorUuids)-1 do begin
    if (DescriptorUuids[i].Uuid = uuid) or (DescriptorUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      Result := DescriptorUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 16 bit assigned company id to name }
function BleAssignedCompanyIdToName(code: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(CompanyIds)-1 do begin
    if CompanyIds[i].code = code then begin
      Result := CompanyIds[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit vsp service uuid to name }
function BleVspServiceUuidToName(uuid: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(VspServiceUuids)-1 do begin  // check proprietary uart service uuids
    if (VspServiceUuids[i].Uuid = uuid) or (VspServiceUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      Result := VspServiceUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit vsp characteristic uuid to name and function id }
function BleVspCharacteristicUuidToName(uuid: String; var f: Integer): String;
var
  i: Integer;
begin
  for i := 0 to Length(VspCharacteristicUuids)-1 do begin  // check proprietary uart service uuids
    if (VspCharacteristicUuids[i].Uuid = uuid) or (VspCharacteristicUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      f := VspCharacteristicUuids[i].Func;
      Result := VspCharacteristicUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;

end.

