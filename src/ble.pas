unit Ble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, SimpleBle, AssignedNumbers;

type

  TVspServiceUuids = record
    Uuid:       String;
    Name:       String;
    ChRx:       Integer;
    ChTx:       Integer;
    ChModemIn:  Integer;
    ChModemOut: Integer;
  end;

  TVspCharacteristicUuids = record
    Uuid: String;
    Name: String;
  end;

  procedure BleInit;
  function  BleAssignedServiceUuidToName(uuid: String): String;
  function  BleAssignedCharacteristicUuidToName(uuid: String): String;
  function  BleAssignedDescriptorUuidToName(uuid: String): String;
  function  BleAssignedCompanyIdToName(code: String): String;
  function  BleVspServiceUuidToName(uuid: String): String;
  function  BleVspCharacteristicUuidToName(uuid: String): String;

const

  CharDescMaxLength = 512;


  // proprietary ble uart services, indexes point to characteristics in VspCharacteristicUuids (set to -1 if some do not exist)
  VspServiceUuids: array of TVspServiceUuids = (
    (Uuid: '569a1101-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Connectivity VSP Service'; ChRx: 0; ChTx: 1; ChModemIn: 2;  ChModemOut: 3),
    (Uuid: '6e400001-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'Nordic UART Service (NUS)';      ChRx: 4; ChTx: 6; ChModemIn: -1; ChModemOut: -1)
  );

  VspCharacteristicUuids: array of TVspCharacteristicUuids = (  // proprietary ble uart characteristics
    (Uuid: '569a2001-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp Rx'),
    (Uuid: '569a2000-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp Tx'),
    (Uuid: '569a2003-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemIn'),
    (Uuid: '569a2002-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemOut'),
    (Uuid: '6e400002-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS Rx'),
    (Uuid: '6e400003-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS TX')
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
  BleAdapter: TSimplebleAdapter;
  SimpleBleErr: TSimpleBleErr;
  BleAdapterIsInitialized: Boolean;
  BleIsEnabled: Boolean;


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
function BleVspCharacteristicUuidToName(uuid: String): String;
var
  i: Integer;
begin
  for i := 0 to Length(VspCharacteristicUuids)-1 do begin  // check proprietary uart service uuids
    if (VspCharacteristicUuids[i].Uuid = uuid) or (VspCharacteristicUuids[i].Uuid = Copy(uuid, 5, 4)) then begin
      Result := VspCharacteristicUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;

end.

