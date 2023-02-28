unit Ble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  SimpleBle, AssignedNumbers;

type
  TVspServiceUuids = record
    Uuid:       TSimpleBleUuid;
    Name:       String;
    ChRx:       Integer;
    ChTx:       Integer;
    ChModemIn:  Integer;
    ChModemOut: Integer;
  end;

  TVspCharacteristicUuids = record
    Uuid: TSimpleBleUuid;
    Name: String;
  end;

  procedure BleInit;
  function  BleAssignedServiceUuidToName(uuid: TSimpleBleUuid): String;
  function  BleAssignedCharacteristicUuidToName(uuid: TSimpleBleUuid): String;
  function  BleAssignedDescriptorUuidToName(uuid: TSimpleBleUuid): String;
  function  BleAssignedCompanyIdToName(code: String): String;
  function  BleVspServiceUuidToName(uuid: TSimpleBleUuid): String;
  function  BleVspCharacteristicUuidToName(uuid: TSimpleBleUuid): String;

const
  CharDescMaxLength = 512;


  // proprietary ble uart services, indexes point to characteristics in VspCharacteristicUuids (set to -1 if some do not exist)
  VspServiceUuids: array of TVspServiceUuids = (
    (Uuid: (Value: '569a1101-b87f-490c-92cb-11ba5ea5167c'); Name: 'Laird Connectivity VSP Service';          ChRx: 0; ChTx: 1; ChModemIn: 2;  ChModemOut: 3),
    (Uuid: (Value: '6e400001-b5a3-f393-e0a9-e50e24dcca9e'); Name: 'Nordic UART Service (NUS)';               ChRx: 4; ChTx: 5; ChModemIn: -1; ChModemOut: -1),
    (UUid: (Value: '2456e1b9-26e2-8f83-e744-f34f01e9d701'); Name: 'u-connectXpress BLE Serial Port Service'; ChRx: 6; ChTx: 7; ChModemIn: -1; ChModemOut: -1)

  );

  VspCharacteristicUuids: array of TVspCharacteristicUuids = (  // proprietary ble uart characteristics
    (Uuid: (Value: '569a2001-b87f-490c-92cb-11ba5ea5167c'); Name: 'Laird Vsp Rx'),        // 0
    (Uuid: (Value: '569a2000-b87f-490c-92cb-11ba5ea5167c'); Name: 'Laird Vsp Tx'),        // 1
    (Uuid: (Value: '569a2003-b87f-490c-92cb-11ba5ea5167c'); Name: 'Laird Vsp ModemIn'),   // 2
    (Uuid: (Value: '569a2002-b87f-490c-92cb-11ba5ea5167c'); Name: 'Laird Vsp ModemOut'),  // 3

    (Uuid: (Value: '6e400002-b5a3-f393-e0a9-e50e24dcca9e'); Name: 'NUS Rx'),  // 4
    (Uuid: (Value: '6e400003-b5a3-f393-e0a9-e50e24dcca9e'); Name: 'NUS TX'),  // 5

    (Uuid: (Value: '2456e1b9-26e2-8f83-e744-f34f01e9d703'); Name: 'Serial Port FIFO / Rx'),   // 6
    (Uuid: (Value: '2456e1b9-26e2-8f83-e744-f34f01e9d704'); Name: 'Serial Port Credit / Tx')  // 7
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
function BleAssignedServiceUuidToName(uuid: TSimpleBleUuid): String;
var
  i: Integer;
begin
  for i := 0 to Length(ServiceUuids)-1 do begin  // check official service uuids
    if (CompareChar(uuid.Value, ServiceUuids[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0) or (CompareChar(uuid.Value[4], ServiceUuids[i].Uuid.Value, 4) = 0) then begin
      Result := ServiceUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit assigned characteristic uuid to name }
function BleAssignedCharacteristicUuidToName(uuid: TSimpleBleUuid): String;
var
  i: Integer;
begin
  for i := 0 to Length(CharacteristicUuids)-1 do begin
    if (CompareChar(uuid.Value, CharacteristicUuids[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0) or (CompareChar(uuid.Value[4], CharacteristicUuids[i].Uuid.Value, 4) = 0) then begin
      Result := CharacteristicUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit assigned descriptor uuid to name }
function BleAssignedDescriptorUuidToName(uuid: TSimpleBleUuid): String;
var
  i: Integer;
begin
  for i := 0 to Length(DescriptorUuids)-1 do begin
    if (CompareChar(uuid.Value, DescriptorUuids[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0) or (CompareChar(uuid.Value[4], DescriptorUuids[i].Uuid.Value, 4) = 0) then begin
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
function BleVspServiceUuidToName(uuid: TSimpleBleUuid): String;
var
  i: Integer;
begin
  for i := 0 to Length(VspServiceUuids)-1 do begin  // check proprietary uart service uuids
    if (CompareChar(uuid.Value, VspServiceUuids[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0) or (CompareChar(uuid.Value[4], VspServiceUuids[i].Uuid.Value, 4) = 0) then begin
      Result := VspServiceUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit vsp characteristic uuid to name and function id }
function BleVspCharacteristicUuidToName(uuid: TSimpleBleUuid): String;
var
  i: Integer;
begin
  for i := 0 to Length(VspCharacteristicUuids)-1 do begin  // check proprietary uart service uuids
    if (CompareChar(uuid.Value, VspCharacteristicUuids[i].Uuid.Value, SIMPLEBLE_UUID_STR_LEN-1) = 0) or (CompareChar(uuid.Value[4], VspCharacteristicUuids[i].Uuid.Value, 4) = 0) then begin
      Result := VspCharacteristicUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;

end.

