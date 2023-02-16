unit Ble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, SimpleBle, AssignedNumbers;

type
  TVspServiceUuids = record
    Uuid:    String;
    Name:    String;
    RxIdx:   Integer;  // data TO the device
    TxIdx:   Integer;  // data FROM the device
    MInIdx:  Integer;  // control TO the device
    MoutIdx: Integer;  // control FROM the device
  end;

  TVspCharacteristicUuids = record
    Uuid: String;
    Name: String;
  end;

  TVspTerminal = record
    ServiceFound: Boolean;
    RxIdx:        Integer;  // data TO the device
    TxIdx:        Integer;  // data FROM the device
    MInIdx:       Integer;  // control TO the device
    MoutIdx:      Integer;  // control FROM the device
  end;

  procedure BleInit;
  function BleAssignedServiceUuidToName(uuid: String): String;
  function BleAssignedCharacteristicUuidToName(uuid: String): String;
  function BleAssignedDescriptorUuidToName(uuid: String): String;
  function BleAssignedCompanyIdToName(code: String): String;
  function BleVspServiceUuidToName(uuid: String): String;
  function BleVspCharacteristicUuidToName(uuid: String): String;

const
  VspServiceUuids: array of TVspServiceUuids = (  // proprietary ble uart services
    (Uuid: '569a1101-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Connectivity VSP Service'; RxIdx: 0; TxIdx: 1; MInIdx: 2; MOutIdx: 3),
    (Uuid: '6e400001-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'Nordic UART Service (NUS)';      RxIdx: 4; TxIdx: 5; MInIdx: -1; MOutIdx: -1)
  );

  VspCharacteristicUuids: array of TVspCharacteristicUuids = (  // proprietary ble uart services
    (Uuid: '569a2001-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp RX'),
    (Uuid: '569a2000-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp Tx'),
    (Uuid: '569a2003-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemIn'),
    (Uuid: '569a2002-b87f-490c-92cb-11ba5ea5167c'; Name: 'Laird Vsp ModemOut'),
    (Uuid: '6e400002-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS Rx'),
    (Uuid: '6e400003-b5a3-f393-e0a9-e50e24dcca9e'; Name: 'NUS TX')
  );

var
  BleAdapter: TSimplebleAdapter;
  SimpleBleErr: TSimpleBleErr;
  BleAdapterIsInitialized: Boolean;
  BleIsEnabled: Boolean;
  BleVspTerminal: TVspTerminal;


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
      BleVspTerminal.RxIdx        := VspServiceUuids[i].RxIdx;
      BleVspTerminal.TxIdx        := VspServiceUuids[i].TxIdx;
      BleVspTerminal.MInIdx       := VspServiceUuids[i].MInIdx;
      BleVspTerminal.MoutIdx      := VspServiceUuids[i].MoutIdx;
      BleVspTerminal.ServiceFound := true;
      Result := VspServiceUuids[i].Name;
      Exit;
    end;
  end;
  Result := '';
end;


{ Convert 128 bit vsp characteristics uuid to name }
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

