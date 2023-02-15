unit Ble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, SimpleBle, AssignedNumbers;

  procedure BleInit;
  function BleAssignedServiceUuidToName(uuid: String): String;
  function BleAssignedCharacteristicUuidToName(uuid: String): String;
  function BleAssignedDescriptorUuidToName(uuid: String): String;
  function BleAssignedCompanyIdToName(code: String): String;

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
  for i := 0 to Length(ServiceUuids)-1 do begin
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

end.

