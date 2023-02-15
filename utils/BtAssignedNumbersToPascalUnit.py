# BtAssignedNumbersToPascal.py
#
# This Python script converts the files
# - service_uuids.json
# - characteristic_uuids.json
# - descriptor_uuids.json
# - company_ids.json
# into a Pascal unit with constant array definitions for that information.
#
# The original files are available from https://github.com/NordicSemiconductor/bluetooth-numbers-database/tree/master/v1
#
# Erik Lins, 2023, (https://github.com/eriklins)

import sys
import re
import json
from urllib.request import urlopen

# open again for writing and parse line by line
outfile = open("assignednumbers.pas", "w", encoding="utf-8")

# print header and start of Pascal unit
print("""unit AssignedNumbers;

{$mode ObjFPC}{$H+}

{ Bluetooth SiG Assigned Numbers for
  - Service UUIDs
  - Characteristic UUIDs
  - Descriptor UUIDs
  - Company IDs

  Converted from https://github.com/NordicSemiconductor/bluetooth-numbers-database/tree/master/v1
  into a Pascal unit with BtAssignedNumbersToPascal.py Python script.

  Erik Lins, 2023, (https://github.com/eriklins)
  }

interface

uses
  Classes, SysUtils;

type
  TServiceUuids = record
    Uuid: String;
    Name: String;
  end;

  TCharacteristicUuids = record
    Uuid: String;
    Name: String;
  end;

  TDescriptorUuids = record
    Uuid: String;
    Name: String;
  end;

  TCompanyIds = record
    Code: String;
    Name: String;
  end;

const
""", file=outfile)

# service_uuids.json
url = urlopen("https://raw.githubusercontent.com/NordicSemiconductor/bluetooth-numbers-database/master/v1/service_uuids.json")
data = json.loads(url.read())
print("  ServiceUuids: array of TServiceUuids = (", file=outfile)
for i in data:
    print("    (Uuid: '"+i['uuid'].lower()+"'; Name: '"+i['name']+"'),", file=outfile)
print("    (Uuid: ''; Name: '')", file=outfile)
print("  );", file=outfile)
print("", file=outfile)

# characteristic_uuids.json
url = urlopen("https://raw.githubusercontent.com/NordicSemiconductor/bluetooth-numbers-database/master/v1/characteristic_uuids.json")
data = json.loads(url.read())
print("  CharacteristicUuids: array of TCharacteristicUuids = (", file=outfile)
for i in data:
    print("    (Uuid: '"+i['uuid'].lower()+"'; Name: '"+i['name']+"'),", file=outfile)
print("    (Uuid: ''; Name: '')", file=outfile)
print("  );", file=outfile)
print("", file=outfile)

# descriptor_uuids.json
url = urlopen("https://raw.githubusercontent.com/NordicSemiconductor/bluetooth-numbers-database/master/v1/descriptor_uuids.json")
data = json.loads(url.read())
print("  DescriptorUuids: array of TDescriptorUuids = (", file=outfile)
for i in data:
    print("    (Uuid: '"+i['uuid'].lower()+"'; Name: '"+i['name']+"'),", file=outfile)
print("    (Uuid: ''; Name: '')", file=outfile)
print("  );", file=outfile)
print("", file=outfile)

# company_ids.json
url = urlopen("https://raw.githubusercontent.com/NordicSemiconductor/bluetooth-numbers-database/master/v1/company_ids.json")
data = json.loads(url.read())
print("  CompanyIds: array of TCompanyIds = (", file=outfile)
for i in data:
    print("    (Code: '"+format(i['code'],'04x')+"'; Name: '"+re.sub(r"(?i)\'"," ",i['name'])+"'),", file=outfile)
print("    (Code: ''; Name: '')", file=outfile)
print("  );", file=outfile)
print("", file=outfile)

# print footer
print("""
implementation

end.

""", file=outfile)


# close output file
outfile.close()

