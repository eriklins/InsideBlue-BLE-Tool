unit Util;

{$mode ObjFPC}{$H+}


interface

uses
  Classes, SysUtils, StrUtils, Forms, StdCtrls,
  Ble;

type
  TArrayOfByte = array of Byte;

  procedure UtilInit(memo: TMemo);
  procedure UtilLog(s: string);

  procedure UtilSetNextFormLeft(f: TForm; reset: Boolean = false);
  function UtilGetNextFormLeft(): Integer;
  procedure UtilSetNextFormTop(f: TForm);
  function UtilGetNextFormTop(): Integer;

  function UtilDataToAscii(data: array of Byte; len: Integer): String;
  function UtilDataToHex(data: array of Byte; len: Integer): String;
  function UtilAsciiToData(data: string; var len: Integer): TArrayOfByte;
  function UtilHexToData(data: string; var len: Integer): TArrayOfByte;
  function UtilStringIsHex(s: string): Boolean;


implementation

const
  NextFormMargin = 8;

var
  Log: TMemo;
  NextFormTop : Integer;
  NextFormLeft: Integer;


{ Initialize util functions }
procedure UtilInit(memo: TMemo);
begin
  Log := memo;
end;


{ expose log output function to other units }
procedure UtilLog(s: string);
begin
 Log.Append(s);
end;


{ Functions handling start coordinates of upcoming form windows }
procedure UtilSetNextFormLeft(f: TForm; reset: Boolean = false);
begin
  if not reset then
    NextFormLeft := f.Left + f.Width + NextFormMargin
  else
    NextFormLeft := NextFormLeft - f.Width - NextFormMargin;
end;

function UtilGetNextFormLeft(): Integer;
begin
  Result := NextFormLeft;
end;

procedure UtilSetNextFormTop(f: TForm);
begin
  NextFormTop := f.Top;
end;

function UtilGetNextFormTop(): Integer;
begin
  Result := NextFormTop;
end;


{ Convert data bytes to ASCII string }
function UtilDataToAscii(data: array of Byte; len: Integer): String;
var
  i: Integer;
  c: Byte;
  s: String;
begin
  s := '';
  i := 0;
  while i < len do begin
    c := data[i];
    if (c >= 32) and (c <= 127) then
      s := s + AnsiChar(c)
    else
      s := s + '■';  // ALT-254 should be a solid square block to indicate non-ascii bytes
    Inc(i);
  end;
  Result := s;
end;


{ Convert data bytes to HEX string }
function UtilDataToHex(data: array of Byte; len: Integer): String;
var
  i: Integer;
  s: String;
begin
  s := '';
  i := 0;
  while i < len do begin
    s := s + IntToHex(data[i], 2);
    Inc(i);
  end;
  Result := s;
end;


{ Convert ASCII string to array of bytes}
function UtilAsciiToData(data: string; var len: Integer): TArrayOfByte;
var
  i: Integer;
  s: TArrayOfByte;
begin
  SetLength(s, CharDescMaxLength);
  i := 0;
  while i < Length(data) do begin
    s[i] := Byte(data[i+1]);
    Inc(i);
  end;
  len := i;
  Result := s;
end;


{ Convert HEX string to array of bytes }
function UtilHexToData(data: string; var len: Integer): TArrayOfByte;
var
  i: Integer;
  Buffer: TArrayOfByte;
  t: string;
begin
  SetLength(Buffer, CharDescMaxLength);
  len := Length(data) div 2;
  i := 0;
  while i < len do begin
    t := Copy(data, (i*2)+1, 2);  // get two characters (one hex number) from the string (index starts at one!)
    Buffer[i] := Byte(Hex2Dec(t));
    Inc(i);
  end;
  Result := Buffer;
end;


{ Test if string only contains valid hex numbers and is of even length }
function UtilStringIsHex(s: string): Boolean;
var
  i, len: Integer;
  c: Byte;
begin
  len := Length(s);
  if Odd(len) then begin  // string must of even length
    Result := false;
    Exit;
  end;
  i := 1;
  while i <= len do begin
    c := Byte(s[i]);
    if not ((c >= 48) and (c <= 57) or (c >= 65) and (c <= 70) or (c >= 97) and (c <= 102)) then begin  // 0-9 or A-F or a-f
      Result := false;
      Exit;
    end;
    Inc(i);
  end;
  Result := true;
end;


end.

