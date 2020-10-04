unit DeepStar.Utils.UString;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils;

type
  UString = UnicodeString;
  UChar = UnicodeChar;

  TUStringHelper = type Helper for UString
  private type
    TArr_chr = array of UChar;
    TArr_str = array of UString;

  private
    function __getChar(index: integer): UChar;
    function __getLength: integer;

  public
    class function Create(const chrArr: TArr_chr): UString; static;
    class function Create(const chrArr: TArr_chr; startIndex, len: integer): UString; static;

    function ToCharArray: TArr_chr;
    function Split(const Separators: TCharArray): TArr_str;
    function ReverseString: UString;
    function Substring(index: integer): UString;
    function Substring(index: integer; len: integer): UString;

    property Chars[index: integer]: UChar read __getChar;
    property Length: integer read __getLength;
  end;

  TStringBuilder = TUnicodeStringBuilder;

implementation

{ TUStringHelper }

class function TUStringHelper.Create(const chrArr: TArr_chr): UString;
begin
  Result := Create(chrArr, 0, System.Length(chrArr));
end;

class function TUStringHelper.Create(const chrArr: TArr_chr;
  startIndex, len: integer): UString;
begin
  SetLength(Result, Len);
  Move(chrArr[StartIndex], PChar(PChar(Result))^, Len * SizeOf(UChar));
end;

function TUStringHelper.ReverseString: UString;
var
  i, j: integer;
begin
  setlength(Result, Self.Length);

  i := 1;
  j := Self.Length;
  while (i <= j) do
  begin
    Result[i] := Self[j - i + 1];
    Inc(i);
  end;
end;

function TUStringHelper.Split(const Separators: TCharArray): TArr_str;
var
  ret: TArr_str;
  tmp: TStringArray;
  i: integer;
begin
  tmp := string(Self).Split(Separators);
  SetLength(ret, System.Length(tmp));

  for i := 0 to High(tmp) do
  begin
    ret[i] := UString(tmp[i]);
  end;

  Result := ret;
end;

function TUStringHelper.Substring(index: integer): UString;
begin
  Result := System.Copy(Self, index + 1, Self.Length - index);
end;

function TUStringHelper.Substring(index: integer; len: integer): UString;
begin
  Result := System.Copy(Self, index + 1, len);
end;

function TUStringHelper.ToCharArray: TArr_chr;
var
  chrArr: TArr_chr;
  c: UChar;
  i: integer;
begin
  SetLength(chrArr, Self.Length);

  i := 0;
  for c in Self do
  begin
    chrArr[i] := c;
    i += 1;
  end;

  Result := chrArr;
end;

function TUStringHelper.__getChar(index: integer): UChar;
begin
  Result := Self[index + 1];
end;

function TUStringHelper.__getLength: integer;
begin
  Result := System.Length(Self);
end;

end.
