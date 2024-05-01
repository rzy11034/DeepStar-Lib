unit DeepStar.UString;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF UNIX}
  cwstring,
  {$ENDIF}
  Classes,
  SysUtils,
  LazUTF8;

type
  PString = ^UString;
  UString = unicodestring;
  PChar = ^UChar;
  UChar = unicodechar;

  TUStringHelper = type Helper(TUnicodeStringHelper) for UString
  private type
    TArr_chr = array of UChar;
    TArr_str = array of UString;

  private
    function __getChar(index: integer): UChar;
    function __getLength: integer;
    procedure __setChar(index: integer; const newChars: UChar);

  public
    constructor Create(const chrArr: TArr_chr);
    constructor Create(const chrArr: TArr_chr; startIndex, len: integer);

    procedure Format(const args: array of const);
    function PadLeft(toltaWidth: integer; PaddingChar: UChar): UString;
    function ReverseString: UString;
    function Split(const Separators: TArr_chr): TArr_str;
    function Substring(index: integer): UString;
    function Substring(index: integer; len: integer): UString;
    function ToAnsiString: AnsiString;
    function ToCharArray: TArr_chr;
    function ToInteger: integer;
    function ToPAnsiChar: PAnsiChar; inline;
    function Trim: UString;

    property Chars[index: integer]: UChar read __getChar write __setChar;
    property Length: integer read __getLength;
  end;

  TStringBuilder = TUnicodeStringBuilder;

implementation

uses
  DeepStar.Utils;

  { TUStringHelper }

constructor TUStringHelper.Create(const chrArr: TArr_chr);
begin
  Create(chrArr, 0, System.Length(chrArr));
end;

constructor TUStringHelper.Create(const chrArr: TArr_chr; startIndex, len: integer);
begin
  SetLength(Self, Len);
  Move(chrArr[StartIndex], PChar(PChar(Self))^, Len * SizeOf(UChar));
end;

procedure TUStringHelper.Format(const args: array of const);
begin
  Self := string(SysUtils.Format(Self, Args));
end;

function TUStringHelper.PadLeft(toltaWidth: integer; PaddingChar: UChar): UString;
var
  left: TArr_chr;
  l: integer;
begin
  Result := Self;
  l := toltaWidth - Length;

  if l > 0 then
  begin
    SetLength(left, l);
    TArrayUtils_chr.FillArray(left, PaddingChar);
    Result := UString.Create(left) + Result;
  end;
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

function TUStringHelper.Split(const Separators: TArr_chr): TArr_str;
begin
  Result := inherited Split(Separators);
end;

function TUStringHelper.Substring(index: integer): UString;
begin
  Result := System.Copy(Self, index + 1, Self.Length - index);
end;

function TUStringHelper.Substring(index: integer; len: integer): UString;
begin
  Result := System.Copy(Self, index + 1, len);
end;

function TUStringHelper.ToAnsiString: AnsiString;
begin
  Result := AnsiString(Self);
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

function TUStringHelper.ToInteger: integer;
begin
  Result := inherited ToInteger;
end;

function TUStringHelper.ToPAnsiChar: PAnsiChar;
begin
  Result := PAnsiChar(AnsiString(Self));
end;

function TUStringHelper.Trim: UString;
begin
  Result := SysUtils.Trim(Self);
end;

function TUStringHelper.__getChar(index: integer): UChar;
begin
  Result := Self[index + 1];
end;

function TUStringHelper.__getLength: integer;
begin
  Result := System.Length(Self);
end;

procedure TUStringHelper.__setChar(index: integer; const newChars: UChar);
begin
  Self[index + 1] := newChars;
end;

end.
