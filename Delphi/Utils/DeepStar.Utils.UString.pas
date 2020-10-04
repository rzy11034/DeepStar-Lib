unit DeepStar.Utils.UString;

interface

uses
  System.SysUtils;

type
  UString = type string;
  UChar = Char;

  TUStringHelper = record helper for UString
  private type
    ;
    TArr_chr = TArray<UChar>;
    TArr_str = TArray<UString>;

  private
    function __getChar(index: integer): UChar;
    function __getLength: integer;

  public
    class function Create(const chrArr: TArr_chr): UString; overload; static;
    class function Create(const chrArr: TArr_chr; startIndex, len: integer): UString;
      overload; static;

    function ToCharArray: TArr_chr;
    function ReverseString: UString;
    function Split(const Separators: TArr_chr): TArr_str;
    function Substring(index: integer): UString; overload;
    function Substring(index: integer; len: integer): UString; overload;

    property Chars[index: integer]: UChar read __getChar;
    property Length: integer read __getLength;
  end;

implementation

{ TUStringHelper }

class function TUStringHelper.Create(const chrArr: TArr_chr): UString;
begin
  Result := Create(chrArr, 0, System.Length(chrArr));
end;

class function TUStringHelper.Create(const chrArr: TArr_chr;
  startIndex, len: integer): UString;
begin
  SetLength(Result, len);
  Move(chrArr[startIndex], PChar(PChar(Result))^, len * SizeOf(UChar));
end;

function TUStringHelper.ReverseString: UString;
var
  i, j: integer;
begin
  SetLength(Result, Self.Length);

  i := 1;
  j := Self.Length;
  while (i <= j) do
  begin
    Result[i] := Self[j - i + 1];
    Inc(i);
  end;
end;

function TUStringHelper.Split(const Separators: TArr_chr): TArr_str;
var
  ret: TArr_str;
  tmp: TArr_str;
  i: integer;
begin
  tmp := TArr_str(string(Self).Split(Separators));
  SetLength(ret, System.Length(tmp));

  for i := 0 to high(tmp) do
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
    i := i + 1;
  end;

  Result := chrArr;
end;

function TUStringHelper.__getChar(index: integer): UChar;
begin
{$ZEROBASEDSTRINGS ON}
  Result := Self[index];
{$ZEROBASEDSTRINGS OFF}
end;

function TUStringHelper.__getLength: integer;
begin
  Result := System.Length(Self);
end;

end.
