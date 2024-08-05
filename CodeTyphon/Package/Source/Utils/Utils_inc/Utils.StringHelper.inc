  {%MainUint DeepStar.Utils.pas}

  {$ifdef InterfacePart}

type
  UChar = unicodechar;
  PChar = ^UChar;
  PString = ^UString;
  UString = unicodestring;

  TUnicodeStringHelper = type Helper(SysUtils.TUnicodeStringHelper) for UString
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

  TStringHelper = type Helper(SysUtils.TStringHelper) for AnsiString
  public
    function ToUString: UnicodeString;
  end;

  TStringBuilder = TUnicodeStringBuilder;

  {$Else}

constructor TUnicodeStringHelper.Create(const chrArr: TArr_chr);
begin
  Create(chrArr, 0, System.Length(chrArr));
end;

constructor TUnicodeStringHelper.Create(const chrArr: TArr_chr; startIndex, len: integer);
var
  i: Integer;
begin
  if len > 0 then
  begin
    if len + startIndex > System.Length(chrArr) then
      len := System.Length(chrArr) - startIndex;

    Self := '';
    SetLength(Self, len);

    for i := 0 to len - 1 do
    begin
      Self.Chars[i] := chrArr[i + startIndex];
    end;
  end;
end;

procedure TUnicodeStringHelper.Format(const args: array of const);
begin
  Self := inherited Format(args);
end;

function TUnicodeStringHelper.PadLeft(toltaWidth: integer; PaddingChar: UChar): UString;
var
  left: TArr_chr;
  l, i: integer;
begin
  Result := Self;
  l := toltaWidth - Length;

  if l > 0 then
  begin
    SetLength(left, l);
    for i := 0 to High(left) do
      left[i] := PaddingChar;

    //TArrayUtils_chr.FillArray(left, PaddingChar);
    Result := UString.Create(left) + Result;
  end;
end;

function TUnicodeStringHelper.ReverseString: UString;
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

function TUnicodeStringHelper.Split(const Separators: TArr_chr): TArr_str;
begin
  Result := inherited Split(Separators);
end;

function TUnicodeStringHelper.Substring(index: integer): UString;
begin
  Result := System.Copy(Self, index + 1, Self.Length - index);
end;

function TUnicodeStringHelper.Substring(index: integer; len: integer): UString;
begin
  Result := System.Copy(Self, index + 1, len);
end;

function TUnicodeStringHelper.ToAnsiString: AnsiString;
begin
  Result := AnsiString(Self);
end;

function TUnicodeStringHelper.ToCharArray: TArr_chr;
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

function TUnicodeStringHelper.ToInteger: integer;
begin
  Result := inherited ToInteger;
end;

function TUnicodeStringHelper.ToPAnsiChar: PAnsiChar;
begin
  Result := PAnsiChar(AnsiString(Self));
end;

function TUnicodeStringHelper.Trim: UString;
begin
  Result := SysUtils.Trim(Self);
end;

function TUnicodeStringHelper.__getChar(index: integer): UChar;
begin
  Result := Self[index + 1];
end;

function TUnicodeStringHelper.__getLength: integer;
begin
  Result := System.Length(Self);
end;

procedure TUnicodeStringHelper.__setChar(index: integer; const newChars: UChar);
begin
  Self[index + 1] := newChars;
end;

{ TStringHelper }

function TStringHelper.ToUString: UnicodeString;
begin
  Result := UnicodeString(Self);
end;

{$EndIf}
