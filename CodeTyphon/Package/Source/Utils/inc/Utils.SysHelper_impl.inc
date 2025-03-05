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

function TStringHelper.ToUString: String;
begin
  Result := String(Self);
end;

{ TIntegerHelper }

function TIntegerHelper.ToBinString: string;
begin
  Result := String(inherited ToBinString);
end;

function TIntegerHelper.ToHexString(const AMinDigits: Integer): string;
begin
  Result := String(inherited ToHexString(AMinDigits));
end;

function TIntegerHelper.ToHexString: string;
begin
  Result := String(inherited ToHexString);
end;

function TIntegerHelper.ToString: String;
begin
  Result := String(inherited ToString);
end;

{ TCardinalHelper }

function TCardinalHelper.ToBinString: string;
begin
  Result := String(inherited ToBinString);
end;

function TCardinalHelper.ToHexString(const AMinDigits: Integer): string;
begin
  Result := String(inherited ToHexString(AMinDigits));
end;

function TCardinalHelper.ToHexString: string;
begin
  Result := String(inherited ToHexString);
end;

function TCardinalHelper.ToString: String;
begin
  Result := String(inherited ToString);
end;

{ TLongIntHelper }

function TLongIntHelper.ToBinString: string;
begin
  Result := String(inherited ToBinString);
end;

function TLongIntHelper.ToHexString(const AMinDigits: Integer): string;
begin
  Result := String(inherited ToHexString(AMinDigits));
end;

function TLongIntHelper.ToHexString: string;
begin
  Result := String(inherited ToHexString);
end;

function TLongIntHelper.ToString: String;
begin
  Result := String(inherited ToString);
end;

{ TInt64Helper }

function TInt64Helper.ToString: String;
begin
  Result := String(inherited ToString);
end;

{ TSingleHelper }

function TSingleHelper.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string;
begin
  Result := String(inherited ToString(AFormat, APrecision, ADigits));
end;

function TSingleHelper.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer;
  const AFormatSettings: TFormatSettings): string;
begin
  Result := String(inherited ToString(AFormat, APrecision, ADigits, AFormatSettings));
end;

function TSingleHelper.ToString(const AFormatSettings: TFormatSettings): string;
begin
  Result := String(inherited ToString(AFormatSettings));
end;

function TSingleHelper.ToString: string;
begin
  Result := String(inherited ToString);
end;

{ TDoubleHelper }

function TDoubleHelper.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string;
begin
  Result := String(inherited ToString(AFormat, APrecision, ADigits));
end;

function TDoubleHelper.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer;
  const AFormatSettings: TFormatSettings): string;
begin
  Result := String(inherited ToString(AFormat, APrecision, ADigits, AFormatSettings));
end;

function TDoubleHelper.ToString(const AFormatSettings: TFormatSettings): string;
begin
  Result := String(inherited ToString(AFormatSettings));
end;

function TDoubleHelper.ToString: string;
begin
  Result := String(inherited ToString);
end;

{ TBooleanHelper }

function TBooleanHelper.ToString(UseBoolStrs: TUseBoolStrs): string;
begin
  Result := String(inherited ToString(UseBoolStrs));
end;

