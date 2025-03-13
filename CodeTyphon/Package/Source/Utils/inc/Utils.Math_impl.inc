
class function TMath.AnyToDec(numStr: string; NumberSystem: integer): integer;
var
  stack: IStack_chr;
  i, res: integer;
begin
  stack := TStack_chr.Create;
  res := 0;

  for i := Low(numStr) to High(numStr) do
    stack.Push(numStr[i]);

  i := 0;
  while not stack.IsEmpty do
  begin
    res += CharToNum(stack.Pop) * Trunc(IntPower(NumberSystem, i));
    i += 1;
  end;

  Result := res;
end;

class function TMath.DecToAny(num: integer; NumberSystem: integer): string;
var
  stack: IStack_chr;
  sb: TStringBuilder;
begin
  stack := TStack_chr.Create;

  sb := TStringBuilder.Create();
  try
    while num > 0 do
    begin
      stack.Push(NumToChar(num mod NumberSystem));
      num := num div NumberSystem;
    end;

    while not stack.IsEmpty do
    begin
      sb.Append(stack.Pop);
    end;

    Result := sb.ToString;

  finally
    sb.Free;
  end;
end;

class function TMath.IsPrime(n: uint64): boolean;
var
  i: integer;
begin
  if n <= 1 then Exit(false);
  if n = 2 then Exit(true);

  for i := 2 to Round(sqrt(n)) do
    if n mod i = 0 then Exit(false);

  Result := true;
end;

class function TMath.RoundEx(Value: extended; roundMode: TFPURoundingMode): int64;
var
  rm: TFPURoundingMode;
  res: int64;
begin
  if roundMode = rmNearest then
  begin
    rm := GetRoundMode;
    try
      SetRoundMode(roundMode);
      res := Round(SimpleRoundTo(Value, 0));
    finally
      SetRoundMode(rm);
    end;
  end
  else
  begin
    rm := GetRoundMode;
    try
      SetRoundMode(roundMode);
      res := Round(Value);
    finally
      SetRoundMode(rm);
    end;
  end;

  Result := res;
end;

class function TMath.CharToNum(c: UChar): integer;
var
  res: integer;
begin
  res := -1;

  if c in ['0'..'9'] then
    res := Ord(c) - Ord('0')
  else
    res := Ord(UpCase(c)) - Ord('A') + 10;

  Result := res;
end;

class function TMath.NumToChar(n: integer): char;
var
  res: char;
begin
  if n in [0..9] then
    res := Chr(n + Ord('0'))
  else
    res := Chr(n - 10 + Ord('A'));

  Result := res;
end;
