  {%MainUint DeepStar.Utils.pas}   

class function TMath.AnyToDec(numStr: UString; NumberSystem: integer): integer;
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
    res += __CharToNum(stack.Pop) * Trunc(IntPower(NumberSystem, i));
    i += 1;
  end;

  Result := res;
end;

class function TMath.DecToAny(num: integer; NumberSystem: integer): UString;
var
  stack: IStack_chr;
  sb: TStringBuilder;
begin
  stack := TStack_chr.Create;

  sb := TStringBuilder.Create();
  try
    while num > 0 do
    begin
      stack.Push(__NumToChar(num mod NumberSystem));
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

  for i := 2 to round(sqrt(n)) do
    if n mod i = 0 then Exit(false);

  Result := true;
end;

class function TMath.MatrixMultiply(m1, m2: TArr2D_int): TArr2D_int;
var
  n, m, p, i, j, k: integer;
  ret: TArr2D_int;
begin
  n := length(m1);
  m := length(m1[0]);

  if m <> length(m2) then
    raise Exception.Create('两矩阵不能相乘!');

  p := length(m2[0]);

  SetLength(ret, n, p); // 新矩阵的行数为m1的行数，列数为m2的列数

  for i := 0 to n - 1 do
  begin // m1的每一行
    for j := 0 to p - 1 do
    begin // m2的每一列
      for k := 0 to m - 1 do
      begin
        ret[i, j] := ret[i, j] + m1[i, k] * m2[k, j];
      end;
    end;
  end;

  Result := ret;
end;

class function TMath.MatrixPower(matrix: TArr2D_int; n: integer): TArr2D_int;
var
  ret, tmp: TArr2D_int;
  i: integer;
begin
  // 初始化结果为单位矩阵，对角线为 1
  SetLength(ret, Length(matrix), Length(matrix[0]));

  //单位矩阵，相当于整数的 1
  for i := 0 to Length(ret) - 1 do
    ret[i, i] := 1;

  //平方数
  tmp := matrix; // 一次方
  while n <> 0 do
  begin
    if (n and 1) <> 0 then // 当前二进制位最低位为 1，将当前平方数乘到结果中
      ret := matrixMultiply(ret, tmp);

    //平方数继续上翻
    tmp := matrixMultiply(tmp, tmp);
    n := n shr 1;
  end;

  Result := ret;
end;

class function TMath.Power(a, n: integer): integer;
var
  temp, res, exponent: integer;
begin
  if (n = 0) then
    Exit(1);
  if (n = 1) then
    Exit(a);

  temp := a; // a 的 1 次方
  res := 1;
  exponent := 1;
  while exponent shl 1 < n do
  begin
    temp := temp * temp;
    exponent := exponent shl 1;
  end;

  res := res * Power(a, n - exponent);

  Result := res * temp;
end;

class function TMath.__CharToNum(c: UChar): integer;
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

class function TMath.__NumToChar(n: integer): UChar;
var
  res: UChar;
begin
  if n in [0..9] then
    res := Chr(n + Ord('0'))
  else
    res := Chr(n - 10 + Ord('A'));

  Result := res;
end;