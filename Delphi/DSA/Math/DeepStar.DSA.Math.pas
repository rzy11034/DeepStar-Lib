unit DeepStar.DSA.Math;

interface

uses
  System.SysUtils,
  System.Math,
  LQA.Utils;

type
  TMath = class
  private
    class function __charToNum(c: UChar): integer;
    class function __numToChar(n: integer): UChar;
  public
    class function AnyToDec(numStr: UString; inType: integer): integer;
    class function DecToAny(numStr: integer; OutType: integer): UString;
    /// <summary> 以最快速度求a的n次方 ===> O(lgn) </summary>
    class function Power(a, n: integer): integer;
    /// <summary> 求矩阵 matrix 的 n 次方 </summary>
    class function MatrixPower(matrix: TArr2D_int; n: integer): TArr2D_int;
    /// <summary> 矩阵乘法
    /// 矩阵 1 为 n*m 矩阵，矩阵 2 为 m*p 矩阵
    /// 结果为 n*p 矩阵 </summary>
    class function MatrixMultiply(m1, m2: TArr2D_int): TArr2D_int;
  end;

implementation

{ TMath }

class function TMath.AnyToDec(numStr: UString; inType: integer): integer;
var
  tmp, i: integer;
  stack: TStack_chr;
begin
  tmp := 0;

  stack := TStack_chr.Create;
  try
    for i := Low(numStr) to High(numStr) do
      stack.Push(numStr[i]);

    i := 0;
    while stack.Count > 0 do
    begin
      tmp := tmp + __charToNum(stack.Pop) * Trunc(IntPower(inType, i));
      Inc(i);
    end;

    Result := tmp;
  finally
    stack.Free;
  end;
end;

class function TMath.DecToAny(numStr: integer; OutType: integer): UString;
var
  tmp: integer;
  stack: TStack_chr;
  sb: TStringBuilder;
begin
  stack := TStack_chr.Create;
  sb := TStringBuilder.Create;
  try
    tmp := numStr;

    while tmp > 0 do
    begin
      stack.Push(__numToChar(tmp mod OutType));
      tmp := tmp div OutType;
    end;

    while stack.Count > 0 do
      sb.Append(stack.Pop);

    Result := sb.ToString;
  finally
    sb.Free;
    stack.Free;
  end;
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
  SetLength(ret, length(matrix), length(matrix[0]));

  //单位矩阵，相当于整数的 1
  for i := 0 to length(ret) - 1 do
    ret[i, i] := 1;

  //平方数
  tmp := matrix; // 一次方
  while n <> 0 do
  begin
    if (n and 1) <> 0 then // 当前二进制位最低位为 1，将当前平方数乘到结果中
      ret := MatrixMultiply(ret, tmp); //

    //平方数继续上翻
    tmp := MatrixMultiply(tmp, tmp);
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

class function TMath.__charToNum(c: UChar): integer;
var
  ret: integer;
  tmp: char;
begin
  ret := -1;
  tmp := UpCase(c);

  if (tmp >= '0') and (tmp <= '9') then
    ret := Ord(tmp) - Ord('0')
  else if (tmp >= 'A') and (tmp <= 'Z') then
    ret := Ord(tmp) - Ord('A') + 10;

  Result := ret;
end;

class function TMath.__numToChar(n: integer): UChar;
var
  ret: char;
begin
  ret := #0;

  if (n >= 0) and (n <= 9) then
    ret := chr(n + Ord('0'))
  else if (n >= 10) and (n <= 35) then
    ret := chr(n + Ord('A') - 10);

  Result := ret;
end;

end.
