unit DeepStar.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {%H-}Rtti,
  Generics.Defaults,
  Generics.Collections,
  DeepStar.DSA.Interfaces,
  DeepStar.UString,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.DSA.Linear.Stack,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Hash.HashSet;

type
  // 一维数组
  TArr_int = array of integer;
  TArr_int64 = array of int64;
  TArr_chr = array of UChar;
  TArr_str = array of UString;
  TArr_bool = array of boolean;
  TArr_dbl = array of double;

  // 二维数组
  TArr2D_int = array of array of integer;
  TArr2D_int64 = array of array of int64;
  TArr2D_chr = array of array of UChar;
  TArr2D_str = array of array of UString;
  TArr2D_bool = array of array of boolean;
  TArr2D_dbl = array of array of double;

  // 三维数组
  TArr3D_int = array of array of array of integer;

type
  generic TUtils<T> = class
  public
    class procedure Swap(var a, b: T); inline;
  end;

  ///////////////////////////////////
  TUtils_int = specialize TUtils<integer>;
  TUtils_int64 = specialize TUtils<int64>;
  TUtils_str = specialize TUtils<UString>;
  TUtils_chr = specialize TUtils<UChar>;
  TUtils_dbl = specialize TUtils<double>;

type
  // 数组辅助类
  generic TArrayUtils<T> = class
  private type
    TArr_T = array of T;
    TArr2D_T = array of array of T;
    TArr3D_T = array of array of array of T;
    TArrayHelper_T = specialize TArrayHelper<T>;
    ICmp_T = specialize IComparer<T>;

  public type
    TCmp_T = specialize TComparer<T>;
    TOnComparison_T = specialize TOnComparison<T>;
    TComparisonFunc_T = specialize TComparisonFunc<T>;

  public
    // 快速排序
    class procedure Sort(var arr: array of T);
    // 快速排序
    class procedure Sort(var arr: array of T; const cmp: ICmp_T);
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T): integer;
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T; const cmp: ICmp_T): integer;
    // 顺序查找，返回元素e的下标，元素不存在则返回 -1
    class function IndexOf(const arr: array of T; e: T): integer;
    // 输出一维数组
    class procedure Print(arr: TArr_T);
    // 输出二维数组
    class procedure Print2D(arr: TArr2D_T; formated: boolean = true);
    // 输出三维数组
    class procedure Print3D(arr: TArr3D_T);
    // 复制一维数组
    class function CopyArray(arr: TArr_T): TArr_T;
    // 复制二维数组
    class function CopyArray2D(arr2D: TArr2D_T): TArr2D_T;
    // 填充数组
    class procedure FillArray(var arr: TArr_T; e: T);
    // 反转数组
    class procedure Reverse(var arr: TArr_T);
  end;

  TArrayUtils_int = specialize TArrayUtils<integer>;
  TArrayUtils_str = specialize TArrayUtils<UString>;
  TArrayUtils_chr = specialize TArrayUtils<UChar>;
  TArrayUtils_bool = specialize TArrayUtils<boolean>;
  TArrayUtils_dbl = specialize TArrayUtils<double>;

type // 容器类

  //////////////////////////////////
  TList_int = specialize TArrayList<integer>;
  TList_str = specialize TArrayList<UString>;
  TList_chr = specialize TArrayList<UChar>;
  TList_TArr_int = specialize TArrayList<TArr_int>;

  ///////////////////////////////////
  TStack_int = specialize TStack<integer>;
  TStack_chr = specialize TStack<UChar>;
  TStack_str = specialize TStack<UString>;

  //////////////////////////////////
  TQueue_int = specialize TQueue<integer>;
  TQueue_chr = specialize TQueue<UChar>;
  TQueue_str = specialize TQueue<UString>;

  //////////////////////////////////
  TMap_int_int = specialize THashMap<integer, integer>;
  TMap_str_int = specialize THashMap<UString, integer>;

  /////////////////////////////////
  TSet_str = specialize THashSet<UString>;
  TSet_int = specialize THashSet<integer>;

procedure DrawLineBlockEnd;
procedure DrawLineProgramEnd;
procedure NeedInput;
function Chr(i: cardinal): UChar;

resourcestring
  END_OF_PROGRAM_EN = 'Press any key to continue...';
  END_OF_PROGRAM_CH = '按任意键继续...';

implementation

procedure DrawLineBlockEnd;
var
  i: integer;
begin
  for i := 0 to 10 do
  begin
    Write('-');
  end;
  Writeln;
end;

procedure DrawLineProgramEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('=');
  end;
  Writeln;
end;

procedure NeedInput;
begin
  writeln('Need input data: ');
end;

function Chr(i: cardinal): UChar;
begin
  Result := UChar(i);
end;

{ TArrayUtils }

class function TArrayUtils.BinarySearch(const arr: TArr_T; const e: T): integer;
var
  ret: SizeInt;
begin
  Result := -1;

  if TArrayHelper_T.BinarySearch(arr, e, ret) then
    Result := ret;
end;

class function TArrayUtils.BinarySearch(const arr: TArr_T; const e: T;
  const cmp: ICmp_T): integer;
var
  ret: SizeInt;
begin
  Result := -1;

  if TArrayHelper_T.BinarySearch(arr, e, ret, cmp) then
    Result := ret;
end;

class function TArrayUtils.CopyArray(arr: TArr_T): TArr_T;
begin
  Result := Copy(arr);
end;

class function TArrayUtils.CopyArray2D(arr2D: TArr2D_T): TArr2D_T;
var
  i: integer;
  res: TArr2D_T;
begin
  SetLength(res, Length(arr2D));

  for i := 0 to Length(arr2D) - 1 do
    res[i] := Copy(arr2D[i]);

  Result := res;
end;

class procedure TArrayUtils.FillArray(var arr: TArr_T; e: T);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    arr[i] := e;
end;

class function TArrayUtils.IndexOf(const arr: array of T; e: T): integer;
var
  i: integer;
  cmp: ICmp_T;
begin
  Result := -1;
  cmp := TCmp_T.Default;

  for i := 0 to High(arr) do
  begin
    if cmp.Compare(arr[i], e) = 0 then
      Result := i;
  end;
end;

class procedure TArrayUtils.Print(arr: TArr_T);
var
  i: integer;
  Value: TValue;
begin
  if arr = nil then
  begin
    WriteLn('[]');
    Exit;
  end;

  Write('[');
  for i := 0 to High(arr) do
  begin
    TValue.Make(@arr[i], TypeInfo(T), Value);
    Write(Value.ToString);

    if i <> High(arr) then
      Write(', ');
  end;
  Write(']'#10);
end;

class procedure TArrayUtils.Print2D(arr: TArr2D_T; formated: boolean);
var
  i, j: integer;
  Value: TValue;
  tmp: T;
begin
  if arr = nil then
  begin
    WriteLn('[]');
    Exit;
  end;

  case formated of
    true:
    begin
      for i := 0 to High(arr) do
      begin
        Write('[');
        for j := 0 to High(arr[i]) do
        begin
          tmp := arr[i, j];
          TValue.Make(@tmp, System.TypeInfo(T), Value);
          Write(Value.ToString);

          if j <> High(arr[i]) then
            Write(', '#9);
        end;
        Write(']'#10);
      end;
    end;

    false:
    begin
      for i := 0 to High(arr) do
      begin
        Write('[');
        for j := 0 to High(arr[i]) do
        begin
          tmp := arr[i, j];
          TValue.Make(@tmp, TypeInfo(T), Value);
          Write(Value.ToString);

          if j <> High(arr[i]) then
            Write(', ');
        end;
        Write(']'#10);
      end;
    end;
  end;
end;

class procedure TArrayUtils.Print3D(arr: TArr3D_T);
var
  i, j, k: integer;
  Value: TValue;
  tmp: T;
begin
  if arr = nil then
  begin
    WriteLn('[]');
    Exit;
  end;

  for i := 0 to High(arr) do
  begin
    Write('[');
    for j := 0 to High(arr[i]) do
    begin
      Write('(');
      for k := 0 to High(arr[i, j]) do
      begin
        tmp := arr[i, j, k];
        TValue.Make(@tmp, TypeInfo(T), Value);
        Write(Value.ToString);

        if k <> High(arr[i, j]) then
          Write(',');
      end;
      Write(')');

      if j <> High(arr[i]) then
        Write(', ');
    end;
    Write(']'#10);
  end;
end;

class procedure TArrayUtils.Reverse(var arr: TArr_T);
var
  temp: TArr_T;
  j, i: integer;
begin
  SetLength(temp, Length(arr));

  j := 0;
  for i := High(arr) downto 0 do
  begin
    temp[j] := arr[i];
    j += 1;
  end;

  arr := temp;
end;

class procedure TArrayUtils.Sort(var arr: array of T);
begin
  TArrayHelper_T.Sort(arr);
end;

class procedure TArrayUtils.Sort(var arr: array of T; const cmp: ICmp_T);
begin
  TArrayHelper_T.Sort(arr, cmp);
end;

{ TUtils }

class procedure TUtils.Swap(var a, b: T);
var
  temp: T;
begin
  temp := a;
  a := b;
  b := temp;
end;

end.
