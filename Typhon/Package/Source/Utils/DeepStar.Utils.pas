unit DeepStar.Utils;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}

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
  DeepStar.DSA.Linear.LinkedList,
  DeepStar.DSA.Linear.DoubleLinkedList,
  DeepStar.DSA.Linear.Stack,
  DeepStar.DSA.Linear.Queue,
  DeepStar.DSA.Hash.HashMap,
  DeepStar.DSA.Hash.HashSet,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.DSA.Tree.TreeSet;

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
    TUtils_T = specialize TUtils<T>;

  public type
    ICmp_T = specialize IComparer<T>;
    TCmp_T = specialize TComparer<T>;
    TOnComparison_T = specialize TOnComparison<T>;
    TComparisonFunc_T = specialize TComparisonFunc<T>;

  public
    // 快速排序
    class procedure Sort(var arr: TArr_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; const cmp: ICmp_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; const cmp: TOnComparison_T);
    // 快速排序
    class procedure Sort(var arr: TArr_T; const cmp: TComparisonFunc_T);
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T): integer;
    // 二分查找法
    class function BinarySearch(const arr: TArr_T; const e: T; const cmp: ICmp_T): integer;
    // 顺序查找，返回元素e的下标，元素不存在则返回 -1
    class function IndexOf(const arr: TArr_T; e: T): integer;
    // 输出一维数组
    class procedure Print(arr: TArr_T);
    // 输出二维数组
    class procedure Print2D(arr: TArr2D_T; formated: boolean = true);
    // 输出三维数组
    class procedure Print3D(arr: TArr3D_T);
    // 复制一维数组
    class function CopyArray(const arr: TArr_T): TArr_T;
    // 复制二维数组
    class function CopyArray2D(const arr2D: TArr2D_T): TArr2D_T;
    // 填充数组
    class procedure FillArray(var arr: TArr_T; e: T);
    // 反转数组
    class procedure Reverse(var arr: TArr_T);
    // 初始化一维数组并填充默认初始值
    class procedure SetLengthAndFill(var arr: TArr_T; n: integer);
    // 初始化一维数组并填充初始值
    class procedure SetLengthAndFill(var arr: TArr_T; n: integer; f: T);
    // 初始化一维数组并填充默认初始值
    class procedure SetLengthAndFill(var arr: TArr2D_T; n, m: integer);
    // 初始化二维数组并填充初始值
    class procedure SetLengthAndFill(var arr: TArr2D_T; n, m: integer; f: T);
  end;

  TArrayUtils_int = specialize TArrayUtils<integer>;
  TArrayUtils_str = specialize TArrayUtils<UString>;
  TArrayUtils_chr = specialize TArrayUtils<UChar>;
  TArrayUtils_bool = specialize TArrayUtils<boolean>;
  TArrayUtils_dbl = specialize TArrayUtils<double>;

type // 容器类

  //////////////////////////////////
  {$REGION 'List'}
  IList_int = specialize IList<integer>;
  TArrayList_int = specialize TArrayList<integer>;
  TLinkedList_int = specialize TLinkedList<integer>;
  TDoubleLinkedList_int = specialize TDoubleLinkedList<integer>;

  IList_str = specialize IList<UString>;
  TArrayList_str = specialize TArrayList<UString>;
  TLinkedList_str = specialize TLinkedList<UString>;
  TDoubleLinkedList_str = specialize TDoubleLinkedList<UString>;

  IList_chr = specialize IList<UChar>;
  TArrayList_chr = specialize TArrayList<UChar>;
  TLinkedList_chr = specialize TLinkedList<UChar>;
  TDoubleLinkedList_chr = specialize TDoubleLinkedList<UChar>;

  IList_dbl = specialize IList<double>;
  TArrayList_dbl = specialize TArrayList<double>;
  TLinkedList_dbl = specialize TLinkedList<double>;
  TDoubleLinkedList_dbl = specialize TDoubleLinkedList<double>;

  IList_TArr_int = specialize IList<TArr_int>;
  TArrayList_TArr_int = specialize TArrayList<TArr_int>;
  {$ENDREGION}

  ///////////////////////////////////
  {$REGION 'Stack'}
  IStack_int = specialize IStack<integer>;
  TStack_int = specialize TStack<integer>;

  IStack_chr = specialize IStack<UChar>;
  TStack_chr = specialize TStack<UChar>;

  IStack_str = specialize IStack<UString>;
  TStack_str = specialize TStack<UString>;
  {$ENDREGION}

  //////////////////////////////////
  {$REGION 'Queue'}
  IQueue_int = specialize IQueue<integer>;
  TQueue_int = specialize TQueue<integer>;

  IQueue_chr = specialize IQueue<UChar>;
  TQueue_chr = specialize TQueue<UChar>;

  IQueue_str = specialize IQueue<UString>;
  TQueue_str = specialize TQueue<UString>;
  {$ENDREGION}

  //////////////////////////////////
  {$REGION 'Map'}
  IMap_int_int = specialize IMap<integer, integer>;
  THashMap_int_int = specialize THashMap<integer, integer>;
  TTreeMap_int_int = specialize TTreeMap<integer, integer>;

  IMap_str_int = specialize IMap<UString, integer>;
  THashMap_str_int = specialize THashMap<UString, integer>;
  TTreeMap_str_int = specialize TTreeMap<UString, integer>;

  IMap_chr_int = specialize IMap<UString, integer>;
  THashMap_chr_int = specialize THashMap<UString, integer>;
  TTreeMap_chr_int = specialize TTreeMap<UString, integer>;

  IMap_int_str = specialize IMap<integer, UString>;
  THashMap_int_str = specialize THashMap<integer, UString>;
  TTreeMap_int_str = specialize TTreeMap<integer, UString>;
  {$ENDREGION}

  /////////////////////////////////
  {$REGION 'Set'}
  ISet_str = specialize ISet<UString>;
  THashSet_str = specialize THashSet<UString>;
  TTreeSet_str = specialize TTreeSet<UString>;

  ISet_int = specialize ISet<integer>;
  THashSet_int = specialize THashSet<integer>;
  TTreeSet_int = specialize TTreeSet<integer>;

  ISet_chr = specialize ISet<UChar>;
  THashSet_chr = specialize THashSet<UChar>;
  TTreeSet_chr = specialize TTreeSet<UChar>;

  ISet_dbl = specialize ISet<double>;
  THashSet_dbl = specialize THashSet<double>;
  TTreeSet_dbl = specialize TTreeSet<double>;
  {$ENDREGION}

resourcestring
  END_OF_PROGRAM_EN = 'Press <ENTER> key to continue...';
  END_OF_PROGRAM_CH = '按 <ENTER> 键继续...';

procedure DrawLineBlockEnd;

procedure DrawLineProgramEnd;
procedure NeedInput;
function Exp(d: ValReal): ValReal;
function Chr(i: cardinal): UChar;
procedure WriteF(const Fmt: string; const Args: array of const);
procedure WriteLnF(const Fmt: string; const Args: array of const);
function IfThen(Condition: boolean; TrueResult, FalseResult: variant): variant;
  deprecated 'Use IfThen<T> instead';

generic function IfThen<T>(Condition: boolean; TrueResult, FalseResult: T): T;

implementation

procedure DrawLineBlockEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('─');
  end;
  Writeln;
end;

procedure DrawLineProgramEnd;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('═');
  end;
  Writeln;
end;

procedure NeedInput;
begin
  writeln('Need input data: ');
end;

function Exp(d: ValReal): ValReal;
begin
  Result := system.Exp(d);
end;

function Chr(i: cardinal): UChar;
begin
  Result := UChar(i);
end;

function IfThen(Condition: boolean; TrueResult, FalseResult: variant): variant;
begin
  Result := Default(variant);

  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

generic function IfThen<T>(Condition: boolean; TrueResult, FalseResult: T): T;
begin
  Result := Default(T);

  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

procedure WriteF(const Fmt: string; const Args: array of const);
begin
  Write(Format(Fmt, Args));
end;

procedure WriteLnF(const Fmt: string; const Args: array of const);
begin
  WriteLn(Format(Fmt, Args));
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

class function TArrayUtils.CopyArray(const arr: TArr_T): TArr_T;
begin
  Result := system.Copy(arr);
end;

class function TArrayUtils.CopyArray2D(const arr2D: TArr2D_T): TArr2D_T;
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

class function TArrayUtils.IndexOf(const arr: TArr_T; e: T): integer;
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
    if Value.Kind <> tkFloat then
      Write(Value.ToString)
    else
      Write(Value.AsExtended.ToString);

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
    WriteLn('[[]]');
    Exit;
  end;

  Write('[');
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
          if Value.Kind <> tkFloat then
            Write(Value.ToString)
          else
            Write(Value.AsExtended.ToString);

          if j <> High(arr[i]) then
            Write(', '#9);
        end;

        if i <> High(arr) then
          Write('], ')
        else
          Write(']');
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
          if Value.Kind <> tkFloat then
            Write(Value.ToString)
          else
            Write(Value.AsExtended.ToString);

          if j <> High(arr[i]) then
            Write(', ');
        end;

        if i <> High(arr) then
          Write('], ')
        else
          Write(']');
      end;
    end;
  end;
  Write(']'#10);
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
        if Value.Kind <> tkFloat then
          Write(Value.ToString)
        else
          Write(Value.AsExtended.ToString);

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
  l, r: integer;
begin
  l := 0;
  r := High(arr);

  while l < r do
  begin
    TUtils_T.Swap(arr[l], arr[r]);

    l += 1;
    r -= 1;
  end;
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr2D_T; n, m: integer);
var
  i: integer;
begin
  SetLength(arr, n, m);
  for i := 0 to High(arr) do
    FillArray(arr[i], Default(T));
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr2D_T; n, m: integer; f: T);
var
  i: integer;
begin
  SetLength(arr, n, m);
  for i := 0 to High(arr) do
    FillArray(arr[i], f);
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr_T; n: integer);
begin
  SetLength(arr, n);
  FillArray(arr, Default(T));
end;

class procedure TArrayUtils.SetLengthAndFill(var arr: TArr_T; n: integer; f: T);
begin
  SetLength(arr, n);
  FillArray(arr, f);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T);
begin
  TArrayHelper_T.Sort(arr);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; const cmp: ICmp_T);
begin
  TArrayHelper_T.Sort(arr, cmp);
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; const cmp: TComparisonFunc_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp));
end;

class procedure TArrayUtils.Sort(var arr: TArr_T; const cmp: TOnComparison_T);
begin
  TArrayHelper_T.Sort(arr, TCmp_T.Construct(cmp));
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
